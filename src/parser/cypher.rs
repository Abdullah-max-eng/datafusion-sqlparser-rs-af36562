//! Minimal Cypher → SQL desugaring that ALSO uses the repo's AST by
//! parsing the generated SQL into `Vec<Statement>` and re-rendering from AST.
//!
//! Supported forms:
//!   MATCH (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!     ⇒ SELECT * FROM label WHERE k1 = 'v1' AND k2 = 'v2'
//!
//!   CREATE (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!     ⇒ INSERT INTO label (k1, k2) VALUES ('v1', 'v2');
//!        SELECT * FROM label WHERE k1 = 'v1' AND k2 = 'v2' LIMIT 1;

use thiserror::Error;

// These are the repo types (AST, dialect, parser) — this proves we use the crate.
use crate::ast::Statement;
use crate::dialect::MySqlDialect;
use crate::parser::Parser;

#[derive(Error, Debug)]
pub enum CypherError {
    #[error("unsupported or invalid cypher: {0}")]
    Unsupported(String),
    #[error("parse error: {0}")]
    Parse(String),
}
pub type CypherResult<T> = Result<T, CypherError>;

/// Entry 1 (simple): return SQL text built by our Cypher→SQL mapper (strings).
/// This is your "desugaring" step, independent of the AST.
/// Stage A: Cypher → provisional SQL string.
pub fn cypher_to_sql_text(cypher: &str) -> CypherResult<String> {
    let s = cypher.trim();
    let su = s.to_uppercase();
    if su.starts_with("MATCH ") {
        return match_to_select(s);
    }
    if su.starts_with("CREATE ") {
        return create_to_insert_select(s);
    }
    Err(CypherError::Unsupported(s.to_string()))
}

/// Entry 2 (AST): build AST by parsing the provisional SQL with the repo parser.
/// Stage B: provisional SQL string → AST (Vec<Statement>).
pub fn cypher_to_sql_ast(cypher: &str) -> CypherResult<Vec<Statement>> {
    let sql = cypher_to_sql_text(cypher)?; // reuse our mapping (Stage A)
    let dialect = MySqlDialect {};
    Parser::parse_sql(&dialect, &sql).map_err(|e| CypherError::Parse(e.to_string()))
}

/// Stage C: AST → final SQL text using the repo's pretty-printer (Display impl).
/// We output one statement per line, ending with ';'.
pub fn render_statements(stmts: &[Statement]) -> String {
    let mut out = String::new();
    for (i, s) in stmts.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        out.push_str(&s.to_string()); // repo converts AST → SQL
        out.push(';');
    }
    out
}

















/* ---------- Desugaring helpers (Stage A: Cypher → provisional SQL) ---------- */

fn match_to_select(s: &str) -> CypherResult<String> {
    // Only support: MATCH (n:Label {props}) RETURN n
    let label = parse_label(s)?;
    let where_sql = if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        format!(" WHERE {}", conjunct_eq(&cols, &vals))
    } else {
        String::new()
    };
    Ok(format!("SELECT * FROM {}{}", label.to_lowercase(), where_sql))
}


fn create_to_insert_select(s: &str) -> CypherResult<String> {
    // Only support: CREATE (n:Label {props}) RETURN n
    let label = parse_label(s)?;
    let props = need_between(s, "{", "}", "missing {..} properties")?;
    let (cols, vals) = parse_kv_props(&props)?;

    let cols_sql = cols
        .iter()
        .map(|c| ident_sql(c))
        .collect::<Vec<_>>()
        .join(", ");
    let vals_sql = vals
        .iter()
        .map(|v| format!("'{}'", v.replace('\'', "''")))
        .collect::<Vec<_>>()
        .join(", ");

    let insert = format!(
        "INSERT INTO {} ({}) VALUES ({})",
        label.to_lowercase(),
        cols_sql,
        vals_sql
    );

    let where_sql = conjunct_eq(&cols, &vals);
    let select = format!(
        "SELECT * FROM {} WHERE {} LIMIT 1",
        label.to_lowercase(),
        where_sql
    );

    Ok(format!("{insert};\n{select}"))
}








/* ---------- Tiny string parsing utils for Stage A ---------- */

/// Extract the label after ':' up to first delimiter: space, '{', or ')'.
fn parse_label(s: &str) -> CypherResult<String> {
    let start = s
        .find(':')
        .ok_or_else(|| CypherError::Parse("missing :Label".into()))?
        + 1;

    let mut label = String::new();
    for ch in s[start..].chars() {
        match ch {
            c if c.is_ascii_alphanumeric() || c == '_' => label.push(c),
            ' ' | '{' | ')' => break,
            _ => break,
        }
    }

    if label.is_empty() {
        return Err(CypherError::Parse("empty label after ':'".into()));
    }
    Ok(label)
}


fn ident_sql(c: &str) -> String {
    if c.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '_') {
        c.to_string()
    } else {
        format!("`{}`", c.replace('`', "``"))
    }
}

fn conjunct_eq(cols: &[String], vals: &[String]) -> String {
    cols.iter()
        .zip(vals)
        .map(|(c, v)| format!("{} = '{}'", ident_sql(c), v.replace('\'', "''")))
        .collect::<Vec<_>>()
        .join(" AND ")
}

/// Parse a simple `{k1:'v1', k2:'v2'}` property list.
fn parse_kv_props(s: &str) -> CypherResult<(Vec<String>, Vec<String>)> {
    let mut cols = Vec::new();
    let mut vals = Vec::new();
    for pair in s.split(',') {
        let p = pair.trim();
        if p.is_empty() {
            continue;
        }
        let mut it = p.splitn(2, ':');
        let k = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad key".into()))?
            .trim();
        let v = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad value".into()))?
            .trim();
        let v = v.trim_matches('\'');
        if k.is_empty() {
            return Err(CypherError::Parse("empty key".into()));
        }
        cols.push(k.to_string());
        vals.push(v.to_string());
    }
    if cols.is_empty() {
        return Err(CypherError::Parse("no properties".into()));
    }
    Ok((cols, vals))
}

fn extract_between<'a>(s: &'a str, left: &str, right: &str) -> Option<&'a str> {
    let start = s.find(left)? + left.len();
    let end = s[start..].find(right)? + start;
    Some(&s[start..end])
}
fn need_between(s: &str, left: &str, right: &str, msg: &str) -> CypherResult<String> {
    extract_between(s, left, right)
        .map(|x| x.to_string())
        .ok_or_else(|| CypherError::Parse(msg.into()))
}

/* ---------- Optional unit tests (prove AST is involved) ---------- */
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn match_roundtrip_ast() {
        let cy = "MATCH (n:Person {name:'Alice', city:'ATX'}) RETURN n";
        let ast = cypher_to_sql_ast(cy).unwrap(); // Stage B: AST created
        let sql = render_statements(&ast);        // Stage C: AST → SQL
        let got = sql.to_lowercase();
        assert!(got.contains("select * from person"));
        assert!(got.contains("name = 'alice'") || got.contains("name = 'Alice'"));
        assert!(got.contains("city = 'atx'") || got.contains("city = 'ATX'"));
    }

    #[test]
    fn create_roundtrip_ast() {
        let cy = "CREATE (n:Person {name:'Alice'}) RETURN n";
        let ast = cypher_to_sql_ast(cy).unwrap();
        let sql = render_statements(&ast).to_lowercase();
        assert!(sql.contains("insert into person"));
        assert!(sql.contains("select * from person"));
    }
}
