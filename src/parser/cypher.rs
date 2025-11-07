//! Minimal Cypher → SQL desugaring using the repo’s AST.
//!
//! Supported Cypher forms (lower/upper insensitive):
//!
//! 1) MATCH (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!    MATCH (n:Label {k1:'v1'}) RETURN n.k1, n.k2 AS alias
//!      → SELECT [*|cols] FROM label WHERE k1='v1' [AND ...]
//!
//! 2) CREATE (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!      → INSERT INTO label (k1, k2) VALUES ('v1','v2');
//!        SELECT * FROM label WHERE k1='v1' AND k2='v2' LIMIT 1
//!
//! 3) MATCH (n:Label {id:'123'}) SET n.k1='x', n.k2='y' RETURN n
//!      → UPDATE label SET k1='x', k2='y' WHERE id='123';
//!        SELECT * FROM label WHERE id='123'
//!
//! 4) MATCH (n:Label {k1:'v1'}) DELETE n
//!      → DELETE FROM label WHERE k1='v1'
//!
//!    MATCH (n:Label {k1:'v1'}) DELETE n RETURN count(n)
//!      → DELETE FROM label WHERE k1='v1';
//!        SELECT ROW_COUNT() AS `count`

use thiserror::Error;

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

/// Stage A: Cypher text → provisional SQL string(s).
pub fn cypher_to_sql_text(cypher: &str) -> CypherResult<String> {
    let s = cypher.trim();
    let su = s.to_uppercase();

    if su.starts_with("MATCH ") {
        // Branch by presence of SET / DELETE / RETURN
        if su.contains(" SET ") {
            return match_set_to_update_select(s);
        }
        if su.contains(" DELETE ") {
            return match_delete_to_delete_select(s);
        }
        // Plain MATCH ... RETURN ...
        return match_to_select(s);
    }
    if su.starts_with("CREATE ") {
        return create_to_insert_select(s);
    }
    Err(CypherError::Unsupported(s.to_string()))
}






pub fn cypher_to_sql_ast(cypher: &str) -> CypherResult<Vec<Statement>> {
    let sql = cypher_to_sql_text(cypher)?;
    let dialect = MySqlDialect {};
    let mut out: Vec<Statement> = Vec::new();

    // Parse each statement separately so multi-stmt strings work reliably
    for part in sql.split(';') {
        let stmt = part.trim();
        if stmt.is_empty() { continue; }
        let mut v = Parser::parse_sql(&dialect, stmt)
            .map_err(|e| CypherError::Parse(format!("while parsing `{stmt}`: {e}")))?;
        out.append(&mut v);
    }
    Ok(out)
}


/// Stage C: Repo pretty-printer (AST → SQL).
pub fn render_statements(stmts: &[Statement]) -> String {
    let mut out = String::new();
    for (i, s) in stmts.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        out.push_str(&s.to_string());
        out.push(';');
    }
    out
}

/* ===================== Stage A helpers (desugaring) ===================== */

/// MATCH ... RETURN ...
fn match_to_select(s: &str) -> CypherResult<String> {
    let label = parse_label(s)?;
    let where_sql = if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        format!(" WHERE {}", conjunct_eq(&cols, &vals))
    } else {
        String::new()
    };

    // RETURN clause
    let up = s.to_uppercase();
    let ret_pos = up
        .find(" RETURN ")
        .ok_or_else(|| CypherError::Unsupported("missing RETURN".into()))?;
    let ret_txt = s[ret_pos + " RETURN ".len()..].trim();

    let projection = if ret_txt.eq_ignore_ascii_case("n") {
        "*".to_string()
    } else {
        // Accept: n.prop [AS alias], n.k2, prop, ...
        let mut pieces = Vec::new();
        for part in ret_txt.split(',') {
            let p = part.trim();
            if p.is_empty() { continue; }
            let core = p.strip_prefix("n.").unwrap_or(p).trim();
            let (prop, alias) = split_as(core);
            if prop.is_empty() {
                return Err(CypherError::Parse("empty property in RETURN".into()));
            }
            let mut expr = ident_sql(prop);
            if let Some(a) = alias {
                expr.push_str(" AS ");
                expr.push_str(&ident_sql(a));
            }
            pieces.push(expr);
        }
        if pieces.is_empty() { "*".to_string() } else { pieces.join(", ") }
    };

    Ok(format!(
        "SELECT {} FROM {}{}",
        projection,
        ident_sql(&label.to_lowercase()),
        where_sql
    ))
}

/// CREATE ... RETURN n  → INSERT; SELECT ... LIMIT 1
fn create_to_insert_select(s: &str) -> CypherResult<String> {
    let label = parse_label(s)?;
    let props = need_between(s, "{", "}", "missing {..} properties")?;
    let (cols, vals) = parse_kv_props(&props)?;

    let cols_sql = cols.iter().map(|c| ident_sql(c)).collect::<Vec<_>>().join(", ");
    let vals_sql = vals.iter().map(|v| sql_str(v)).collect::<Vec<_>>().join(", ");

    let insert = format!(
        "INSERT INTO {} ({}) VALUES ({})",
        ident_sql(&label.to_lowercase()),
        cols_sql,
        vals_sql
    );
    let where_sql = conjunct_eq(&cols, &vals);
    let select = format!(
        "SELECT * FROM {} WHERE {} LIMIT 1",
        ident_sql(&label.to_lowercase()),
        where_sql
    );

    Ok(format!("{insert};\n{select}"))
}

/// MATCH ... SET ... RETURN n  → UPDATE; SELECT ...
fn match_set_to_update_select(s: &str) -> CypherResult<String> {
    let up = s.to_uppercase();

    // label + WHERE from {props}
    let label = parse_label(s)?;
    let where_sql = if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        Some((cols, vals))
    } else {
        None
    };

    // Extract SET clause region between " SET " and optional " RETURN "
    let set_start = up
        .find(" SET ")
        .ok_or_else(|| CypherError::Unsupported("missing SET".into()))? + " SET ".len();
    let set_end = up.find(" RETURN ").unwrap_or(s.len());
    let set_txt = s[set_start..set_end].trim();
    let set_pairs = parse_set_list(set_txt)?; // Vec<(col,val)>

    if set_pairs.is_empty() {
        return Err(CypherError::Parse("empty SET list".into()));
    }

    let set_sql = set_pairs
        .iter()
        .map(|(k,v)| format!("{} = {}", ident_sql(k), sql_str(v)))
        .collect::<Vec<_>>()
        .join(", ");

    let update = if let Some((cols, vals)) = &where_sql {
        let where_expr = conjunct_eq(cols, vals);
        format!(
            "UPDATE {} SET {} WHERE {}",
            ident_sql(&label.to_lowercase()), set_sql, where_expr
        )
    } else {
        // No {props} → UPDATE all (allowed here for simplicity)
        format!("UPDATE {} SET {}", ident_sql(&label.to_lowercase()), set_sql)
    };

    // Optional: if RETURN n present, follow with a SELECT of matched rows
    let ret_select = if up.contains(" RETURN ") {
        if let Some((cols, vals)) = where_sql {
            Some(format!(
                "SELECT * FROM {} WHERE {}",
                ident_sql(&label.to_lowercase()),
                conjunct_eq(&cols, &vals)
            ))
        } else {
            // No filter → return all after update (basic demo)
            Some(format!("SELECT * FROM {}", ident_sql(&label.to_lowercase())))
        }
    } else {
        None
    };

    Ok(match ret_select {
        Some(sel) => format!("{update};\n{sel}"),
        None => update,
    })
}

/// MATCH ... DELETE n [RETURN count(n)] → DELETE [; SELECT ROW_COUNT()]
fn match_delete_to_delete_select(s: &str) -> CypherResult<String> {
    let up = s.to_uppercase();
    let label = parse_label(s)?;

    let where_sql = if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        format!(" WHERE {}", conjunct_eq(&cols, &vals))
    } else {
        String::new()
    };

    let del = format!(
        "DELETE FROM {}{}",
        ident_sql(&label.to_lowercase()),
        where_sql
    );

    // If user wrote RETURN count(n), tack on a ROW_COUNT() select
    if up.contains(" RETURN ") && up.contains("COUNT(") {
        Ok(format!("{del};\nSELECT ROW_COUNT() AS {}", ident_sql("count")))
    } else {
        Ok(del)
    }
}

/* ===================== Tiny string parsing utils ===================== */

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

fn sql_str(v: &str) -> String {
    format!("'{}'", v.replace('\'', "''"))
}

fn conjunct_eq(cols: &[String], vals: &[String]) -> String {
    cols.iter()
        .zip(vals)
        .map(|(c, v)| format!("{} = {}", ident_sql(c), sql_str(v)))
        .collect::<Vec<_>>()
        .join(" AND ")
}

/// Parse a simple `{k1:'v1', k2:'v2'}` property list.
fn parse_kv_props(s: &str) -> CypherResult<(Vec<String>, Vec<String>)> {
    let mut cols = Vec::new();
    let mut vals = Vec::new();
    for pair in s.split(',') {
        let p = pair.trim();
        if p.is_empty() { continue; }
        let mut it = p.splitn(2, ':');
        let k = it.next().ok_or_else(|| CypherError::Parse("bad key".into()))?.trim();
        let v = it.next().ok_or_else(|| CypherError::Parse("bad value".into()))?.trim();
        let v = v.trim_matches('\'');
        if k.is_empty() { return Err(CypherError::Parse("empty key".into())); }
        cols.push(k.to_string());
        vals.push(v.to_string());
    }
    if cols.is_empty() {
        return Err(CypherError::Parse("no properties".into()));
    }
    Ok((cols, vals))
}

/// Parse SET list like: "n.k1='x', n.k2='y'"
fn parse_set_list(s: &str) -> CypherResult<Vec<(String,String)>> {
    let mut out = Vec::new();
    for part in s.split(',') {
        let p = part.trim();
        if p.is_empty() { continue; }
        let p = p.strip_prefix("n.").unwrap_or(p).trim();
        let mut it = p.splitn(2, '=');
        let k = it.next().ok_or_else(|| CypherError::Parse("bad SET key".into()))?.trim();
        let v = it.next().ok_or_else(|| CypherError::Parse("bad SET value".into()))?.trim();
        if k.is_empty() { return Err(CypherError::Parse("empty SET key".into())); }
        let v = v.trim_matches('\''); // accept only quoted strings for simplicity
        out.push((k.to_string(), v.to_string()));
    }
    Ok(out)
}

fn split_as(s: &str) -> (&str, Option<&str>) {
    if let Some(idx) = s.to_uppercase().find(" AS ") {
        let prop = s[..idx].trim();
        let alias = s[idx + 4..].trim();
        (prop, Some(alias))
    } else {
        (s.trim(), None)
    }
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

/* ===================== Unit tests (prove AST is used) ===================== */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_match_return_n() {
        let cy = "MATCH (n:Person {name:'Alice', city:'ATX'}) RETURN n";
        let ast = cypher_to_sql_ast(cy).unwrap();
        let sql = render_statements(&ast).to_lowercase();
        assert!(sql.contains("select * from person"));
        assert!(sql.contains("name = 'alice'") || sql.contains("name = 'alice'"));
        assert!(sql.contains("city = 'atx'"));
    }

    #[test]
    fn t_match_projection_alias() {
        let cy = "MATCH (n:Person) RETURN n.name AS name, n.city";
        let txt = cypher_to_sql_text(cy).unwrap().to_lowercase();
        assert!(txt.contains("select name as name, city from person"));
    }

    #[test]
    fn t_create_insert_then_select() {
        let cy = "CREATE (n:Person {name:'Alice', city:'ATX'}) RETURN n";
        let ast = cypher_to_sql_ast(cy).unwrap();
        let sql = render_statements(&ast).to_lowercase();
        assert!(sql.contains("insert into person"));
        assert!(sql.contains("select * from person"));
    }

    #[test]
    fn t_match_set_update_then_select() {
        let cy = "MATCH (n:Person {id:'123'}) SET n.city='SEA', n.name='Bob' RETURN n";
        let txt = cypher_to_sql_text(cy).unwrap().to_lowercase();
        assert!(txt.contains("update person set city = 'sea', name = 'bob' where id = '123'")
            || txt.contains("update person set name = 'bob', city = 'sea' where id = '123'"));
        assert!(txt.contains("select * from person where id = '123'"));
    }

    #[test]
    fn t_match_delete_plain() {
        let cy = "MATCH (n:Person {id:'1'}) DELETE n";
        let sql = cypher_to_sql_text(cy).unwrap().to_lowercase();
        assert!(sql.contains("delete from person where id = '1'"));
    }

    #[test]
    fn t_match_delete_return_count() {
        let cy = "MATCH (n:Person {city:'ATX'}) DELETE n RETURN count(n)";
        let sql = cypher_to_sql_text(cy).unwrap().to_lowercase();
        assert!(sql.contains("delete from person where city = 'atx'"));
        assert!(sql.contains("select row_count() as `count`"));
    }
}
