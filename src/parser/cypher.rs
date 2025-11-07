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

    // WHERE: label + optional {props}
    let mut preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];

    if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        preds.extend(prop_predicates(&cols, &vals));
    }
    let where_sql = if preds.is_empty() { String::new() } else { format!(" WHERE {}", preds.join(" AND ")) };

    // RETURN clause
    let up = s.to_uppercase();
    let ret_pos = up.find(" RETURN ").ok_or_else(|| CypherError::Unsupported("missing RETURN".into()))?;
    let ret_txt = s[ret_pos + " RETURN ".len()..].trim();

    let projection = if ret_txt.eq_ignore_ascii_case("n") {
        "*".to_string()
    } else {
        // Accept: n.prop [AS alias], prop, ...
        let mut pieces = Vec::new();
        for part in ret_txt.split(',') {
            let p = part.trim();
            if p.is_empty() { continue; }
            let core = p.strip_prefix("n.").unwrap_or(p).trim();
            let (prop, alias) = split_as(core);

            let expr = if prop.eq_ignore_ascii_case("name") {
                "name".to_string()
            } else {
                json_get_expr(prop) // JSON_UNQUOTE(JSON_EXTRACT(props,'$."k"'))
            };
            let piece = match alias {
                Some(a) => format!("{} AS {}", expr, ident_sql(a)),
                None => expr,
            };
            pieces.push(piece);
        }
        if pieces.is_empty() { "*".to_string() } else { pieces.join(", ") }
    };

    Ok(format!("SELECT {} FROM {}{}", projection, ident_sql("nodes"), where_sql))
}

/// CREATE ... RETURN n  → INSERT; SELECT ... LIMIT 1
fn create_to_insert_select(s: &str) -> CypherResult<String> {
    let label = parse_label(s)?;
    let props = need_between(s, "{", "}", "missing {..} properties")?;
    let (cols, vals) = parse_kv_props(&props)?;

    // Split into (name, others→JSON)
    let (name_opt, other_cols, other_vals) = split_name_and_others(&cols, &vals);
    let name_sql = name_opt.map_or("NULL".to_string(), |v| sql_str(v));
    let props_sql = if other_cols.is_empty() {
        "NULL".to_string()
    } else {
        json_object(&other_cols, &other_vals)
    };

    let insert = format!(
        "INSERT INTO {} (label, name, props) VALUES ({}, {}, {})",
        ident_sql("nodes"), sql_str(&label), name_sql, props_sql
    );

    // Return the created row (simple filter by label + name if provided, else label only)
    let mut preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];
    if let Some(v) = name_opt { preds.push(format!("name = {}", sql_str(v))); }
    let select = format!(
        "SELECT * FROM {} WHERE {} LIMIT 1",
        ident_sql("nodes"), preds.join(" AND ")
    );

    Ok(format!("{insert};\n{select}"))
}

/// MATCH ... SET ... RETURN n  → UPDATE; SELECT ...
fn match_set_to_update_select(s: &str) -> CypherResult<String> {
    let up = s.to_uppercase();
    let label = parse_label(s)?;

    // WHERE from label + optional {props}
    let mut preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];
    if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        preds.extend(prop_predicates(&cols, &vals));
    }

    let where_sql = if preds.is_empty() { String::new() } else { format!(" WHERE {}", preds.join(" AND ")) };

    // SET list
    let set_start = up.find(" SET ").ok_or_else(|| CypherError::Unsupported("missing SET".into()))? + " SET ".len();
    let set_end = up.find(" RETURN ").unwrap_or(s.len());
    let set_txt = s[set_start..set_end].trim();
    let set_pairs = parse_set_list(set_txt)?;
    if set_pairs.is_empty() { return Err(CypherError::Parse("empty SET list".into())); }

    // Build SET clause: name → column, others → JSON_SET
    let mut set_sqls: Vec<String> = Vec::new();
    let mut json_sets: Vec<(String, String)> = Vec::new();

    for (k, v) in set_pairs {
        if k.eq_ignore_ascii_case("name") {
            set_sqls.push(format!("name = {}", sql_str(&v)));
        } else {
            json_sets.push((k, v));
        }
    }

    if !json_sets.is_empty() {
        // props = JSON_SET(IFNULL(props, JSON_OBJECT()), '$."k1"', 'v1', '$."k2"', 'v2', ...)
        let mut args: Vec<String> = vec!["IFNULL(props, JSON_OBJECT())".to_string()];
        for (k, v) in &json_sets {
            args.push(sql_str(&format!("$.\"{}\"", k)));
            args.push(sql_str(v));
        }
        set_sqls.push(format!("props = JSON_SET({})", args.join(", ")));
    }

    let update = format!("UPDATE {} SET {}{}", ident_sql("nodes"), set_sqls.join(", "), where_sql);

    // Optional follow-up SELECT if RETURN is present
    let ret_select = if up.contains(" RETURN ") {
        Some(format!("SELECT * FROM {}{}", ident_sql("nodes"), where_sql))
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

    let mut preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];
    if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        preds.extend(prop_predicates(&cols, &vals));
    }
    let where_sql = if preds.is_empty() { String::new() } else { format!(" WHERE {}", preds.join(" AND ")) };

    let del = format!("DELETE FROM {}{}", ident_sql("nodes"), where_sql);

    if up.contains(" RETURN ") && up.contains("COUNT(") {
        Ok(format!("{del};\nSELECT ROW_COUNT() AS {}", ident_sql("count")))
    } else {
        Ok(del)
    }
}

/* ===================== JSON helpers for nodes.props ===================== */

/// WHERE predicates from k/v lists using `name` column or JSON props.
fn prop_predicates(cols: &[String], vals: &[String]) -> Vec<String> {
    cols.iter().zip(vals).map(|(k,v)| {
        if k.eq_ignore_ascii_case("name") {
            format!("name = {}", sql_str(v))
        } else {
            format!("JSON_UNQUOTE(JSON_EXTRACT(props, {})) = {}",
                sql_str(&format!("$.\"{}\"", k)), sql_str(v))
        }
    }).collect()
}

/// SELECT expression to read a JSON property as text
fn json_get_expr(prop: &str) -> String {
    format!("JSON_UNQUOTE(JSON_EXTRACT(props, {}))",
            sql_str(&format!("$.\"{}\"", prop)))
}

/// Build JSON_OBJECT('k1','v1','k2','v2',...) from k/v lists
fn json_object(cols: &[String], vals: &[String]) -> String {
    let mut parts: Vec<String> = Vec::new();
    for (k,v) in cols.iter().zip(vals) {
        parts.push(sql_str(k));
        parts.push(sql_str(v));
    }
    format!("JSON_OBJECT({})", parts.join(", "))
}

/// Split properties into optional name and (others)
fn split_name_and_others<'a>(cols: &'a [String], vals: &'a [String]) -> (Option<&'a str>, Vec<String>, Vec<String>) {
    let mut name: Option<&str> = None;
    let mut other_c = Vec::new();
    let mut other_v = Vec::new();
    for (k, v) in cols.iter().zip(vals) {
        if k.eq_ignore_ascii_case("name") {
            name = Some(v.as_str());
        } else {
            other_c.push(k.clone());
            other_v.push(v.clone());
        }
    }
    (name, other_c, other_v)
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
