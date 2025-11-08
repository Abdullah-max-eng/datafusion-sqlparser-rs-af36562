//! Minimal Cypher → SQL desugaring for a nodes(+edges) graph stored in MySQL.
//!
//! Table layout this targets:
//!   nodes(id BIGINT PK AUTO_INCREMENT, label VARCHAR(64), name VARCHAR(255), props JSON)
//!
//! Supported Cypher (case-insensitive):
//!   1) MATCH (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!      MATCH (n:Label) RETURN n.name, n.k2 AS alias
//!         → SELECT [* | projections] FROM nodes WHERE label='Label' [AND filters]
//!
//!   2) CREATE (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!         → INSERT INTO nodes(label,name,props) ...; SELECT * FROM nodes WHERE ... LIMIT 1
//!
//!   3) MATCH (n:Label {id:'123'}) SET n.k1='x', n.k2='y' [RETURN n]
//!         → UPDATE nodes SET ... WHERE label='Label' [AND filters]; [SELECT ...]
//!
//!   4) MATCH (n:Label {k1:'v1'}) DELETE n [RETURN count(n)]
//!         → DELETE FROM nodes WHERE label='Label' [AND filters]; [SELECT ROW_COUNT() AS `count`]
//!
//! Notes:
//!  - Property `name` maps to the `name` column.
//!  - All other properties live inside JSON column `props` using JSON_EXTRACT/JSON_SET.
//!  - We split multi-statement strings and parse them one-by-one to AST.
//!
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






/// Entry A: Cypher text → provisional SQL text (possibly multiple statements).
pub fn cypher_to_sql_text(cypher: &str) -> CypherResult<String> {
    let s = cypher.trim();
    let su = s.to_uppercase();

    if su.starts_with("MATCH ") {
        if su.contains(" SET ") {
            return match_set_to_update_select(s);
        }
        if su.contains(" DELETE ") {
            return match_delete_to_delete_select(s);
        }
        return match_to_select(s);
    }
    if su.starts_with("CREATE ") {
        return create_to_insert_select(s);
    }
    Err(CypherError::Unsupported(s.to_string()))
}






/// Entry B: Parse provisional SQL via repo parser → AST.
/// Handles multi-statement strings safely.
pub fn cypher_to_sql_ast(cypher: &str) -> CypherResult<Vec<Statement>> {
    let sql = cypher_to_sql_text(cypher)?;
    let dialect = MySqlDialect {};
    let mut out: Vec<Statement> = Vec::new();

    for part in sql.split(';') {
        let stmt = part.trim();
        if stmt.is_empty() {
            continue;
        }
        let mut v = Parser::parse_sql(&dialect, stmt)
            .map_err(|e| CypherError::Parse(format!("while parsing `{stmt}`: {e}")))?;
        out.append(&mut v);
    }
    Ok(out)
}

/// Entry C: AST → final SQL text (one statement per line, ending with ';').
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

    // WHERE predicates: label + optional {props}
    let mut preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];
    if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        preds.extend(prop_predicates(&cols, &vals));
    }
    let where_sql = format!(" WHERE {}", preds.join(" AND "));

    // RETURN clause
    let up = s.to_uppercase();
    let ret_pos = up
        .find(" RETURN ")
        .ok_or_else(|| CypherError::Unsupported("missing RETURN".into()))?;
    let ret_txt = s[ret_pos + " RETURN ".len()..].trim();

    let projection = if ret_txt.eq_ignore_ascii_case("n") {
        "*".to_string()
    } else {
        // Accept: n.prop [AS alias], prop, ...
        let mut pieces = Vec::new();
        for part in ret_txt.split(',') {
            let p = part.trim();
            if p.is_empty() {
                continue;
            }
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
        if pieces.is_empty() {
            "*".to_string()
        } else {
            pieces.join(", ")
        }
    };

    Ok(format!(
        "SELECT {} FROM {}{}",
        projection,
        ident_sql("nodes"),
        where_sql
    ))
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
        ident_sql("nodes"),
        sql_str(&label),
        name_sql,
        props_sql
    );

    // Return created row (filter by label + optional name)
    let mut preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];
    if let Some(v) = name_opt {
        preds.push(format!("name = {}", sql_str(v)));
    }
    let select = format!(
        "SELECT * FROM {} WHERE {} LIMIT 1",
        ident_sql("nodes"),
        preds.join(" AND ")
    );

    Ok(format!("{insert};\n{select}"))
}

/// MATCH ... SET ... [RETURN n] → UPDATE; [SELECT ...]
fn match_set_to_update_select(s: &str) -> CypherResult<String> {
    let up = s.to_uppercase();
    let label = parse_label(s)?;

    // WHERE: label + optional {props}
    let mut preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];
    if let Some(props) = extract_between(s, "{", "}") {
        let (cols, vals) = parse_kv_props(props)?;
        preds.extend(prop_predicates(&cols, &vals));
    }
    let where_sql = format!(" WHERE {}", preds.join(" AND "));

    // SET list
    let set_start =
        up.find(" SET ")
            .ok_or_else(|| CypherError::Unsupported("missing SET".into()))?
            + " SET ".len();
    let set_end = up.find(" RETURN ").unwrap_or(s.len());
    let set_txt = s[set_start..set_end].trim();
    let set_pairs = parse_set_list(set_txt)?;
    if set_pairs.is_empty() {
        return Err(CypherError::Parse("empty SET list".into()));
    }

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

    let update = format!(
        "UPDATE {} SET {}{}",
        ident_sql("nodes"),
        set_sqls.join(", "),
        where_sql
    );

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
    let where_sql = format!(" WHERE {}", preds.join(" AND "));

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
    cols.iter()
        .zip(vals)
        .map(|(k, v)| {
            if k.eq_ignore_ascii_case("name") {
                format!("name = {}", sql_str(v))
            } else {
                format!(
                    "JSON_UNQUOTE(JSON_EXTRACT(props, {})) = {}",
                    sql_str(&format!("$.\"{}\"", k)),
                    sql_str(v)
                )
            }
        })
        .collect()
}

/// SELECT expression to read a JSON property as text
fn json_get_expr(prop: &str) -> String {
    format!(
        "JSON_UNQUOTE(JSON_EXTRACT(props, {}))",
        sql_str(&format!("$.\"{}\"", prop))
    )
}

/// Build JSON_OBJECT('k1','v1','k2','v2',...) from k/v lists
fn json_object(cols: &[String], vals: &[String]) -> String {
    let mut parts: Vec<String> = Vec::new();
    for (k, v) in cols.iter().zip(vals) {
        parts.push(sql_str(k));
        parts.push(sql_str(v));
    }
    format!("JSON_OBJECT({})", parts.join(", "))
}

/// Split properties into optional name and (others)
fn split_name_and_others<'a>(
    cols: &'a [String],
    vals: &'a [String],
) -> (Option<&'a str>, Vec<String>, Vec<String>) {
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

/* ===================== Generic string parsing utils ===================== */

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
    if c.chars()
        .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
    {
        c.to_string()
    } else {
        format!("`{}`", c.replace('`', "``"))
    }
}

fn sql_str(v: &str) -> String {
    format!("'{}'", v.replace('\'', "''"))
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

/// Parse SET list like: "n.k1='x', n.k2='y'"
fn parse_set_list(s: &str) -> CypherResult<Vec<(String, String)>> {
    let mut out = Vec::new();
    for part in s.split(',') {
        let p = part.trim();
        if p.is_empty() {
            continue;
        }
        let p = p.strip_prefix("n.").unwrap_or(p).trim();
        let mut it = p.splitn(2, '=');
        let k = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad SET key".into()))?
            .trim();
        let v = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad SET value".into()))?
            .trim();
        if k.is_empty() {
            return Err(CypherError::Parse("empty SET key".into()));
        }
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

/* ===================== Unit tests ===================== */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_match_return_n_nodes() {
        let cy = "MATCH (n:Person {name:'Alice', city:'ATX'}) RETURN n";
        let ast = cypher_to_sql_ast(cy).unwrap();
        let sql = render_statements(&ast).to_lowercase();
        assert!(sql.contains("from nodes"));
        assert!(sql.contains("label = 'person'"));
        // `name` is a column; `city` is in JSON
        assert!(sql.contains("name = 'alice'"));
        assert!(sql.contains("json_extract(props, '$.\"city\"')"));
    }

    #[test]
    fn t_match_projection_alias_nodes() {
        let cy = "MATCH (n:Person) RETURN n.name AS name, n.city";
        let txt = cypher_to_sql_text(cy).unwrap().to_lowercase();
        assert!(txt.contains("select name as name"));
        assert!(txt.contains("json_extract(props, '$.\"city\"')"));
        assert!(txt.contains("from nodes"));
        assert!(txt.contains("where label = 'person'"));
    }

    #[test]
    fn t_create_insert_then_select_nodes() {
        let cy = "CREATE (n:Person {name:'Alice', city:'ATX'}) RETURN n";
        let ast = cypher_to_sql_ast(cy).unwrap();
        let sql = render_statements(&ast).to_lowercase();
        assert!(sql.contains("insert into nodes"));
        assert!(sql.contains("(label, name, props)"));
        assert!(sql.contains("json_object('city', 'atx')"));
        assert!(sql.contains("select * from nodes where label = 'person' and name = 'alice'"));
    }

    #[test]
    fn t_match_set_update_then_select_nodes() {
        let cy = "MATCH (n:Person {id:'123'}) SET n.city='SEA', n.name='Bob' RETURN n";
        let txt = cypher_to_sql_text(cy).unwrap().to_lowercase();
        // name becomes column, city goes via JSON_SET
        assert!(txt.contains("update nodes set"));
        assert!(txt.contains("name = 'bob'"));
        assert!(txt.contains("props = json_set("));
        // WHERE includes label + json filter for id
        assert!(txt.contains("where label = 'person' and json_unquote(json_extract(props, '$.\"id\"')) = '123'"));
        // Follow-up SELECT:
        assert!(txt.contains("select * from nodes where label = 'person' and"));
    }

    #[test]
    fn t_match_delete_plain_nodes() {
        let cy = "MATCH (n:Person {id:'1'}) DELETE n";
        let sql = cypher_to_sql_text(cy).unwrap().to_lowercase();
        assert!(sql.contains("delete from nodes where label = 'person' and"));
        assert!(sql.contains("json_unquote(json_extract(props, '$.\"id\"')) = '1'"));
    }

    #[test]
    fn t_match_delete_return_count_nodes() {
        let cy = "MATCH (n:Person {city:'ATX'}) DELETE n RETURN count(n)";
        let sql = cypher_to_sql_text(cy).unwrap().to_lowercase();
        assert!(sql.contains("delete from nodes where label = 'person' and"));
        assert!(sql.contains("json_unquote(json_extract(props, '$.\"city\"')) = 'atx'"));
        assert!(sql.contains("select row_count() as `count`"));
    }
}
