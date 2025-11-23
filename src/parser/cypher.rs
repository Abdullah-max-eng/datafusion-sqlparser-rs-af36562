//! Very small Cypher → SQL “desugaring” layer for a nodes(+edges) graph in MySQL.
//!        Abdullah Fazli 
//! 
//  /// you can reference to the sql directory in the graphdb folder, on the go server side 
// Target table layout (only `nodes` is used here):
//!   nodes(
//!       id    BIGINT PK AUTO_INCREMENT,
//!       label VARCHAR(64)   NOT NULL,
//!       name  VARCHAR(255),
//!       props JSON          NULL
//!   )
//!




//! Supported Cypher patterns (case-insensitive):
//!
//! 1) MATCH
//!    - MATCH (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!    - MATCH (n:Label) RETURN n.name, n.k2 AS alias
//!
//!    becomes roughly:
//!      SELECT [* | projections] FROM nodes
//!      WHERE label='Label' [AND props-based filters]
//!
//! 2) CREATE
//!    - CREATE (n:Label {k1:'v1', k2:'v2'}) RETURN n
//!
//!    becomes:
//!      INSERT INTO nodes(label, name, props) ...
//!      SELECT * FROM nodes WHERE ... LIMIT 1
//!
//! 3) SET (update)
//!    - MATCH (n:Label {id:'123'}) SET n.k1='x', n.k2='y' [RETURN n]
//!
//!    becomes:
//!      UPDATE nodes SET ... WHERE label='Label' [AND filters];
//!      [SELECT ...]
//!
//! 4) DELETE
//!    - MATCH (n:Label {k1:'v1'}) DELETE n [RETURN count(n)]
//!
//!    becomes:
//!      DELETE FROM nodes WHERE label='Label' [AND filters];
//!      [SELECT ROW_COUNT() AS `count`]
//!
//! Notes:
//!  - Property `name` is mapped to the `name` column.
//!  - All other properties live inside the JSON `props` column and are accessed
//!    via JSON_EXTRACT / JSON_SET.
//!  - Multi-statement strings are split and each piece is parsed into the AST.
//!



use thiserror::Error;

use crate::ast::Statement;
use crate::dialect::MySqlDialect;
use crate::parser::Parser;

/// Errors that can occur while handling Cypher input.
#[derive(Error, Debug)]
pub enum CypherError {
    /// The query shape is not something this tiny layer understands.
    #[error("unsupported or invalid cypher: {0}")]
    Unsupported(String),

    /// Parsing failed somewhere after translation to SQL.
    #[error("parse error: {0}")]
    Parse(String),
}

/// Convenience alias for fallible Cypher operations.
pub type CypherResult<T> = Result<T, CypherError>;







// // Entry A: Cypher text provisional SQL text (may be multi-statement). --- this is out main entyr that will call other expernal fucntion (public api )





/// Convert a single Cypher statement into SQL text.
///
/// This is intentionally very small in scope. It simply looks at the
/// *shape* of the Cypher string and dispatches to one of the supported
/// patterns (MATCH/CREATE with optional SET/DELETE).
pub fn cypher_to_sql_text(cypher: &str) -> CypherResult<String> {
    // Trim whitespace and normalize for simple prefix checks.
    let trimmed = cypher.trim();
    let upper = trimmed.to_uppercase();

    // Basic pattern routing:
    if upper.starts_with("MATCH ") {
        // MATCH with SET → UPDATE (and optional SELECT)
        if upper.contains(" SET ") {
            return match_set_to_update_select(trimmed);
        }
               // MATCH with DELETE → DELETE (and optional ROW_COUNT() SELECT)
        if upper.contains(" DELETE ") {
            return match_delete_to_delete_select(trimmed);
        }
        // Plain MATCH ... RETURN ...
        return match_to_select(trimmed);
    }
    // CREATE ... RETURN ...
    if upper.starts_with("CREATE ") {
        return create_to_insert_select(trimmed);
    }

    // Anything else is out of scope for this tiny adapter.
    Err(CypherError::Unsupported(trimmed.to_string()))
}




//. ----------- ---- 
// Entry B: Cypher to ast
// Handles multi-statement SQL safely.
/// Convert Cypher text into a vector of SQL AST statements.
/// This does:
///   1. it changes cyper to ast provisional SQL (see `cypher_to_sql_text`).
///   2. Split SQL on `;`.
///   3. Parse each piece into one or more `Statement`s using the repo parser.


pub fn cypher_to_sql_ast(cypher: &str) -> CypherResult<Vec<Statement>> {
    let sql = cypher_to_sql_text(cypher)?;
    let dialect = MySqlDialect {};
    let mut stmts: Vec<Statement> = Vec::new();
    for part in sql.split(';') {
        let stmt = part.trim();
        if stmt.is_empty() {
            continue;
        }
        let mut parsed = Parser::parse_sql(&dialect, stmt).map_err(|e| CypherError::Parse(format!("while parsing `{stmt}`: {e}")))?;  // tgus us guvubg us an array
        // printls(parsed); 
        stmts.append(&mut parsed);
    }
    Ok(stmts)
}


// entry 3: AST === final SQL text
// One statement per line, each ending with `;`.
/// Render a list of parsed SQL statements back into concrete SQL text.
/// This is mostly for tests or debugging; execution callers will usually
/// use the AST directly.
pub fn render_statements(stmts: &[Statement]) -> String {
    let mut buf = String::new();

    for (idx, stmt) in stmts.iter().enumerate() {
        if idx > 0 {
            buf.push('\n');
        }

        buf.push_str(&stmt.to_string());
        buf.push(';');
    }

    buf
}


// Stage A helpers: Cypher → SQL text (desugaring).
/// Desugar a simple MATCH ... RETURN ... into a SELECT.
/// Supported patterns:
///   MATCH (n:Label {k1:'v1', ...}) RETURN ...
fn match_to_select(source: &str) -> CypherResult<String> {
    let label = parse_label(source)?;

    // Build the WHERE predicate list.
    // We always filter on `label`, and maybe more if `{props}` exists.
    let mut where_preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];

    if let Some(raw_props) = extract_between(source, "{", "}") {
        let (cols, vals) = parse_kv_props(raw_props)?;
        where_preds.extend(prop_predicates(&cols, &vals));
    }

    let where_sql = format!(" WHERE {}", where_preds.join(" AND "));

    // Figure out the RETURN clause text (projections).
    let upper = source.to_uppercase();
    let ret_pos = upper
        .find(" RETURN ")
        .ok_or_else(|| CypherError::Unsupported("missing RETURN".into()))?;

    let ret_txt = source[ret_pos + " RETURN ".len()..].trim();

    // Special case: RETURN n → SELECT *
    let projection = if ret_txt.eq_ignore_ascii_case("n") {
        "*".to_string()
    } else {
        // More interesting case: RETURN n.prop [AS alias], ...
        // We keep this fairly permissive but simple.
        let mut pieces = Vec::new();

        for part in ret_txt.split(',') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }

            // Strip the leading `n.` if present.
            let core = part.strip_prefix("n.").unwrap_or(part).trim();
            let (prop, alias) = split_as(core);

            // `name` maps to the dedicated column; everything else is JSON.
            let expr = if prop.eq_ignore_ascii_case("name") {
                "name".to_string()
            } else {
                json_get_expr(prop)
            };

            let projected = match alias {
                Some(a) => format!("{} AS {}", expr, ident_sql(a)),
                None => expr,
            };

            pieces.push(projected);
        }

        if pieces.is_empty() {
            // If we somehow parsed nothing, be forgiving and just use `*`.
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
///
/// We do a two-step:
///   1. INSERT a row into `nodes`.
///   2. SELECT the inserted row using label + optional name filter.
fn create_to_insert_select(source: &str) -> CypherResult<String> {
    let label = parse_label(source)?;
    let props_block = need_between(source, "{", "}", "missing {..} properties")?;
    let (cols, vals) = parse_kv_props(&props_block)?;

    // Split properties into `name` vs "everything else" (which goes into JSON).
    let (name_opt, other_cols, other_vals) = split_name_and_others(&cols, &vals);

    let name_sql = name_opt
        .map(|v| sql_str(v))
        .unwrap_or_else(|| "NULL".to_string());

    let props_sql = if other_cols.is_empty() {
        "NULL".to_string()
    } else {
        json_object(&other_cols, &other_vals)
    };

    // INSERT statement.
    let insert_sql = format!(
        "INSERT INTO {} (label, name, props) VALUES ({}, {}, {})",
        ident_sql("nodes"),
        sql_str(&label),
        name_sql,
        props_sql
    );

    // Build a small predicate to re-select the inserted row.
    let mut where_preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];

    if let Some(name_val) = name_opt {
        where_preds.push(format!("name = {}", sql_str(name_val)));
    }

    let select_sql = format!(
        "SELECT * FROM {} WHERE {} LIMIT 1",
        ident_sql("nodes"),
        where_preds.join(" AND ")
    );

    Ok(format!("{insert_sql};\n{select_sql}"))
}

/// MATCH ... SET ... [RETURN n] → UPDATE; [SELECT ...]
///
/// We:
///   1. Build a WHERE clause from label + optional {props}.
///   2. Parse the SET list into column/JSON updates.
///   3. Optionally append a SELECT if there's a RETURN.
fn match_set_to_update_select(source: &str) -> CypherResult<String> {
    let upper = source.to_uppercase();
    let label = parse_label(source)?;

    // WHERE predicates (label + props).
    let mut where_preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];

    if let Some(raw_props) = extract_between(source, "{", "}") {
        let (cols, vals) = parse_kv_props(raw_props)?;
        where_preds.extend(prop_predicates(&cols, &vals));
    }

    let where_sql = format!(" WHERE {}", where_preds.join(" AND "));

    // Slice out the body of the SET clause.
    let set_start = upper
        .find(" SET ")
        .ok_or_else(|| CypherError::Unsupported("missing SET".into()))?
        + " SET ".len();

    // SET runs until RETURN (if present) or end of string.
    let set_end = upper.find(" RETURN ").unwrap_or(source.len());
    let set_body = source[set_start..set_end].trim();

    let set_pairs = parse_set_list(set_body)?;
    if set_pairs.is_empty() {
        return Err(CypherError::Parse("empty SET list".into()));
    }

    // We'll build two groups:
    //   - direct column assignments (e.g., `name = 'Bob'`)
    //   - JSON_SET mutation over `props`.
    let mut set_clauses: Vec<String> = Vec::new();
    let mut json_updates: Vec<(String, String)> = Vec::new();

    for (key, val) in set_pairs {
        if key.eq_ignore_ascii_case("name") {
            set_clauses.push(format!("name = {}", sql_str(&val)));
        } else {
            json_updates.push((key, val));
        }
    }

    if !json_updates.is_empty() {
        // We always treat `props` as a JSON object; if NULL, start with {}.
        //
        // props = JSON_SET(IFNULL(props, JSON_OBJECT()),
        //                  '$."k1"', 'v1',
        //                  '$."k2"', 'v2', ...)
        let mut args: Vec<String> = vec!["IFNULL(props, JSON_OBJECT())".to_string()];

        for (key, val) in &json_updates {
            args.push(sql_str(&format!("$.\"{}\"", key)));
            args.push(sql_str(val));
        }

        set_clauses.push(format!("props = JSON_SET({})", args.join(", ")));
    }

    let update_sql = format!(
        "UPDATE {} SET {}{}",
        ident_sql("nodes"),
        set_clauses.join(", "),
        where_sql
    );

    // Optional follow-up SELECT if RETURN is present.
    let optional_select = if upper.contains(" RETURN ") {
        Some(format!("SELECT * FROM {}{}", ident_sql("nodes"), where_sql))
    } else {
        None
    };

    Ok(match optional_select {
        Some(select_sql) => format!("{update_sql};\n{select_sql}"),
        None => update_sql,
    })
}




fn match_delete_to_delete_select(source: &str) -> CypherResult<String> {
    let upper = source.to_uppercase();
    let label = parse_label(source)?;

    let mut where_preds: Vec<String> = vec![format!("label = {}", sql_str(&label))];

    if let Some(raw_props) = extract_between(source, "{", "}") {
        let (cols, vals) = parse_kv_props(raw_props)?;
        where_preds.extend(prop_predicates(&cols, &vals));
    }

    let where_sql = format!(" WHERE {}", where_preds.join(" AND "));
    let delete_sql = format!("DELETE FROM {}{}", ident_sql("nodes"), where_sql);

    // Special-case: RETURN count(n) → ROW_COUNT() wrapper.
    if upper.contains(" RETURN ") && upper.contains("COUNT(") {
        Ok(format!(
            "{delete_sql};\nSELECT ROW_COUNT() AS {}",
            ident_sql("count")
        ))
    } else {
        Ok(delete_sql)
    }
}


// 



// JSON helpers for `nodes.props`.
// ==========================================================================

/// Build WHERE predicates from property lists.
///
/// `name` is mapped to the `name` column; all other keys are pulled from JSON.
fn prop_predicates(cols: &[String], vals: &[String]) -> Vec<String> {
    cols.iter()
        .zip(vals)
        .map(|(key, val)| {
            if key.eq_ignore_ascii_case("name") {
                format!("name = {}", sql_str(val))
            } else {
                // JSON_UNQUOTE(JSON_EXTRACT(props, '$."key"')) = 'val'
                format!(
                    "JSON_UNQUOTE(JSON_EXTRACT(props, {})) = {}",
                    sql_str(&format!("$.\"{}\"", key)),
                    sql_str(val)
                )
            }
        })
        .collect()
}

/// Build a SELECT expression to read a JSON property as plain text.
///
/// e.g. `JSON_UNQUOTE(JSON_EXTRACT(props, '$."city"'))`
fn json_get_expr(prop: &str) -> String {
    format!(
        "JSON_UNQUOTE(JSON_EXTRACT(props, {}))",
        sql_str(&format!("$.\"{}\"", prop))
    )
}

/// Build `JSON_OBJECT('k1','v1','k2','v2',...)` from parallel lists.
fn json_object(cols: &[String], vals: &[String]) -> String {
    let mut parts: Vec<String> = Vec::new();

    for (key, val) in cols.iter().zip(vals) {
        parts.push(sql_str(key));
        parts.push(sql_str(val));
    }

    format!("JSON_OBJECT({})", parts.join(", "))
}

/// Split properties into optional `name` and "everything else".
///
/// Returns:
///   (maybe_name_value, other_keys, other_values)
fn split_name_and_others<'a>(
    cols: &'a [String],
    vals: &'a [String],
) -> (Option<&'a str>, Vec<String>, Vec<String>) {
    let mut name_value: Option<&str> = None;
    let mut other_keys = Vec::new();
    let mut other_vals = Vec::new();

    for (key, val) in cols.iter().zip(vals) {
        if key.eq_ignore_ascii_case("name") {
            name_value = Some(val.as_str());
        } else {
            other_keys.push(key.clone());
            other_vals.push(val.clone());
        }
    }

    (name_value, other_keys, other_vals)
}



// ==========================================================================

/// Extract the label after ':' up to the first delimiter: space, '{', or ')'.
///
/// For example:
///   "MATCH (n:Person {name:'Alice'})" → "Person"
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

/// Produce a safe SQL identifier, quoting with backticks only when needed.
/// This tries to stay out of the way for "normal" identifiers.
fn ident_sql(name: &str) -> String {
    if name.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '_') {
        name.to_string()
    } else {
        format!("`{}`", name.replace('`', "``"))
    }
}



/// Single-quote a string for SQL, escaping embedded single quotes.
fn sql_str(v: &str) -> String {
    format!("'{}'", v.replace('\'', "''"))
}



/// Parse a simple `{k1:'v1', k2:'v2'}` property list.

/// This is deliberately small and strict:
///   - Only `key:'value'` pairs.
///   - Comma-separated.
///   - Values must be single-quoted strings.
fn parse_kv_props(s: &str) -> CypherResult<(Vec<String>, Vec<String>)> {
    let mut keys = Vec::new();
    let mut values = Vec::new();

    for pair in s.split(',') {
        let pair = pair.trim();
        if pair.is_empty() {
            continue;
        }

        let mut it = pair.splitn(2, ':');

        let key = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad key".into()))?
            .trim();

        let val_raw = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad value".into()))?
            .trim();

        // Strip outer quotes for the value (we only accept strings here).
        let val = val_raw.trim_matches('\'');
        if key.is_empty() {
            return Err(CypherError::Parse("empty key".into()));
        }

        keys.push(key.to_string());
        values.push(val.to_string());
    }

    if keys.is_empty() {
        return Err(CypherError::Parse("no properties".into()));
    }

    Ok((keys, values))
}

/// Parse a SET list like: "n.k1='x', n.k2='y'".
///
/// We only support string literals on the right-hand side.
fn parse_set_list(s: &str) -> CypherResult<Vec<(String, String)>> {
    let mut out = Vec::new();

    for part in s.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        // Strip leading `n.` if present.
        let part = part.strip_prefix("n.").unwrap_or(part).trim();

        let mut it = part.splitn(2, '=');

        let key = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad SET key".into()))?
            .trim();

        let val_raw = it
            .next()
            .ok_or_else(|| CypherError::Parse("bad SET value".into()))?
            .trim();

        if key.is_empty() {
            return Err(CypherError::Parse("empty SET key".into()));
        }

        // For now, only accept quoted strings on the right.
        let val = val_raw.trim_matches('\'');

        out.push((key.to_string(), val.to_string()));
    }

    Ok(out)
}

/// Split something like "prop AS alias" into ("prop", Some("alias")).
///
/// If there's no AS keyword, the alias is `None`.
fn split_as(s: &str) -> (&str, Option<&str>) {
    if let Some(idx) = s.to_uppercase().find(" AS ") {
        let prop = s[..idx].trim();
        let alias = s[idx + 4..].trim();
        (prop, Some(alias))
    } else {
        (s.trim(), None)
    }
}

/// Extract the substring between `left` and `right`, if present.
fn extract_between<'a>(s: &'a str, left: &str, right: &str) -> Option<&'a str> {
    let start = s.find(left)? + left.len();
    let end = s[start..].find(right)? + start;
    Some(&s[start..end])
}

/// Same as `extract_between`, but returns a CypherError if not found.
fn need_between(s: &str, left: &str, right: &str, msg: &str) -> CypherResult<String> {
    extract_between(s, left, right)
        .map(|x| x.to_string())
        .ok_or_else(|| CypherError::Parse(msg.into()))
}


// ==========================================================================
// Unit tests.
// ==========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Normalize a string: lowercase + collapse whitespace.
    ///
    /// Handy for slightly fuzzy substring checks in tests.
    fn norm(s: &str) -> String {
        let mut out = String::new();
        let mut prev_space = false;

        for ch in s.chars() {
            let c = ch.to_ascii_lowercase();

            if c.is_whitespace() {
                if !prev_space {
                    out.push(' ');
                    prev_space = true;
                }
            } else {
                out.push(c);
                prev_space = false;
            }
        }

        out.trim().to_string()
    }

    /// Assert that cypher_to_sql_text produces SQL containing ALL substrings.
    fn assert_text_contains_all(cypher: &str, want_subs: &[&str]) {
        let got = norm(&cypher_to_sql_text(cypher).expect("to sql_text"));

        for w in want_subs {
            let ww = norm(w);
            assert!(
                got.contains(&ww),
                "\nCY: {cypher}\nGOT:\n{got}\nMISSING SUBSTR:\n{ww}\n"
            );
        }
    }

    /// Assert that the full path (A→B→C) renders SQL containing ALL substrings.
    fn assert_ast_contains_all(cypher: &str, want_subs: &[&str]) {
        let ast = cypher_to_sql_ast(cypher).expect("to ast");
        let got = norm(&render_statements(&ast));

        for w in want_subs {
            let ww = norm(w);
            assert!(
                got.contains(&ww),
                "\nCY: {cypher}\nGOT:\n{got}\nMISSING SUBSTR:\n{ww}\n"
            );
        }
    }

    #[test]
    fn match_return_all() {
        assert_ast_contains_all(
            "MATCH (n:Person) RETURN n",
            &["select * from nodes", "where label = 'Person'"],
        );
    }

    #[test]
    fn match_projection_and_filter() {
        assert_text_contains_all(
            "MATCH (n:Person {name:'Alice', city:'ATX'}) RETURN n.name AS name, n.city",
            &[
                "select name as name, json_unquote(json_extract(props, '$.\"city\"'))",
                "from nodes",
                "where label = 'Person'",
                "and name = 'Alice'",
                "and json_unquote(json_extract(props, '$.\"city\"')) = 'ATX'",
            ],
        );
    }

    // #[test]
    // fn create_insert_then_select() {
    //     assert_ast_contains_all(
    //         "CREATE (n:Person {name:'Alice', city:'ATX'}) RETURN n",
    //         &[
    //             "insert into nodes (label, name, props) values ('Person','Alice', json_object('city','ATX'))",
    //             "select * from nodes where label = 'Person' and name = 'Alice' limit 1",
    //         ],
    //     );
    // }

    #[test]
    fn update_name_and_json_then_select() {
        assert_text_contains_all(
            "MATCH (n:Person {id:'123'}) SET n.city='SEA', n.name='Bob' RETURN n",
            &[
                "update nodes set",
                "name = 'Bob'",
                "props = json_set(",
                "'$.\"city\"', 'SEA'",
                "where label = 'Person'",
                "and json_unquote(json_extract(props, '$.\"id\"')) = '123'",
                "select * from nodes where label = 'Person' and json_unquote(json_extract(props, '$.\"id\"')) = '123'",
            ],
        );
    }

    // #[test]
    // fn delete_plain_and_count() {
    //     assert_text_contains_all(
    //         "MATCH (n:Person {city:'ATX'}) DELETE n RETURN count(n)",
    //         &[
    //             "delete from nodes where label = 'Person' and json_unquote(json_extract(props, '$.\"city\"')) = 'ATX'",
    //             "select row_count() as `count`",
    //         ],
    //     );
    // }

    #[test]
    fn errors_when_missing_return() {
        let err = cypher_to_sql_text("MATCH (n:Person)").unwrap_err();
        let msg = format!("{err}");
        assert!(msg.to_lowercase().contains("missing return"));
    }

    // #[test]
    // fn unsupported_relationship_patterns() {
    //     let err = cypher_to_sql_text("MATCH (a:Person)-[:ACTED_IN]->(m:Movie) RETURN a,m").unwrap_err();
    //     let msg = format!("{err}");
    //     assert!(msg.to_lowercase().contains("unsupported"));
    // }

    // #[test]
    // fn quote_escaping() {
    //     assert_text_contains_all(
    //         "MATCH (n:Person {nickname:'O\\'Brien'}) RETURN n.nickname",
    //         &[
    //             "json_unquote(json_extract(props, '$.\"nickname\"'))",
    //             // in SQL the single quote must be doubled:
    //             "='O''Brien'",
    //         ],
    //     );
    // }
}
