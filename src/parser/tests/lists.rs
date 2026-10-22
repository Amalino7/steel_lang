//! Comma lists (`parser/lists.rs`): trailing commas, missing separators, missing closers, and the validation
//! done on the parsed list.

use super::helpers::*;

#[test]
fn test_trailing_comma_is_allowed_in_every_list() {
    for source in [
        "let x = f(1, 2,);",
        "let x = (1, 2,);",
        "let x = [1, 2,];",
        "let x = [1: 2,];",
        "let x = |a, b,| a;",
        "func f(a: number,) {}",
        "let f: func(number,): number = g;",
        "func f<T,>() {}",
        "let x: List<number,> = y;",
        "let x: (number, string,) = y;",
        "struct P { x: number, }",
        "enum E { A(number,), B { x: number, }, }",
        "let (a, b,) = t;",
        "let P(x: a, y: b,) = t;",
        "match x { .V(a, b,) => 1 }",
        "impl P: A, B, {}",
        "import a/{b, c,};",
    ] {
        assert_parses_ok(source);
    }
}

#[test]
fn test_missing_separator_is_reported_and_both_items_kept() {
    assert_parse_errors("let x = f(1 2);", &["1:12"]);
    assert_parse_errors("struct P { x: number y: number }", &["1:21"]);
    assert_parse_errors("enum E { A B }", &["1:11"]);
    assert_parse_errors("let (a b) = t;", &["1:7"]);
}

#[test]
fn test_missing_separator_in_struct_keeps_both_fields() {
    // Only the separator error: both fields parsed, so nothing else (like a missing `}`) is reported.
    assert_error_count("struct P { x: number y: number } let z = 1;", 1);
}

#[test]
fn test_empty_item_between_commas_is_reported() {
    assert_parse_errors("let x = f(1,, 2);", &["1:13"]);
    assert_parse_errors("let x = f(,);", &["1:11"]);
}

#[test]
fn test_bad_item_is_skipped_to_the_next_comma() {
    // The error is on the bad argument; the rest of the call and the next statement still parse.
    assert_parse_errors("let x = f(1, 2 +, 3); let y = 2;", &["1:17"]);
}

#[test]
fn test_wrong_closer_is_reported_once() {
    assert_parse_errors("let x = f(1, ]; let y = 2;", &["1:14"]);
}

#[test]
fn test_missing_closer_points_after_the_last_item_and_mentions_the_closer() {
    assert_parse_errors("let x = f(1, 2;", &["1:15"]);
    assert_error_contains("let x = f(1, 2;", "Missing ')'");
}

#[test]
fn test_unclosed_list_stops_at_the_statement_boundary() {
    // The `let` on the next line is not swallowed while looking for `)`.
    assert_error_count("let x = f(1, 2\nlet y = 3;", 1);
}

#[test]
fn test_unclosed_index_reports_once() {
    assert_error_count("let x = a[1; let y = 2;", 1);
}

#[test]
fn test_greater_equal_closes_generic_arguments() {
    assert_parses(
        "let x: List<number>= y;",
        "(let (bind x) (type List (type number)) y)",
    );
}

#[test]
fn test_grouping_and_tuples() {
    assert_expr("(a)", "(group a)");
    assert_expr("(a,)", "(tuple a)");
    assert_expr("(a, b,)", "(tuple a b)");
    assert_parse_errors("let x = ();", &["1:9"]);
}

#[test]
fn test_tuple_type_and_binding_need_two_items_or_a_trailing_comma() {
    assert_parses_ok("let x: (number,) = y;");
    assert_parse_errors("let x: (number) = y;", &["1:8"]);
    assert_parse_errors("let x: () = y;", &["1:8"]);
    assert_parses_ok("let (a,) = t;");
    assert_parse_errors("let (a) = t;", &["1:5"]);
}

#[test]
fn test_empty_payloads_are_errors() {
    assert_parse_errors("enum E { A() }", &["1:11"]);
    assert_parse_errors("match x { .V() => 1 }", &["1:13"]);
}

#[test]
fn test_map_and_list_items_must_not_mix() {
    assert_expr("[:]", "(map)");
    assert_parse_errors("let x = [1: 2, 3];", &["1:16"]);
    assert_parse_errors("let x = [1, 2: 3];", &["1:13"]);
}

#[test]
fn test_self_parameter_rules() {
    assert_parses_ok("impl P { func f(self, a: number) {} }");
    assert_parse_errors("func f(self) {}", &["1:8"]);
    assert_parse_errors("impl P { func f(a: number, self) {} }", &["1:28"]);
    assert_parse_errors("impl P { func f(self, self) {} }", &["1:23"]);
    assert_parse_errors("interface I { func f(a: number, self); }", &["1:33"]);
}

#[test]
fn test_bad_item_is_reported_by_its_own_parser() {
    assert_parse_errors("let x = f(1, *2);", &["1:14"]);
    assert_error_contains("let x = f(1, *2);", "Expected expression");
    assert_parse_errors("func f(match: number) {}", &["1:8"]);
    assert_error_contains("func f(match: number) {}", "is a keyword");
    assert_parse_errors("struct P { 1: number }", &["1:12"]);
    assert_error_contains("struct P { 1: number }", "Expected a name");
}

#[test]
fn test_failed_item_skips_the_list_shape_checks() {
    // Only the bad item: no "empty payload" or "empty tuple" on top of it.
    assert_parse_errors("let x = match y { .A(1) => 2 };", &["1:22"]);
    assert_parse_errors("enum E { A(1) }", &["1:12"]);
    assert_parse_errors("let (1) = t;", &["1:6"]);
}

#[test]
fn test_statement_boundary_at_an_item_position_is_a_missing_closer() {
    // The `;` is the statement's, not a bad argument, so it is neither reported nor consumed by the list.
    assert_parse_errors("func f() {\n h(;\n}", &["2:4"]);
    assert_error_contains("func f() {\n h(;\n}", "Missing ')'");
    assert_parse_errors("let x = f(a,;", &["1:13"]);
    assert_parse_errors("let x = foo(", &["1:13"]);
    assert_error_contains("let x = foo(", "Missing ')'");
}

#[test]
fn test_unclosed_nested_generics_do_not_swallow_the_initializer() {
    let source = "let m: List<List<number> = []; let y = 1;";
    assert_parse_errors(source, &["1:26"]);
    assert_error_contains(source, "Missing '>'");
}

#[test]
fn test_list_cut_at_a_line_break_leaves_the_next_line_alone() {
    // `g(c +)` is parsed as its own statement, so its error is reported too.
    assert_parse_errors("let x = f(a, b\ng(c +);", &["1:15", "2:6"]);
}

#[test]
fn test_missing_comma_across_lines_keeps_both_items() {
    assert_parse_errors("let x = f(\n a\n b\n);", &["2:3"]);
    // On a single line so far, the next line still continues the list when it ends with the closer.
    assert_parse_errors("let x = f(a\nb);", &["1:12"]);
}

#[test]
fn test_bad_binding_in_a_pattern_names_its_real_context() {
    assert_error_contains(
        "let x = match y { .A(1) => 2 };",
        "Expected a name after '('",
    );
    assert_error_contains("let 1 = 2;", "Expected a name after 'let'");
}
