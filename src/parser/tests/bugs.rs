//! Known parser bugs, one failing test each. The `#[ignore]` reason is the issue id from
//! `docs/features/parser-improvements/issues-and-plan.md`; remove the attribute in the commit that fixes it.
//! (B4 is a runtime bug and lives in `vm/tests/integration/`; B11 is fixed by the AST printer.)

use super::helpers::*;

/// The trailing statements of a file are silently dropped once the scanner has hit the end of the input.
#[test]
fn test_b1_last_statement_is_kept() {
    assert_parses("let x = 1; x;", "(let (bind x) ? 1) x");
}

#[test]
fn test_b1_trailing_expression_without_semicolon_is_reported() {
    assert_parse_errors("let x = 1; x", &["1:13"]);
}

#[test]
fn test_b1_unclosed_paren_before_end_reports_once() {
    assert_error_count("func f() {\n let a = (1 + 2;\n}", 1);
}

#[test]
fn test_b2_missing_semicolon_in_block_expression_is_reported() {
    assert_error_count("let x = { 1 2 }; let y = 1;", 1);
}

#[test]
fn test_b3_function_type_span_starts_at_func_keyword() {
    assert_span(
        "let f: func(number): string = 5;",
        "let/fn-type",
        "func(number): string",
    );
}

#[test]
fn test_b5_nested_tuple_access() {
    assert_parses(
        "let t = ((1, 2), 3); let y = t.0.1;",
        "(let (bind t) ? (tuple (tuple 1 2) 3)) (let (bind y) ? (get (get t .0) .1))",
    );
}

#[test]
fn test_b5_tuple_index_after_safe_access_and_float_like_index() {
    assert_expr("t?.0.1", "(get (get? t .0) .1)");
    assert_expr("t.1.5", "(get (get t .1) .5)");
}

#[test]
fn test_b5_tuple_index_with_leading_zero_is_error() {
    assert_parse_errors("let y = t.01;", &["1:11"]);
    assert_parse_errors("let y = t?.00;", &["1:12"]);
}

#[test]
fn test_b6_invalid_number_is_reported_on_the_literal() {
    assert_parse_errors("let z = 0xFFFFFFFFFFFFFFFFFF;", &["1:9"]);
}

#[test]
fn test_b7_invalid_assignment_target_is_reported_on_the_target() {
    assert_parse_errors("5 = a + 1; let y = 1;", &["1:1"]);
}

/// A lexer error is reported once and skipped; it must not also derail the statement after it.
#[test]
fn test_b8_lexer_error_is_reported_once() {
    assert_parse_errors("let a = 1; @ let b = 2;", &["1:12"]);
}

#[test]
fn test_b9_block_statement_span_covers_the_braces() {
    assert_span(
        "func f() { let a = 1; } let y = 1;",
        "func/block-stmt",
        "{ let a = 1; }",
    );
    assert_span("func f() { } let y = 1;", "func/block-stmt", "{ }");
}

#[test]
fn test_b10_import_span_includes_the_terminator() {
    assert_span("import a/b; let y = 1;", "import", "import a/b;");
}

// Spans that end too early (issues doc, section B).
#[test]
#[ignore = "B-spans"]
fn test_span_call_includes_closing_paren() {
    assert_span("let x = f(1, 2);", "let/call", "f(1, 2)");
    assert_span("let x = f();", "let/call", "f()");
}

#[test]
#[ignore = "B-spans"]
fn test_span_index_includes_closing_bracket() {
    assert_span("let x = a[0];", "let/index", "a[0]");
}

#[test]
#[ignore = "B-spans"]
fn test_span_grouping_and_tuple_include_parens() {
    assert_span("let x = (a);", "let/group", "(a)");
    assert_span("let x = (a, b);", "let/tuple", "(a, b)");
}

#[test]
#[ignore = "B-spans"]
fn test_span_list_includes_brackets() {
    assert_span("let x = [1, 2];", "let/list", "[1, 2]");
}

#[test]
#[ignore = "B-spans"]
fn test_span_let_includes_keyword_and_semicolon() {
    assert_span("let x = 1;", "let", "let x = 1;");
}

#[test]
#[ignore = "B-spans"]
fn test_span_function_includes_func_keyword() {
    assert_span("public func f() {}", "func", "public func f() {}");
}

#[test]
#[ignore = "B-spans"]
fn test_span_struct_includes_braces() {
    assert_span("struct P { x: number }", "struct", "struct P { x: number }");
}
