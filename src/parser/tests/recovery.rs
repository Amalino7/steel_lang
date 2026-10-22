//! Error recovery (`parser/recovery.rs`), missing `;` handling and lexer errors.

use super::helpers::*;

#[test]
fn test_missing_semicolon_before_a_new_line_points_after_the_previous_token() {
    assert_parse_errors("let a = 1\nlet b = 2;", &["1:10"]);
}

#[test]
fn test_missing_semicolon_on_the_same_line_is_reported_once() {
    assert_parse_errors("let a = 1 2; let b = 3;", &["1:11"]);
}

#[test]
fn test_function_body_has_no_tail_expression() {
    assert_parse_errors("func f(): number { 1 }", &["1:22"]);
}

#[test]
fn test_block_expression_tail() {
    assert_parses_ok("let x = { let a = 1; a };");
    assert_parses_ok("let x = { if a { f(); } g() };");
}

#[test]
fn test_brace_statement_has_no_continuation() {
    // `{ … }` at statement start is a whole statement, so `+ 1` starts a new (invalid) one.
    assert_parse_errors("func f() { { 1 } + 1; }", &["1:18"]);
}

#[test]
fn test_recovery_restarts_at_a_line_start_if() {
    let source = "func f() {\n let x = 1 * * 2\n if c { g(); }\n h(;\n}";
    // `if` is parsed: the only other error is the one inside `h(`.
    assert_parse_errors(source, &["2:14", "4:4"]);
}

#[test]
fn test_recovery_skips_a_mid_line_if() {
    assert_error_count("func f() { let x = 1 * * if c { 1 } else { 2 }; g(); }", 1);
}

#[test]
fn test_recovery_stops_at_declarations_after_a_broken_one() {
    assert_error_count("let a = ; public struct S {} extern func g(); let b = ;", 2);
}

#[test]
fn test_stray_closers_at_top_level_do_not_hang() {
    // Unmatched closers have no enclosing construct at top level: recovery steps over them.
    assert_error_count(") ] }", 1);
    assert_error_count("let a = 1; ) let b = 2;", 1);
}

#[test]
fn test_stray_closer_in_block_does_not_hang() {
    assert_error_count("func f() { ) let a = 1; }", 1);
}

#[test]
fn test_one_bad_method_does_not_abort_the_impl() {
    let source =
        "impl P {\n func a(self) { 1 + ; }\n func b(self x) {}\n func c(self) {}\n}\nlet y = 1;";
    // `self x`: a missing `,`, then `x` without a type. Method `c` and the `let` still parse.
    assert_parse_errors(source, &["2:21", "3:13", "3:15"]);
}

#[test]
fn test_bad_item_in_impl_body_recovers_to_the_next_method() {
    let source = "impl P {\n let x = 1;\n func b(self) {}\n}";
    assert_error_count(source, 1);
}

#[test]
fn test_bad_match_arm_is_skipped() {
    assert_error_count("let x = match y { .A => , .B => 2 };", 1);
    assert_error_count("let x = match y { 5 => 1, .B => 2 };", 1);
}

#[test]
fn test_error_inside_interpolation_recovers() {
    assert_error_count("let s = \"a${ 1 + }b\"; let y = 2;", 1);
}

#[test]
fn test_lexer_symbol_is_dropped() {
    assert_parse_errors("let a = 1 @;", &["1:11"]);
}

#[test]
fn test_unterminated_string_suppresses_later_errors() {
    assert_error_count("let a = (1; let b = \"abc", 2);
    assert_error_contains("let b = \"abc", "Unterminated string");
    assert_error_count("let b = \"abc", 1);
}

#[test]
fn test_keyword_as_name_is_reported_as_such() {
    assert_error_contains("let match = 1;", "is a keyword");
    assert_error_contains("func while() {}", "is a keyword");
}
