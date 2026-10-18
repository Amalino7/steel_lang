use super::helpers::*;

#[test]
fn test_arithmetic_precedence() {
    assert_expr("1 + 2 * 3 - 4 / 2", "(- (+ 1 (* 2 3)) (/ 4 2))");
}

#[test]
fn test_binary_is_left_associative() {
    assert_expr("a - b - c", "(- (- a b) c)");
}

#[test]
fn test_unary_binds_tighter_than_binary() {
    assert_expr("-a * 2", "(* (- a) 2)");
    assert_expr("!a == b", "(== (! a) b)");
}

#[test]
fn test_and_binds_tighter_than_or() {
    assert_expr("true and false or true", "(or (and true false) true)");
}

#[test]
fn test_comparison_binds_tighter_than_equality() {
    assert_expr("a < b == c > d", "(== (< a b) (> c d))");
}

#[test]
fn test_null_coalescing_is_left_associative() {
    assert_expr("a ?? b ?? c", "(?? (?? a b) c)");
}

#[test]
fn test_assignment_forms() {
    assert_parses("let x = 10; x = 20;", "(let (bind x) ? 10) (assign x 20)");
    assert_expr("a.f = 1", "(set a .f 1)");
    assert_expr("a?.f = 1", "(set? a .f 1)");
    assert_expr("a[0] = 1", "(set-index a 0 1)");
}

#[test]
fn test_compound_assignment_desugars_to_binary() {
    assert_parses(
        "let x = 1; x += 5;",
        "(let (bind x) ? 1) (assign x (+= x 5))",
    );
}

#[test]
fn test_call_with_labels() {
    assert_expr("f(1, a: 2)", "(call f 1 (a: 2))");
}

#[test]
fn test_postfix_chain() {
    assert_expr("f(1)(2).x[3]", "(index (get (call (call f 1) 2) .x) 3)");
}

#[test]
fn test_safe_navigation() {
    assert_expr("obj?.field", "(get? obj .field)");
    assert_expr("a?[0]", "(index? a 0)");
    assert_expr("a.b?.c!", "(unwrap (get? (get a .b) .c))");
}

#[test]
fn test_grouping_and_tuple() {
    assert_expr("(a)", "(group a)");
    assert_expr("(a, b)", "(tuple a b)");
}

#[test]
fn test_list_and_map_literals() {
    assert_expr("[]", "(list)");
    assert_expr("[1, 2, 3]", "(list 1 2 3)");
    assert_expr(
        r#"["a": 1, "b": 2]"#,
        r#"(map (entry "a" 1) (entry "b" 2))"#,
    );
}

#[test]
fn test_force_unwrap() {
    assert_expr("maybe!", "(unwrap maybe)");
}

#[test]
fn test_is_expression() {
    assert_expr("val is string", "(is val string)");
}

#[test]
fn test_string_interpolation() {
    assert_expr(r#""a${x}b""#, r#"(interp (text "a") x (text "b"))"#);
}

#[test]
fn test_lambda() {
    assert_expr(
        "|a: number, b|: number a + b",
        "(lambda (param a (type number)) (param b ?) (ret (type number)) (+ a b))",
    );
}

#[test]
fn test_if_and_match_expressions() {
    assert_expr(
        "if a { 1 } else { 2 }",
        "(if a (block (tail 1)) (block (tail 2)))",
    );
    assert_expr(
        "match v { .A => 1, E.B(x) => 2, other => 3 }",
        "(match v (arm (pat .A) 1) (arm (pat E.B (bind x)) 2) (arm (pat-var other) 3))",
    );
}

#[test]
fn test_block_expression_tail() {
    assert_expr("{ let a = 1; a }", "(block (let (bind a) ? 1) (tail a))");
}

#[test]
fn test_expression_spans() {
    assert_span("let x = 1 + 2 * 3;", "let/binary", "1 + 2 * 3");
    assert_span("let x = 1 + 2 * 3;", "let/binary/binary", "2 * 3");
    assert_span("let x = -a.b;", "let/unary", "-a.b");
    assert_span("let x = obj.field;", "let/get", "obj.field");
    assert_span("let x = (1, 2);", "let/tuple/num", "1");
}

#[test]
fn test_printer_shows_spans() {
    assert_parses_with_spans(
        "let x = 1 + 2;",
        "(let@4..13 (bind@4..5 x@4..5) ? (+@8..13 1@8..9 2@12..13))",
    );
}

#[test]
fn test_span_paths_select_direct_children() {
    // `[n]` picks the n-th matching sibling, `*` matches any kind, an operator matches by its printed head.
    assert_span("let x = f(1, 22, 3);", "let/call/num[1]", "22");
    assert_span("let x = f(1, 22, 3);", "let/call/*[2]", "22");
    assert_span("let x = a * b + c * d;", "let/+/*[1]", "c * d");
    assert_span("let x = a * b + c * d;", "let/binary/binary[1]", "c * d");
    // `num` is a grandchild of `let`, not a child.
    assert_no_node("let x = 1 + 2;", "let/num");
    assert_no_node("let x = f(1);", "let/call/num[1]");
}
