use super::helpers::*;

#[test]
fn test_let_with_type() {
    assert_parses("let x: number = 42;", "(let (bind x) (type number) 42)");
}

#[test]
fn test_let_inferred() {
    assert_parses("let x = 42;", "(let (bind x) ? 42)");
}

#[test]
fn test_let_destructuring() {
    assert_parses(
        "let (a, b) = t;",
        "(let (bind-tuple (bind a) (bind b)) ? t)",
    );
    assert_parses(
        "let P(x: a, y: b) = t;",
        "(let (bind-struct P (bind-field x (bind a)) (bind-field y (bind b))) ? t)",
    );
}

#[test]
fn test_public_let() {
    assert_parses("public let z = 1;", "(let public (bind z) ? 1)");
}

#[test]
fn test_if_else_if_chain() {
    assert_parses(
        "if x > 0 { let y = 1; } else if z { y = 2; } else { y = 3; }",
        "(if (> x 0) (block (let (bind y) ? 1)) (if z (block (assign y 2)) (block (assign y 3))))",
    );
}

#[test]
fn test_while_statement() {
    assert_parses(
        "while x > 0 { x = x - 1; }",
        "(while (> x 0) (block-stmt (assign x (- x 1))))",
    );
}

#[test]
fn test_return_statement() {
    assert_parses(
        "func foo(): void { return; }",
        "(func foo (ret (type void)) (block-stmt (return (void))))",
    );
    assert_parses(
        "func foo(): number { return 1; }",
        "(func foo (ret (type number)) (block-stmt (return 1)))",
    );
}

#[test]
fn test_block_statement() {
    assert_parses(
        "{ let x = 1; let y = 2; }",
        "(block (let (bind x) ? 1) (let (bind y) ? 2))",
    );
}

#[test]
fn test_match_statement() {
    assert_parses(
        "match x { .Some(val) => { let y = val; } .None => { let y = 0; } }",
        "(match x \
         (arm (pat .Some (bind val)) (block (let (bind y) ? val))) \
         (arm (pat .None) (block (let (bind y) ? 0))))",
    );
}

#[test]
fn test_else_if_with_parenthesised_conditions() {
    assert_parses_ok(
        r#"
        if (a > 0) {
            println("a is greater than 0");
        } else if (a < 0) {
            println("a is less than 0");
        } else {
            println("a is equal to 0");
        }"#,
    );
}

#[test]
fn test_statement_spans() {
    assert_span("let x = 1; let y = 2;", "let/num", "1");
    assert_span("let x = 1; let y = 2;", "let[1]/num", "2");
    assert_span("while a { b = 1; } let y = 2;", "while/var", "a");
}
