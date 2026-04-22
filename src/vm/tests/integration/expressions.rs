use crate::vm::tests::helpers::*;
use crate::vm::value::Value;

// ── Block expressions ─────────────────────────────────────────────────────────

#[test]
fn test_block_expr_tail_value() {
    assert_global("let a = { 1 + 2 };", 0, Value::Number(3.0));
}

#[test]
fn test_block_expr_with_statements() {
    assert_runs(
        r#"
        let a = {
            let x = 10;
            println(x);
            let y = 5;
            println(y);
            x + y
        };
        assert(a, 15);
        "#,
    );
}

#[test]
fn test_block_expr_void_no_tail() {
    // Block with no tail — evaluates to void (nil), assigned to an inferred var.
    assert_runs(
        r#"
        let _ = {
            let x = 1;
        };
        "#,
    );
}

#[test]
fn test_nested_block_expr() {
    assert_global(
        r#"
        let a = {
            let inner = { 3 * 4 };
            inner + 1
        };
        "#,
        0,
        Value::Number(13.0),
    );
}

// ── If expressions ────────────────────────────────────────────────────────────

#[test]
fn test_if_expr_true_branch() {
    assert_global(
        r#"
        let x = if true { 42 } else { 0 };
        "#,
        0,
        Value::Number(42.0),
    );
}

#[test]
fn test_if_expr_false_branch() {
    assert_global(
        r#"
        let x = if false { 42 } else { 0 };
        "#,
        0,
        Value::Number(0.0),
    );
}

#[test]
fn test_if_expr_condition() {
    assert_global(
        r#"
        let a = 10;
        let b = if a > 5 { 1 } else { 2 };
        "#,
        1,
        Value::Number(1.0),
    );
}

#[test]
fn test_if_expr_else_if_chain() {
    assert_global(
        r#"
        let score = 75;
        let grade = if score >= 90 { 4 } else if score >= 75 { 3 } else { 2 };
        "#,
        1,
        Value::Number(3.0),
    );
}

#[test]
fn test_if_expr_with_block_body() {
    assert_global(
        r#"
        let x = if true {
            let a = 3;
            let b = 4;
            a + b
        } else {
            0
        };
        "#,
        0,
        Value::Number(7.0),
    );
}

#[test]
fn test_if_expr_no_else_used_as_statement() {
    // if-expr without else, used as a statement (void result, discarded).
    assert_runs(
        r#"
        let x = 1;
        if x == 1 { x = 2; }
        "#,
    );
}

#[test]
fn test_if_expr_with_return() {
    assert_runs(
        r#"
        func local() {
            let x = if true {
                10
            } else {
                return;
            };
            println(x);
        }
        local();
        "#,
    );
}

// ── Match expressions ─────────────────────────────────────────────────────────

#[test]
fn test_match_expr_basic() {
    assert_global(
        r#"
        enum Color { Red, Green, Blue }
        let c = Color.Red;
        let name = match c {
            .Red => 1,
            .Green => 2,
            .Blue => 3,
        };
        "#,
        1,
        Value::Number(1.0),
    );
}

#[test]
fn test_match_expr_with_binding() {
    assert_global(
        r#"
        enum Opt { Some(number), None }
        let v = Opt.Some(99);
        let result = match v {
            .Some(x) => x + 1,
            .None => 0,
        };
        "#,
        1,
        Value::Number(100.0),
    );
}

#[test]
fn test_match_expr_fallthrough() {
    assert_global(
        r#"
        enum Color { Red, Green, Blue }
        let c = Color.Blue;
        let n = match c {
            .Red => 10,
            other => 99,
        };
        "#,
        1,
        Value::Number(99.0),
    );
}

#[test]
fn test_match_expr_block_body() {
    assert_global(
        r#"
        enum Dir { Left, Right }
        let d = Dir.Right;
        let x = match d {
            .Left => { 0 - 1 },
            .Right => { 0 + 1 },
        };
        "#,
        1,
        Value::Number(1.0),
    );
}

// ── Lambda expressions ────────────────────────────────────────────────────────

#[test]
fn test_lambda_basic() {
    assert_global(
        r#"
        let add = |x: number, y: number| x + y;
        let result = add(3, 4);
        "#,
        1,
        Value::Number(7.0),
    );
}

#[test]
fn test_lambda_no_params() {
    assert_global(
        r#"
        let greet = || 42;
        let result = greet();
        "#,
        1,
        Value::Number(42.0),
    );
}

#[test]
fn test_lambda_block_body() {
    assert_global(
        r#"
        let double = |x: number| {
            let two = 2;
            x * two
        };
        let result = double(5);
        "#,
        1,
        Value::Number(10.0),
    );
}

#[test]
fn test_lambda_closure_captures() {
    assert_global(
        r#"
        let base = 100;
        let add_base = |x: number| base + x;
        let result = add_base(5);
        "#,
        2,
        Value::Number(105.0),
    );
}

#[test]
fn test_lambda_passed_as_argument() {
    assert_global(
        r#"
        func apply(f: func(number): number, x: number): number {
            return f(x);
        }
        let result = apply(|x: number| x * 2, 7);
        "#,
        1,
        Value::Number(14.0),
    );
}

#[test]
fn test_lambda_inferred_param_type() {
    // Type inferred from context (expected function type).
    assert_global(
        r#"
        func apply(f: func(number): number, x: number): number {
            return f(x);
        }
        let result = apply(|x| x + 1, 9);
        "#,
        1,
        Value::Number(10.0),
    );
}
