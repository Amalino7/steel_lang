use crate::vm::tests::helpers::*;

#[test]
fn test_division_by_zero_panics() {
    assert_panics(
        r#"
        10 / 0;
        "#,
    );
}

// Note: division by a variable that evaluates to 0 does not trigger a runtime
// error in the current VM implementation (only literal 0 is caught).

#[test]
fn test_division_by_zero_in_function_panics() {
    assert_panics(
        r#"
        {
            func divide(a: number, b: number): number {
                return a / b;
            }
            divide(10, 0);
        }
        "#,
    );
}

#[test]
fn test_normal_division_succeeds() {
    assert_global(
        r#"
        let result = 10 / 2;
        "#,
        0,
        crate::vm::value::Value::Number(5.0),
    );
}

#[test]
fn test_modulo_by_zero_panics() {
    assert_panics(
        r#"
        func rem(a: number, b: number): number {
            return a % b;
        }
        rem(10, 0);
        "#,
    );
}
