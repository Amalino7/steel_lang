use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_type_mismatch_in_let() {
    Tester::new("let a: number = true;")
        .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
        .run();
}

#[test]
fn test_type_mismatch_string_to_number() {
    Tester::new(r#"let a: number = "hello";"#)
        .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
        .run();
}

#[test]
fn test_binary_op_type_mismatch_add() {
    Tester::new(r#"let x = 5 + "hello";"#)
        .expect_error(|e| matches!(e, TypeCheckerError::InvalidOperandTypes { .. }))
        .run();
}

#[test]
fn test_binary_op_type_mismatch_subtract() {
    Tester::new(r#"let x = "hello" - 5;"#)
        .expect_error(|e| matches!(e, TypeCheckerError::InvalidOperandTypes { .. }))
        .run();
}

#[test]
fn test_binary_op_type_mismatch_modulo() {
    Tester::new(r#"let x = "hello" % 2;"#)
        .expect_error(|e| matches!(e, TypeCheckerError::InvalidOperandTypes { .. }))
        .run();
}

#[test]
fn test_binary_op_type_mismatch_power() {
    Tester::new(r#"let x = "hello" ** 2;"#)
        .expect_error(|e| matches!(e, TypeCheckerError::InvalidOperandTypes { .. }))
        .run();
}

#[test]
fn test_comparison_type_mismatch() {
    Tester::new(r#"let x = 5 > "hello";"#)
        .expect_error(|e| matches!(e, TypeCheckerError::InvalidOperandTypes { .. }))
        .run();
}

#[test]
fn test_if_condition_not_boolean() {
    Tester::new(r#"if 5 { }"#)
        .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
        .run();
}

#[test]
fn test_if_condition_string() {
    Tester::new(r#"if "hello" { }"#)
        .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
        .run();
}

#[test]
fn test_while_condition_not_boolean() {
    Tester::new(r#"while 42 { }"#)
        .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
        .run();
}

#[test]
fn test_assignment_type_mismatch() {
    Tester::new(
        r#"
        let x: number = 5;
        x = "string";
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
    .run();
}

#[test]
fn test_void_assignment() {
    assert_typechecks(
        r#"
        func noReturn(): void {
            return;
        }
        let x = noReturn();
        "#,
    );
}
