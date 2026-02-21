use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_type_mismatch_in_let() {
    Tester::new("let a: number = true;")
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. } | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
        .run();
}

#[test]
fn test_type_mismatch_string_to_number() {
    Tester::new(r#"let a: number = "hello";"#)
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
        .run();
}

#[test]
fn test_binary_op_type_mismatch_add() {
    Tester::new(r#"let x = 5 + "hello";"#)
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
        .run();
}

#[test]
fn test_binary_op_type_mismatch_subtract() {
    Tester::new(r#"let x = "hello" - 5;"#)
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
        .run();
}

#[test]
fn test_comparison_type_mismatch() {
    Tester::new(r#"let x = 5 > "hello";"#)
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
        .run();
}

#[test]
fn test_if_condition_not_boolean() {
    Tester::new(r#"if 5 { }"#)
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
        .run();
}

#[test]
fn test_if_condition_string() {
    Tester::new(r#"if "hello" { }"#)
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
        .run();
}

#[test]
fn test_while_condition_not_boolean() {
    Tester::new(r#"while 42 { }"#)
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        })
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
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::TypeMismatch { .. }
                | TypeCheckerError::ComplexTypeMismatch { .. }
        )
    })
    .run();
}

#[test]
fn test_void_assignment() {
    // This test checks if assigning void is allowed
    // Based on test_assign_void in old tests, this should succeed
    assert_typechecks(
        r#"
        func noReturn(): void {
            return;
        }
        let x = noReturn();
        "#,
    );
}
