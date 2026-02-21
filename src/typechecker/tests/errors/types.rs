use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_type_mismatch_in_let() {
    assert_type_error("let a: number = true;", |e| {
        matches!(
            e,
            TypeCheckerError::TypeMismatch { .. } | TypeCheckerError::ComplexTypeMismatch { .. }
        )
    });
}

#[test]
fn test_type_mismatch_string_to_number() {
    assert_type_error(
        r#"let a: number = "hello";"#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
}

#[test]
fn test_binary_op_type_mismatch_add() {
    assert_type_error(
        r#"let x = 5 + "hello";"#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
}

#[test]
fn test_binary_op_type_mismatch_subtract() {
    assert_type_error(
        r#"let x = "hello" - 5;"#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
}

#[test]
fn test_comparison_type_mismatch() {
    assert_type_error(
        r#"let x = 5 > "hello";"#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
}

#[test]
fn test_if_condition_not_boolean() {
    assert_type_error(
        r#"if 5 { }"#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
}

#[test]
fn test_if_condition_string() {
    assert_type_error(
        r#"if "hello" { }"#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
}

#[test]
fn test_while_condition_not_boolean() {
    assert_type_error(
        r#"while 42 { }"#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
}

#[test]
fn test_assignment_type_mismatch() {
    assert_type_error(
        r#"
        let x: number = 5;
        x = "string";
        "#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
            )
        },
    );
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
