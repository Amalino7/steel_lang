use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_undefined_variable() {
    assert_type_error(
        "let a = b;",
        |e| matches!(e, TypeCheckerError::UndefinedVariable { name, .. } if name == "b"),
    );
}

#[test]
fn test_undefined_variable_in_expression() {
    assert_type_error(
        "let a = 5 + undefined_var;",
        |e| matches!(e, TypeCheckerError::UndefinedVariable { name, .. } if name == "undefined_var"),
    );
}

#[test]
fn test_undefined_type() {
    assert_type_error(
        "let x: UnknownType = 5;",
        |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
    );
}

#[test]
fn test_undefined_type_in_function_param() {
    assert_type_error(
        r#"func foo(x: UnknownType): void {}"#,
        |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
    );
}

#[test]
fn test_undefined_type_in_function_return() {
    assert_type_error(
        r#"func foo(): UnknownType {}"#,
        |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
    );
}

#[test]
fn test_redeclaration_function() {
    assert_type_error(
        r#"
        func foo(): void {}
        func foo(): void {}
        "#,
        |e| matches!(e, TypeCheckerError::Redeclaration { name, .. } if name == "foo"),
    );
}

#[test]
fn test_self_type_outside_impl() {
    assert_type_error("let x: Self = 5;", |e| {
        matches!(e, TypeCheckerError::SelfOutsideOfImpl { .. })
    });
}

#[test]
fn test_self_value_outside_impl() {
    assert_type_error("let x = self;", |e| {
        matches!(e, TypeCheckerError::SelfOutsideOfImpl { .. })
    });
}

#[test]
fn test_self_in_function_outside_impl() {
    assert_type_error(
        r#"
        func foo() {
            let x = self;
        }
        "#,
        |e| matches!(e, TypeCheckerError::SelfOutsideOfImpl { .. }),
    );
}

#[test]
fn test_assignment_to_captured_variable() {
    assert_type_error(
        r#"{
            let local = 10;
            func foo(): number {
                local = 20;
                return local;
            }
        }
        "#,
        |e| matches!(e, TypeCheckerError::AssignmentToCapturedVariable { name, .. } if name == "local"),
    );
}
