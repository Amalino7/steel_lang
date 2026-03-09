use crate::typechecker::core::error::{BindingError, TypeCheckerError};
use crate::typechecker::tests::helpers::*;

#[test]
fn test_undefined_variable() {
    Tester::new("let a = b;")
        .expect_error(
            |e| matches!(e, TypeCheckerError::UndefinedVariable { name, .. } if name == "b"),
        )
        .run();
}

#[test]
fn test_undefined_variable_in_expression() {
    Tester::new("let a = 5 + undefined_var;")
        .expect_error(|e| {
            matches!(e, TypeCheckerError::UndefinedVariable { name, .. } if name == "undefined_var")
        })
        .run();
}

#[test]
fn test_undefined_type() {
    Tester::new("let x: UnknownType = 5;")
        .expect_error(
            |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
        )
        .run();
}

#[test]
fn test_undefined_type_in_function_param() {
    // The typechecker resolves the unknown type twice (forward declaration + body check)
    Tester::new(r#"func foo(x: UnknownType): void {}"#)
        .expect_error(
            |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
        )
        .expect_error(
            |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
        )
        .run();
}

#[test]
fn test_undefined_type_in_function_return() {
    // The typechecker resolves the unknown type twice (forward declaration + body check)
    Tester::new(r#"func foo(): UnknownType {}"#)
        .expect_error(
            |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
        )
        .expect_error(
            |e| matches!(e, TypeCheckerError::UndefinedType { name, .. } if name == "UnknownType"),
        )
        .run();
}

#[test]
fn test_redeclaration_function() {
    Tester::new(
        r#"
        func foo(): void {}
        func foo(): void {}
        "#,
    )
    .expect_error(
        |e| matches!(e, TypeCheckerError::Binding(BindingError::Redeclaration { name, .. }) if name == "foo"),
    )
    .run();
}

#[test]
fn test_self_type_outside_impl() {
    Tester::new("let x: Self = 5;")
        .expect_error(|e| matches!(e, TypeCheckerError::SelfOutsideOfImpl { .. }))
        .run();
}

#[test]
fn test_self_value_outside_impl() {
    Tester::new("let x = self;")
        .expect_error(|e| matches!(e, TypeCheckerError::SelfOutsideOfImpl { .. }))
        .run();
}

#[test]
fn test_self_in_function_outside_impl() {
    Tester::new(
        r#"
        func foo() {
            let _x = self;
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::SelfOutsideOfImpl { .. }))
    .run();
}

#[test]
fn test_assignment_to_captured_variable() {
    Tester::new(
        r#"{
            let local = 10;
            func foo(): number {
                local = 20;
                return local;
            }
        }
        "#,
    )
    .expect_error(|e| {
        matches!(e, TypeCheckerError::Binding(BindingError::Captured { name, .. }) if name == "local")
    })
    .run();
}
