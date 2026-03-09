use crate::typechecker::core::error::{BindingError, TypeCheckerError};
use crate::typechecker::scope::variables::DeclarationKind;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_invalid_tuple_index_out_of_bounds() {
    Tester::new(
        r#"
        let t = (1, 2, 3);
        let x = t.3;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeHasNoFields { .. }))
    .run();
}

#[test]
fn test_invalid_tuple_index_non_numeric() {
    Tester::new(
        r#"
        let t = (1, 2);
        let x = t.foo;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeHasNoFields { .. }))
    .run();
}

#[test]
fn test_valid_tuple_index() {
    assert_typechecks(
        r#"
        let t = (1, "hello", true);
        let x = t.0;
        let y = t.1;
        let z = t.2;
        "#,
    );
}

#[test]
fn test_destructured_tuple_binding_not_reassignable() {
    Tester::new(
        r#"
        let (a, b) = (1, 2);
        a = 10;
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Binding(BindingError::Immutable {
                kind: DeclarationKind::Binding,
                ..
            })
        )
    })
    .run();
}

#[test]
fn test_destructured_tuple_second_binding_not_reassignable() {
    Tester::new(
        r#"
        let (a, b) = (1, 2);
        b = 10;
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Binding(BindingError::Immutable {
                kind: DeclarationKind::Binding,
                ..
            })
        )
    })
    .run();
}

#[test]
fn test_destructured_tuple_bindings_readable() {
    assert_typechecks(
        r#"
        let (a, b) = (1, 2);
        let sum = a + b;
        "#,
    );
}

#[test]
fn test_nested_destructured_tuple_binding_not_reassignable() {
    Tester::new(
        r#"
        let ((a, b), c) = ((1, 2), 3);
        a = 10;
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Binding(BindingError::Immutable {
                kind: DeclarationKind::Binding,
                ..
            })
        )
    })
    .run();
}
