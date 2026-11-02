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
    .expect_error(|e| matches!(e, TypeCheckerError::InvalidTupleIndex { index: 3, .. }))
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

#[test]
fn test_invalid_tuple_index_in_assignment() {
    Tester::new(
        r#"
        let t = (1, 2);
        t.2 = 5;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::InvalidTupleIndex { index: 2, .. }))
    .run();
}

#[test]
fn test_tuple_index_on_struct() {
    const SRC: &str = "struct P { x: number }\nlet p = P(x: 1);\nlet y = p.0;";
    Tester::new(SRC)
        .expect_error(|e| {
            matches!(e, TypeCheckerError::TupleIndexOnNonTuple { span, .. }
                if SRC.rfind('0') == Some(span.start))
        })
        .run();
}

#[test]
fn test_tuple_index_on_type_name() {
    Tester::new("struct P { x: number }\nlet y = P.0;")
        .expect_error(|e| matches!(e, TypeCheckerError::TupleIndexOnNonTuple { .. }))
        .run();
}

#[test]
fn test_tuple_index_assignment_on_struct() {
    Tester::new("struct P { x: number }\nlet p = P(x: 1);\np.0 = 2;")
        .expect_error(|e| matches!(e, TypeCheckerError::TupleIndexOnNonTuple { .. }))
        .run();
}
