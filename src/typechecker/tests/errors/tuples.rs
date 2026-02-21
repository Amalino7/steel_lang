use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_invalid_tuple_index_out_of_bounds() {
    assert_type_error(
        r#"
        let t = (1, 2, 3);
        let x = t.3;
        "#,
        |e| matches!(e, TypeCheckerError::TypeHasNoFields { .. }),
    );
}

#[test]
fn test_invalid_tuple_index_non_numeric() {
    assert_type_error(
        r#"
        let t = (1, 2);
        let x = t.foo;
        "#,
        |e| matches!(e, TypeCheckerError::TypeHasNoFields { .. }),
    );
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
