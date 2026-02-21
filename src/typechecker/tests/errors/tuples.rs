use crate::typechecker::core::error::TypeCheckerError;
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
