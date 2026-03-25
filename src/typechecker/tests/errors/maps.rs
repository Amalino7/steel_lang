use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_map_wrong_key_type() {
    Tester::new(r#"let m = ["a": 1, 2: 3];"#)
        .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
        .run();
}

#[test]
fn test_map_wrong_value_type() {
    Tester::new(r#"let m = ["a": 1, "b": true];"#)
        .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
        .run();
}

#[test]
fn test_map_index_wrong_key_type() {
    Tester::new(
        r#"
        let m: Map<string, number> = ["a": 1];
        let v = m[42];
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
    .run();
}

#[test]
fn test_map_set_index_wrong_value_type() {
    Tester::new(
        r#"
        let m: Map<string, number> = ["a": 1];
        m["b"] = true;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
    .run();
}

#[test]
fn test_map_annotation_mismatch() {
    Tester::new(
        r#"
        let m: Map<string, number> = ["a": true];
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
    .run();
}
