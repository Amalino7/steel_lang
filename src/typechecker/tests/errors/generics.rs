use crate::typechecker::core::error::{GenericError, TypeCheckerError};
use crate::typechecker::tests::helpers::*;

#[test]
fn test_generic_type_inference_struct() {
    // This should succeed - testing that inference works
    assert_typechecks(
        r#"
        struct Box<T> { value: T }
        let b = Box(value: 5);
        "#,
    );
}

#[test]
fn test_generic_type_explicit_struct() {
    // This should succeed - testing explicit type annotation via Steel's .<T> syntax
    assert_typechecks(
        r#"
        struct Box<T> { value: T }
        let b = Box.<number>(value: 10);
        "#,
    );
}
#[test]
fn test_cannot_infer_fn_type() {
    Tester::new(
        r#"
        func fake_identity<T,U>(x: T): T { return x; }
        let x = fake_identity(1);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::CannotInfer { .. })
        )
    })
    .run();
}
#[test]
fn test_cannot_infer_method() {
    Tester::new(
        r#"
        struct Box<T,U> { value: T }
        impl<T,U> Box<T,U> {
            func new(a: T): Self {
                return Box(value: a);
            }
        }
        let b = Box.new(1);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::CannotInfer { .. })
        )
    })
    .run();
}
#[test]
fn test_cannot_infer() {
    Tester::new(
        r#"
        struct Box<T,U> { value: T }
        let b = Box(value: 5);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::CannotInfer { .. })
        )
    })
    .run();
}

#[test]
fn test_wrong_specialization() {
    Tester::new(
        r#"
        struct Box<T> { top: T }
        impl<T> Box<T> {
            func new(top: T): Box<T> {
                return Box(top);
            }
        }

        let box = Box.<number>.new(10);
        let box2 = Box.<number>(top: 10);
        let box3 = Box.new.<number>(top: 10); // This should fail, generic is on number
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::InvalidSpecification { .. })
        )
    })
    .run();
}

#[test]
fn test_generic_count_mismatch_too_many() {
    Tester::new(
        r#"
        struct Box<T> { value: T }
        let b: Box<number, string> = Box(value: 5);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::CountMismatch {
                found: 2,
                expected: 1,
                ..
            })
        )
    })
    .run();
}

#[test]
fn test_generic_count_mismatch_too_few() {
    Tester::new(
        r#"
        struct Pair<T, U> { first: T, second: U }
        let p: Pair<number> = Pair(first: 1, second: 2);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::CountMismatch {
                found: 1,
                expected: 2,
                ..
            })
        )
    })
    .run();
}

#[test]
fn test_generic_count_mismatch_zero_given() {
    Tester::new(
        r#"
        struct Box<T> { value: T }
        let b: Box = Box(value: 5);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::CountMismatch {
                found: 0,
                expected: 1,
                ..
            })
        )
    })
    .run();
}

#[test]
fn test_generic_mismatch() {
    Tester::new(
        r#"
        struct Box<T> { value: T }
        let b: Box<string> = Box(value: 5);
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
    .run();
}

#[test]
fn test_generic_mismatch2() {
    Tester::new(
        r#"
        struct Box<T> { value: T }
        let b = Box.<string>(value: 5);
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
    .run();
}

#[test]
fn test_generic_function_explicit_type() {
    // This should succeed - explicit type via Steel's .<T>() syntax
    assert_typechecks(
        r#"
        func identity<T>(x: T): T { return x; }
        let x = identity.<number>(5);
        "#,
    );
}

#[test]
fn test_generic_function_inferred_type() {
    // This should succeed - type can be inferred from argument
    assert_typechecks(
        r#"
        func identity<T>(x: T): T { return x; }
        let x = identity(5);
        "#,
    );
}

#[test]
fn test_invalid_generic_specification() {
    Tester::new(
        r#"
        struct Box<T> { value: T }
        let b = Box.<number, string>(value: 5);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::InvalidSpecification { .. })
        )
    })
    .run();
}

#[test]
fn test_invalid_generic_specialization() {
    Tester::new(
        r#"
        struct Vec3 { x: number, y: number, z: number }
        let b = Vec3.<number>(1,2,3);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Generic(GenericError::InvalidSpecification { .. })
        )
    })
    .run();
}
