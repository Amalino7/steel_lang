use crate::typechecker::core::error::TypeCheckerWarning;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_safe_access_on_non_optional_struct() {
    Tester::new(
        r#"
        struct Point { x: number }
        let p = Point(x: 5);
        let x = p?.x;
        "#,
    )
    .expect_warning(|e| matches!(e, TypeCheckerWarning::SafeAccessOnNonOptional { .. }))
    .run();
}

#[test]
fn test_safe_access_on_non_optional_number() {
    use crate::typechecker::core::error::TypeCheckerError;
    Tester::new(
        r#"
        let x: number = 5;
        let y = x?.something;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod(_)))
    .expect_warning(|e| matches!(e, TypeCheckerWarning::SafeAccessOnNonOptional { .. }))
    .run();
}

#[test]
fn test_force_unwrap_on_non_optional() {
    Tester::new(
        r#"
        let x: number = 5;
        let y = x!;
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::RedundantForceUnwrap { .. }))
    .run();
}

#[test]
fn test_force_unwrap_on_non_optional_string() {
    Tester::new(
        r#"
        let s: string = "hello";
        let t = s!;
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::RedundantForceUnwrap { .. }))
    .run();
}

#[test]
fn test_safe_access_on_optional_no_warning() {
    // Safe access on an optional should NOT produce a warning
    assert_typechecks(
        r#"
        struct Point { x: number }
        let p: Point? = Point(x: 5);
        let x = p?.x;
        "#,
    );
}

#[test]
fn test_force_unwrap_on_optional_no_warning() {
    // Force unwrap on an optional should NOT produce a warning
    assert_typechecks(
        r#"
        let x: number? = 5;
        let y = x!;
        "#,
    );
}
