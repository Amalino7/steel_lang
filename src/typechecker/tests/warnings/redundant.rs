use crate::typechecker::core::error::{TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::tests::helpers::*;

// Note: Safe access (?.) on non-optional values produces a TypeCheckerError
// (not a warning) for regular values. SafeAccessOnNonOptional warning is only
// emitted for Metatype (static/type-level) access.

#[test]
fn test_safe_access_on_non_optional_struct() {
    // Using ?. on a non-optional struct value is a type error
    assert_type_error(
        r#"
        struct Point { x: number }
        let p = Point(x: 5);
        let x = p?.x;
        "#,
        |e| matches!(e, TypeCheckerError::TypeMismatch { .. } | TypeCheckerError::ComplexTypeMismatch { .. }),
    );
}

#[test]
fn test_safe_access_on_non_optional_number() {
    // Using ?. on a non-optional number value is a type error
    assert_type_error(
        r#"
        let x: number = 5;
        let y = x?.something;
        "#,
        |e| matches!(e, TypeCheckerError::TypeMismatch { .. } | TypeCheckerError::ComplexTypeMismatch { .. }),
    );
}

#[test]
fn test_force_unwrap_on_non_optional() {
    assert_type_warning(
        r#"
        let x: number = 5;
        let y = x!;
        "#,
        |w| matches!(w, TypeCheckerWarning::RedundantForceUnwrap { .. }),
    );
}

#[test]
fn test_force_unwrap_on_non_optional_string() {
    assert_type_warning(
        r#"
        let s: string = "hello";
        let t = s!;
        "#,
        |w| matches!(w, TypeCheckerWarning::RedundantForceUnwrap { .. }),
    );
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
