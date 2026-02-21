use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_uncovered_pattern_simple() {
    assert_type_error(
        r#"
        enum Bool { True, False }
        let x = Bool.True;
        match x {
            Bool.True => { 1; }
        }
        "#,
        |e| matches!(e, TypeCheckerError::UncoveredPattern { variant, .. } if variant == "False"),
    );
}

#[test]
fn test_uncovered_pattern_multiple_missing() {
    assert_type_error(
        r#"
        enum Color { Red, Green, Blue }
        let c = Color.Red;
        match c {
            Color.Red => { 1; }
        }
        "#,
        |e| matches!(e, TypeCheckerError::UncoveredPattern { .. }),
    );
}

#[test]
fn test_uncovered_pattern_with_data() {
    assert_type_error(
        r#"
        enum Option<T> { Some(T), None }
        let x: Option<number> = Option.Some(5);
        match x {
            Option.Some(v) => { v; }
        }
        "#,
        |e| matches!(e, TypeCheckerError::UncoveredPattern { variant, .. } if variant == "None"),
    );
}

#[test]
fn test_unreachable_pattern_duplicate() {
    assert_type_error(
        r#"
        enum Bool { True, False }
        let x = Bool.True;
        match x {
            Bool.True => { 1; }
            Bool.True => { 2; }
            Bool.False => { 3; }
        }
        "#,
        |e| matches!(e, TypeCheckerError::UnreachablePattern { .. }),
    );
}

#[test]
fn test_invalid_is_usage_on_number() {
    // 'is' is only valid on enum types - using it on a number is an error
    assert_type_error(
        r#"
        enum Color { Red }
        let x = 5;
        if x is Red { }
        "#,
        |e| matches!(e, TypeCheckerError::InvalidIsUsage { .. }),
    );
}

#[test]
fn test_invalid_is_usage_on_string() {
    // 'is' is only valid on enum types - using it on a string is an error
    assert_type_error(
        r#"
        enum Color { Red }
        let s = "hello";
        if s is Red { }
        "#,
        |e| matches!(e, TypeCheckerError::InvalidIsUsage { .. }),
    );
}

#[test]
fn test_valid_is_usage_on_enum() {
    // This should succeed - 'is' is valid on enums
    assert_typechecks(
        r#"
        enum Option<T> { Some(T), None }
        let x: Option<number> = Option.None;
        if x is None {
            let y = 1;
        }
        "#,
    );
}
