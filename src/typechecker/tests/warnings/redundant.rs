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

/// The source text of a `SafeAccessOnNonOptional` warning, or `None` for any other warning.
fn safe_access_text<'a>(src: &'a str, warning: &TypeCheckerWarning) -> Option<&'a str> {
    match warning {
        TypeCheckerWarning::SafeAccessOnNonOptional { span } => Some(&src[span.to_range()]),
        _ => None,
    }
}

#[test]
fn test_safe_field_access_warning_points_at_the_operator() {
    const SRC: &str = "struct Point { x: number }\nlet p = Point(x: 5);\nlet x = p?.x;";
    Tester::new(SRC)
        .expect_warning(|w| safe_access_text(SRC, w) == Some("?."))
        .run();
}

#[test]
fn test_safe_index_warning_points_at_the_operator() {
    const SRC: &str = "let a = [1, 2];\nlet x = a?[0];";
    Tester::new(SRC)
        .expect_warning(|w| safe_access_text(SRC, w) == Some("?"))
        .run();
}

#[test]
fn test_safe_call_warning_points_at_the_operator() {
    const SRC: &str = "func f(): number { return 1; }\nlet x = f?();";
    Tester::new(SRC)
        .expect_warning(|w| safe_access_text(SRC, w) == Some("?("))
        .run();
}

#[test]
fn test_safe_call_of_a_method_warns() {
    // The method value itself is never optional, so the `?(` has no effect.
    const SRC: &str = "struct P { x: number }\nimpl P { func get(self): number { return self.x; } }\nlet p = P(x: 1);\nlet x = p.get?();";
    Tester::new(SRC)
        .expect_warning(|w| safe_access_text(SRC, w) == Some("?("))
        .run();
}

#[test]
fn test_safe_access_on_type_name_points_at_the_operator() {
    const SRC: &str = "enum E { A }\nlet x = E?.A;";
    Tester::new(SRC)
        .expect_warning(|w| safe_access_text(SRC, w) == Some("?."))
        .run();
}
