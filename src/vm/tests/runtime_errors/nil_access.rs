use crate::vm::tests::helpers::*;

#[test]
fn test_force_unwrap_nil_panics() {
    assert_panics(
        r#"
        {
            let x: number? = nil;
            x!;
        }
        "#,
    );
}

#[test]
fn test_force_unwrap_nil_struct_panics() {
    assert_panics(
        r#"
        struct Point { x: number }
        {
            let p: Point? = nil;
            p!;
        }
        "#,
    );
}

#[test]
fn test_safe_access_on_nil_returns_nil() {
    // Safe access on nil should NOT panic - it returns nil
    assert_global(
        r#"
        struct Point { x: number }
        let p: Point? = nil;
        let result: number? = p?.x;
        "#,
        1,
        crate::vm::value::Value::Nil,
    );
}

#[test]
fn test_nil_coalesce_with_nil() {
    // Nil coalescing should NOT panic - it returns the default
    assert_global(
        r#"
        let x: number? = nil;
        let y = x ?? 42;
        "#,
        1,
        crate::vm::value::Value::Number(42.0),
    );
}

#[test]
fn test_force_unwrap_non_nil_succeeds() {
    // Force unwrap on a non-nil value should succeed
    assert_global(
        r#"
        let x: number? = 10;
        let y = x!;
        "#,
        1,
        crate::vm::value::Value::Number(10.0),
    );
}
