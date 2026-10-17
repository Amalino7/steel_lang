use crate::vm::tests::helpers::assert_panics;

#[test]
fn test_panic_builtin_causes_error() {
    assert_panics(
        r#"
        panic("custom error message");
        "#,
    );
}

#[test]
fn test_panic_in_function_causes_error() {
    assert_panics(
        r#"
        func fail(): void {
            panic("intentional panic");
        }
        fail();
        "#,
    );
}

#[test]
fn test_panic_in_nested_call_causes_error() {
    assert_panics(
        r#"
        func inner(): void {
            panic("deep panic");
        }
        func outer(): void {
            inner();
        }
        outer();
        "#,
    );
}
