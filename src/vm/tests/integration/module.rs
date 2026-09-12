use crate::vm::tests::helpers::assert_runs;

#[test]
fn test_basic_module() {
    assert_runs(
        r#"
        import random/garbage/{pne, two}; // Should parse for now
        "#,
    );
}
