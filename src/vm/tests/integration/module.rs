use crate::vm::tests::helpers::assert_runs;

#[test]
fn test_map_literal_and_get() {
    assert_runs(
        r#"
        import random/garbage/{pne, two};
        "#,
    );
}
