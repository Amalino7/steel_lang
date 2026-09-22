use crate::vm::tests::helpers::TestBuilder;

#[test]
fn test_basic_module() {
    TestBuilder::new(
        "main",
        r#"
        import random/garbage/{one, two};
        assert(one + two, 30);
        "#,
    )
    .with_module(
        "random.garbage",
        r#"
        let one = 10;
        let two = 20;
        "#,
    )
    .assert_runs()
}
