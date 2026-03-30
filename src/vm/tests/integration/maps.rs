use crate::vm::tests::helpers::*;
use crate::vm::value::Value;

#[test]
fn test_map_literal_and_get() {
    assert_runs(
        r#"
        let m: Map<string, number> = ["a": 1, "b": 2];
        assert(m["a"]!, 1);
        assert(m["b"]!, 2);
        "#,
    );
}

#[test]
fn test_map_get_missing_key_returns_nil() {
    assert_global(
        r#"
        let m: Map<string, number> = ["a": 1];
        let v = m["missing"];
        "#,
        1,
        Value::Nil,
    );
}

#[test]
fn test_map_set_existing_key() {
    assert_runs(
        r#"
        let m: Map<string, number> = ["x": 10];
        m["x"] = 99;
        assert(m["x"]!, 99);
        "#,
    );
}

#[test]
fn test_map_set_new_key() {
    assert_runs(
        r#"
        let m: Map<string, number> = [];
        m["new"] = 42;
        assert(m["new"]!, 42);
        "#,
    );
}

#[test]
fn test_map_len() {
    assert_runs(
        r#"
        let m: Map<string, number> = ["a": 1, "b": 2, "c": 3];
        assert(m.len(), 3);
        "#,
    );
}

#[test]
fn test_map_len_empty() {
    assert_runs(
        r#"
        let m: Map<string, number> = [];
        assert(m.len(), 0);
        "#,
    );
}

#[test]
fn test_map_contains_key_true() {
    assert_runs(
        r#"
        let m: Map<string, number> = ["a": 1];
        assert(m.contains_key("a"), true);
        "#,
    );
}

#[test]
fn test_map_contains_key_false() {
    assert_runs(
        r#"
        let m: Map<string, number> = ["a": 1];
        assert(m.contains_key("z"), false);
        "#,
    );
}

#[test]
fn test_map_remove_existing() {
    assert_runs(
        r#"
        let m: Map<string, number> = ["a": 1, "b": 2];
        let removed = m.remove("a");
        assert(removed!, 1);
        assert(m.len(), 1);
        assert(m.contains_key("a"), false);
        "#,
    );
}

#[test]
fn test_map_remove_missing_returns_nil() {
    assert_runs(
        r#"
        let m: Map<string, number> = ["a": 1];
        let removed = m.remove("z");
        assert(removed == nil, true);
        assert(m.len(), 1);
        "#,
    );
}

#[test]
fn test_map_number_keys() {
    assert_runs(
        r#"
        let m: Map<number, string> = [1: "one", 2: "two"];
        assert(m[1]!, "one");
        assert(m[2]!, "two");
        "#,
    );
}

#[test]
fn test_map_nan_key_panics_on_get() {
    assert_panics(
        r#"
        let m: Map<number, string> = [];
        let nan = (-10) ** 0.5;
        let _ = m[nan];
        "#,
    );
}

#[test]
fn test_map_nan_key_panics_on_set() {
    assert_panics(
        r#"
        let m: Map<number, string> = [];
        let nan = (-10) ** 0.5;
        m[nan] = "bad";
        "#,
    );
}

#[test]
fn test_map_nan_key_panics_on_contains_key() {
    assert_panics_with_prelude(
        r#"
        let m: Map<number, string> = [];
        let nan = (-10) ** 0.5;
        m.contains_key(nan);
        "#,
    );
}

#[test]
fn test_map_safe_get_on_nil_map() {
    assert_global(
        r#"
        let m: Map<string, number>? = nil;
        let v = m?["a"];
        "#,
        1,
        Value::Nil,
    );
}

#[test]
fn test_map_safe_get_on_some_map_found() {
    assert_runs(
        r#"
        let m: Map<string, number>? = ["a": 1];
        let v = m?["a"];
        assert(v!, 1);
        "#,
    );
}

#[test]
fn test_map_safe_get_on_some_map_missing_key() {
    assert_global(
        r#"
        let m: Map<string, number>? = ["a": 1];
        let v = m?["missing"];
        "#,
        1,
        Value::Nil,
    );
}

#[test]
fn test_map_safe_set_on_nil_map() {
    assert_global(
        r#"
        let m: Map<string, number>? = nil;
        m?["key"] = 42;
        "#,
        0,
        Value::Nil,
    );
}

#[test]
fn test_map_safe_set_on_some_map() {
    assert_runs(
        r#"
        let m: Map<string, number>? = ["a": 1];
        m?["b"] = 2;
        let inner = m!;
        assert(inner["b"]!, 2);
        "#,
    );
}

#[test]
fn test_map_set_result_is_value() {
    assert_global(
        r#"
        let m: Map<string, number> = [];
        let result = (m["k"] = 99);
        "#,
        1,
        Value::Number(99.0),
    );
}

#[test]
fn test_0_hashmap_equality() {
    assert_runs(
        r#"
        let m = [0: "Hello"];
        assert(m[-0], "Hello");
        assert(m[-0], m[0]);
        "#,
    );
}
