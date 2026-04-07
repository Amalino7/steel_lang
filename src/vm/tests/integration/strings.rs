use crate::vm::tests::helpers::*;

// ── Task 1: unknown escapes are parser errors ─────────────────────────────────

#[test]
fn test_unknown_escape_is_error() {
    assert_parse_fails(r#"let s = "\q";"#);
}

#[test]
fn test_unknown_escape_p_is_error() {
    assert_parse_fails(r#"let s = "\p";"#);
}

#[test]
fn test_trailing_backslash_is_error() {
    // A lone `\` at the end of a string should also be rejected.
    // (The scanner skips past EOF but process_escapes catches it.)
    assert_parse_fails("let s = \"test\\\";");
}

// ── Task 1: escape sequences ──────────────────────────────────────────────────

#[test]
fn test_escape_newline() {
    assert_global_string(r#"let s = "hello\nworld";"#, 0, "hello\nworld");
}

#[test]
fn test_escape_tab() {
    assert_global_string(r#"let s = "col1\tcol2";"#, 0, "col1\tcol2");
}

#[test]
fn test_escape_carriage_return() {
    assert_global_string(r#"let s = "a\rb";"#, 0, "a\rb");
}

#[test]
fn test_escape_backslash() {
    assert_global_string(r#"let s = "a\\b";"#, 0, r"a\b");
}

#[test]
fn test_escape_double_quote() {
    assert_global_string(r#"let s = "say \"hi\"";"#, 0, r#"say "hi""#);
}

#[test]
fn test_escape_null() {
    let mut expected = String::from("a");
    expected.push('\0');
    expected.push('b');
    assert_global_string(r#"let s = "a\0b";"#, 0, &expected);
}

#[test]
fn test_escape_dollar_sign() {
    assert_global_string(r#"let s = "price: \$5";"#, 0, "price: $5");
}

#[test]
fn test_escape_unicode_basic() {
    assert_global_string(r#"let s = "\u{41}";"#, 0, "A");
}

#[test]
fn test_escape_unicode_emoji() {
    assert_global_string(r#"let s = "\u{1F600}";"#, 0, "\u{1F600}");
}

#[test]
fn test_escape_unicode_multi() {
    assert_global_string(r#"let s = "\u{48}\u{69}";"#, 0, "Hi");
}

// ── Task 2: string interpolation ─────────────────────────────────────────────

#[test]
fn test_interp_simple_variable() {
    assert_global_string(
        r#"let name = "world"; let s = "hello ${name}";"#,
        1,
        "hello world",
    );
}

#[test]
fn test_interp_number() {
    assert_global_string(r#"let n = 42; let s = "n = ${n}";"#, 1, "n = 42");
}

#[test]
fn test_interp_boolean() {
    assert_global_string(r#"let b = true; let s = "ok: ${b}";"#, 1, "ok: true");
}

#[test]
fn test_interp_expression() {
    assert_global_string(r#"let s = "sum: ${1 + 2}";"#, 0, "sum: 3");
}

#[test]
fn test_interp_multiple() {
    assert_global_string(
        r#"let a = "foo"; let b = "bar"; let s = "${a} and ${b}";"#,
        2,
        "foo and bar",
    );
}

#[test]
fn test_interp_empty_literal_parts() {
    assert_global_string(r#"let x = 7; let s = "${x}";"#, 1, "7");
}

#[test]
fn test_interp_with_escapes() {
    assert_global_string(
        r#"let name = "Ada"; let s = "Hello,\t${name}!\n";"#,
        1,
        "Hello,\tAda!\n",
    );
}

#[test]
fn test_interp_escaped_dollar_not_interpolated() {
    assert_global_string(r#"let s = "cost: \$${5 + 5}";"#, 0, "cost: $10");
}

// ── Task 3: raw strings ───────────────────────────────────────────────────────

#[test]
fn test_raw_string_basic() {
    assert_global_string(r#"let s = """hello world""";"#, 0, "hello world");
}

#[test]
fn test_raw_string_no_escape_processing() {
    // Backslash sequences are NOT processed in raw strings.
    assert_global_string(
        r#"let s = """\n is not a newline""";"#,
        0,
        r"\n is not a newline",
    );
}

#[test]
fn test_raw_string_no_interpolation() {
    // `${expr}` is NOT interpolated in raw strings.
    assert_global_string(r#"let s = """value: ${1+2}""";"#, 0, "value: ${1+2}");
}

// ── Task 4: aligned raw strings (Swift-style indentation) ────────────────────

#[test]
fn test_raw_string_aligned_basic() {
    let src = r#"let s = """
        hello
        world
        """;"#;
    assert_global_string(src, 0, "hello\nworld");
}

#[test]
fn test_raw_string_aligned_preserves_inner_indent() {
    let src = "let s = \"\"\"\n        outer\n            inner\n        \"\"\";";
    assert_global_string(src, 0, "outer\n    inner");
}

#[test]
fn test_raw_string_no_final_newline_not_stripped() {
    // Single-line `"""content"""` — no alignment stripping.
    assert_global_string(r#"let s = """   spaces   """;"#, 0, "   spaces   ");
}
