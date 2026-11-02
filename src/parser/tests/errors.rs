use super::helpers::*;

#[test]
fn test_error_missing_semicolon_at_end_of_file() {
    assert_parse_errors("let a = 10", &[("E1010", "1:11")]);
    assert_error_contains("let a = 10", "expected `;`");
}

#[test]
fn test_error_missing_right_paren() {
    assert_parse_errors("let a = (10 + 2; let b = 1;", &[("E1012", "1:16")]);
    assert_error_contains("let a = (10 + 2; let b = 1;", "expected `)` to close `(`");
}

#[test]
fn test_unclosed_block_wins_over_the_missing_semicolons_at_the_same_point() {
    // The `;` after `1` and after the `let` are reported at the same point but only follow from the `}`.
    assert_parse_errors("let v = { 1", &[("E1012", "1:12")]);
    assert_error_contains("let v = { 1", "expected `}` to close `{`");
}

#[test]
fn test_error_missing_right_brace() {
    assert_error_contains("func foo(): void { let x = 1;", "expected `}` to close `{`");
}

#[test]
fn test_error_invalid_expression() {
    assert_parse_errors("let x = }; let y = 1;", &[("E1010", "1:9")]);
    assert_error_contains("let x = }; let y = 1;", "expected an expression");
}

#[test]
fn test_error_invalid_assignment_target() {
    assert_error_contains("5 = 10; let y = 1;", "invalid assignment target");
}

#[test]
fn test_error_invalid_token() {
    assert_error_contains("let a = @; let b = 1;", "invalid character");
}

#[test]
fn test_multiple_errors_in_source() {
    assert_error_count("let a = 10\nlet b = 20\nlet c = 30;", 2);
}

#[test]
fn test_errors_inside_block() {
    let source = "
    func foo(): void {
        let x = 10
        let y = 20
        let z = 30;
    }";
    assert_error_count(source, 2);
}

#[test]
fn test_recovery_continues_after_block_error() {
    let source = "
    func foo(): void {
        let x = 10
    }
    let y = 42;
    let z = 1;";
    // Only the missing `;`, reported right after `10`: the declarations after the broken block parse cleanly.
    assert_parse_errors(source, &[("E1010", "3:19")]);
}

#[test]
fn test_recovery_reports_errors_in_later_declarations() {
    assert_error_count("let a = ; let b = 1; let c = ;", 2);
}

#[test]
fn test_public_in_block_is_error() {
    assert_error_count("func f(): void { public let x = 1; } let y = 1;", 1);
    assert_error_contains(
        "func f(): void { public let x = 1; }",
        "only top-level declarations can be public",
    );
}

#[test]
fn test_public_impl_is_error() {
    assert_error_count("struct P {} public impl P {} let y = 1;", 1);
}

#[test]
fn test_public_enum_variant_is_error() {
    assert_error_count("enum E { public A } let y = 1;", 1);
}

#[test]
fn test_public_interface_method_is_error() {
    assert_error_count("interface I { public func f(self): void; } let y = 1;", 1);
}

#[test]
fn test_bad_item_in_a_body_expects_a_method_not_a_token() {
    // `func`, `extern` and `public` can all start one, so no single token is named.
    assert_error_contains("impl P { let x = 1; }", "expected a method, found `let`");
    assert_error_contains("interface I { let x = 1; }", "expected a method signature");
}

// One test per error code that no other test asserts by code.

#[test]
fn test_e1001_unterminated_literal_starts_at_its_opener() {
    assert_parse_errors(r#"let s = "abc"#, &[("E1001", "1:9")]);
    assert_parse_errors("let s = 1; /* note", &[("E1001", "1:12")]);
}

#[test]
fn test_e1002_invalid_escape_points_at_the_escape() {
    assert_parse_errors(r#"let s = "ab\qc";"#, &[("E1002", "1:12")]);
    assert_parse_errors(r#"let s = "${a}x\u{zz}";"#, &[("E1002", "1:15")]);
    assert_span_of_error(r#"let s = "ab\qc";"#, r"\q");
    assert_span_of_error(r#"let s = "\u{110000}";"#, r"\u{110000}");
}

#[test]
fn test_e1020_public_where_not_allowed() {
    assert_parse_errors("enum E { public A }", &[("E1020", "1:10")]);
}

#[test]
fn test_e1025_shorthand_field_pattern_needs_a_name() {
    assert_parse_errors("let P(: (a, b)) = p;", &[("E1025", "1:9")]);
}

#[test]
fn test_e1040_import_after_a_declaration() {
    assert_parse_errors("let x = 1; import a/b;", &[("E1040", "1:12")]);
}

#[test]
fn test_e1041_empty_import_group() {
    assert_parse_errors("import a/{};", &[("E1041", "1:10")]);
}

#[test]
fn test_e1042_non_ascii_import_path() {
    assert_parse_errors("import a/œ/b;", &[("E1042", "1:10")]);
}
