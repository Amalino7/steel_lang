use super::helpers::*;

#[test]
fn test_error_missing_semicolon_at_end_of_file() {
    assert_parse_errors("let a = 10", &["1:11"]);
    assert_error_contains("let a = 10", "Missing ';'");
}

#[test]
fn test_error_missing_right_paren() {
    assert_parse_errors("let a = (10 + 2; let b = 1;", &["1:16"]);
    assert_error_contains("let a = (10 + 2; let b = 1;", "Missing ')'");
}

#[test]
fn test_error_missing_right_brace() {
    assert_error_contains("func foo(): void { let x = 1;", "Missing '}'");
}

#[test]
fn test_error_invalid_expression() {
    assert_parse_errors("let x = }; let y = 1;", &["1:9"]);
    assert_error_contains("let x = }; let y = 1;", "Expected expression");
}

#[test]
fn test_error_invalid_assignment_target() {
    assert_error_contains("5 = 10; let y = 1;", "Invalid assignment target");
}

#[test]
fn test_error_invalid_token() {
    assert_error_contains("let a = @; let b = 1;", "Invalid token");
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
    assert_parse_errors(source, &["3:19"]);
}

#[test]
fn test_recovery_reports_errors_in_later_declarations() {
    assert_error_count("let a = ; let b = 1; let c = ;", 2);
}

#[test]
fn test_public_in_block_is_error() {
    assert_error_count("func f(): void { public let x = 1; } let y = 1;", 1);
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
