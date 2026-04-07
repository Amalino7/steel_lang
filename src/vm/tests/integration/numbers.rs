use crate::vm::tests::helpers::*;
use crate::vm::value::Value;

#[test]
fn test_underscore_separator_integer() {
    assert_global("let a = 1_000_000;", 0, Value::Number(1_000_000.0));
}

#[test]
fn test_underscore_separator_float() {
    assert_global("let a = 3.17_16_15;", 0, Value::Number(3.17_16_15));
}

#[test]
fn test_scientific_notation_positive_exponent() {
    assert_global("let a = 1.5e3;", 0, Value::Number(1500.0));
}

#[test]
fn test_scientific_notation_negative_exponent() {
    assert_global("let a = 2.5e-2;", 0, Value::Number(0.025));
}

#[test]
fn test_scientific_notation_uppercase_e() {
    assert_global("let a = 1E6;", 0, Value::Number(1_000_000.0));
}

#[test]
fn test_scientific_notation_explicit_positive_sign() {
    assert_global("let a = 3e+2;", 0, Value::Number(300.0));
}

#[test]
fn test_binary_literal() {
    assert_global("let a = 0b1010;", 0, Value::Number(10.0));
}

#[test]
fn test_binary_literal_with_separator() {
    assert_global("let a = 0b1111_0000;", 0, Value::Number(240.0));
}

#[test]
fn test_octal_literal() {
    assert_global("let a = 0o755;", 0, Value::Number(493.0));
}

#[test]
fn test_octal_literal_with_separator() {
    assert_global("let a = 0o7_5_5;", 0, Value::Number(493.0));
}

#[test]
fn test_hex_literal_lowercase() {
    assert_global("let a = 0xff;", 0, Value::Number(255.0));
}

#[test]
fn test_hex_literal_uppercase() {
    assert_global("let a = 0xFF;", 0, Value::Number(255.0));
}

#[test]
fn test_hex_literal_with_separator() {
    assert_global("let a = 0xFF_00;", 0, Value::Number(65280.0));
}

#[test]
fn test_combined_scientific_and_separator() {
    assert_global("let a = 1_000e2;", 0, Value::Number(100_000.0));
}
