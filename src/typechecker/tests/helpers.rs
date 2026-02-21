use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::typechecker::core::error::{TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::TypeChecker;

/// Test that code type-checks successfully
pub fn assert_typechecks(source: &str) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let mut ast = parser.parse().expect("Parser failed");
    let mut checker = TypeChecker::new();
    checker
        .check(ast.as_mut_slice())
        .expect("Type checker should succeed");
}

/// Test that code produces a specific error
pub fn assert_type_error(source: &str, error_matcher: impl Fn(&TypeCheckerError) -> bool) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let mut ast = parser.parse().expect("Parser failed");
    let mut checker = TypeChecker::new();

    match checker.check(ast.as_mut_slice()) {
        Err(errors) => {
            assert!(
                errors.iter().any(&error_matcher),
                "Expected error not found. Got: {:#?}",
                errors
            );
        }
        Ok(_) => panic!("Expected type error but code type-checked successfully"),
    }
}

/// Test that code produces a specific warning
pub fn assert_type_warning(source: &str, warning_matcher: impl Fn(&TypeCheckerWarning) -> bool) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let mut ast = parser.parse().expect("Parser failed");
    let mut checker = TypeChecker::new();

    match checker.check(ast.as_mut_slice()) {
        Ok((_, warnings)) => {
            assert!(
                warnings.iter().any(&warning_matcher),
                "Expected warning not found. Got: {:#?}",
                warnings
            );
        }
        Err(errors) => panic!("Expected warning but got errors: {:#?}", errors),
    }
}

/// Test that code produces multiple errors
pub fn assert_type_errors(source: &str, count: usize) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let mut ast = parser.parse().expect("Parser failed");
    let mut checker = TypeChecker::new();

    match checker.check(ast.as_mut_slice()) {
        Err(errors) => {
            assert_eq!(
                errors.len(),
                count,
                "Expected {} errors, got {}. Errors: {:#?}",
                count,
                errors.len(),
                errors
            );
        }
        Ok(_) => panic!(
            "Expected {} errors but code type-checked successfully",
            count
        ),
    }
}

/// Test that code produces at least one error
pub fn assert_has_type_error(source: &str) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let mut ast = parser.parse().expect("Parser failed");
    let mut checker = TypeChecker::new();

    match checker.check(ast.as_mut_slice()) {
        Err(errors) => {
            assert!(
                !errors.is_empty(),
                "Expected at least one error but got none"
            );
        }
        Ok(_) => panic!("Expected type error but code type-checked successfully"),
    }
}
