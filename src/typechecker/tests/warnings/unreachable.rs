use crate::typechecker::core::error::TypeCheckerWarning;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_unreachable_after_return() {
    assert_type_warning(
        r#"
        func add(a: number, b: number): number {
            return 12;
            a + b;
        }
        "#,
        |w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }),
    );
}

#[test]
fn test_unreachable_multiple_statements() {
    let (_, warnings) = {
        use crate::parser::Parser;
        use crate::scanner::Scanner;
        use crate::typechecker::TypeChecker;

        let source = r#"
        func test(): number {
            return 10;
            let a = 5;
            return a;
        }
        while false {
        }
        "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().expect("Parser failed");
        let mut checker = TypeChecker::new();
        checker
            .check(ast.as_mut_slice())
            .expect("Should have succeeded with warnings")
    };

    assert_eq!(
        warnings.len(),
        2,
        "Expected 2 unreachable warnings, got {}",
        warnings.len()
    );
    assert!(
        warnings
            .iter()
            .all(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }))
    );
}

#[test]
fn test_unreachable_in_if_branch() {
    assert_type_warning(
        r#"
        func foo(): number {
            if true {
                return 1;
                let dead = 2;
            }
            return 0;
        }
        "#,
        |w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }),
    );
}

#[test]
fn test_no_unreachable_with_correct_returns() {
    // All code is reachable - no warnings expected
    assert_typechecks(
        r#"
        func add(a: number, b: number): number {
            if a > b {
                return a;
            } else {
                return b;
            }
        }
        "#,
    );
}
