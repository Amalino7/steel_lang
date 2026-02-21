use crate::typechecker::core::error::TypeCheckerWarning;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_unreachable_after_return() {
    Tester::new(
        r#"
        func add(a: number, b: number): number {
            return 12;
            a + b;
        }
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }))
    .run();
}

#[test]
fn test_unreachable_multiple_statements() {
    Tester::new(
        r#"
        func test(): number {
            return 10;
            let a = 5;
            return a;
        }
        while false {
        }
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }))
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }))
    .run();
}

#[test]
fn test_unreachable_in_if_branch() {
    Tester::new(
        r#"
        func foo(): number {
            if true {
                return 1;
                let dead = 2;
            }
            return 0;
        }
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }))
    .run();
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
