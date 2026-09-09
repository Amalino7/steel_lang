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
    // Multiple stmts after a single diverge → one grouped warning.
    Tester::new(
        r#"
        func test(): number {
            return 10;
            let _a = 5;
            return _a;
        }
        while false {
        }
        "#,
    )
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
                let _dead = 2;
            }
            return 0;
        }
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }))
    .run();
}

#[test]
fn test_unreachable_all_stmts_after_diverge() {
    // All stmts after a single diverge are grouped into one warning with a unified span.
    Tester::new(
        r#"
        func test(): number {
            return 0;
            let _a = 1;
            let _b = 2;
            let _c = 3;
        }
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. }))
    .run();
}

#[test]
fn test_unreachable_inside_branches_and_after_if() {
    // Unreachable code inside each branch, after if/else when both arms return,
    // and multiple stmts after a later diverge — all must be reported individually.
    Tester::new(
        r#"
        func some_func(): number {
            while true {
                if true {
                    return 1;
                    let _dead1 = 1;
                } else {
                    return 2;
                    let _dead2 = 2;
                }
                let _dead3 = 3;
            }
            return 0;
            let _dead4 = 4;
            let _dead5 = 5;
        }
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. })) // dead1 (if-then scope)
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. })) // dead2 (else scope)
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. })) // dead3 (while-body scope)
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnreachableCode { .. })) // dead4+dead5 grouped
    .run();
}

#[test]
fn test_no_unreachable_with_correct_returns() {
    // All code is reachable - no warnings expected
    assert_typechecks(
        r#"
        func larger(a: number, b: number): number {
            if a > b {
                return a;
            } else {
                return b;
            }
        }
        "#,
    );
}
