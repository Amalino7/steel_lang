// These tests are expected to FAIL until unused binding warnings for local
// variables and parameters are implemented in the typechecker.
use crate::typechecker::core::error::TypeCheckerWarning;
use crate::typechecker::tests::helpers::*;

#[test]
#[ignore = "UnusedBinding warning for local variables is not yet implemented"]
fn test_unused_local_variable() {
    Tester::new(
        r#"
        func foo() {
            let x = 5;
        }
        "#,
    )
    .expect_warning(
        |w| matches!(w, TypeCheckerWarning::UnusedBinding { name, .. } if name == "x"),
    )
    .run();
}

#[test]
#[ignore = "UnusedBinding warning for function parameters is not yet implemented"]
fn test_unused_function_parameter() {
    Tester::new(
        r#"
        func foo(x: number, y: number): number {
            return x;
        }
        "#,
    )
    .expect_warning(
        |w| matches!(w, TypeCheckerWarning::UnusedBinding { name, .. } if name == "y"),
    )
    .run();
}

#[test]
#[ignore = "UnusedBinding warning for local variables is not yet implemented"]
fn test_unused_variable_in_block() {
    Tester::new(
        r#"
        {
            let unused = 42;
        }
        "#,
    )
    .expect_warning(
        |w| matches!(w, TypeCheckerWarning::UnusedBinding { name, .. } if name == "unused"),
    )
    .run();
}
