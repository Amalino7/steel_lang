// These tests are expected to FAIL until unused binding warnings for local
// variables and parameters are implemented in the typechecker.
use crate::typechecker::core::error::TypeCheckerWarning;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_unused_local_variable() {
    // FAILS: UnusedBinding warning for local variables is not yet implemented
    assert_type_warning(
        r#"
        func foo() {
            let x = 5;
        }
        "#,
        |w| matches!(w, TypeCheckerWarning::UnusedBinding { name, .. } if name == "x"),
    );
}

#[test]
fn test_unused_function_parameter() {
    // FAILS: UnusedBinding warning for function parameters is not yet implemented
    assert_type_warning(
        r#"
        func foo(x: number, y: number): number {
            return x;
        }
        "#,
        |w| matches!(w, TypeCheckerWarning::UnusedBinding { name, .. } if name == "y"),
    );
}

#[test]
fn test_unused_variable_in_block() {
    // FAILS: UnusedBinding warning for local variables is not yet implemented
    assert_type_warning(
        r#"
        {
            let unused = 42;
        }
        "#,
        |w| matches!(w, TypeCheckerWarning::UnusedBinding { name, .. } if name == "unused"),
    );
}
