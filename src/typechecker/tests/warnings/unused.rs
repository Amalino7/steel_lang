use crate::typechecker::core::error::TypeCheckerWarning;
use crate::typechecker::tests::helpers::*;

#[test]
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
