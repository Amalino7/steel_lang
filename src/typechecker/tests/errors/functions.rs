use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_callee_not_callable() {
    Tester::new(
        r#"
        let a = 10;
        a(1);
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::CalleeIsNotCallable { .. }))
    .run();
}

#[test]
fn test_string_not_callable() {
    Tester::new(
        r#"
        let s = "hello";
        s();
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::CalleeIsNotCallable { .. }))
    .run();
}

#[test]
fn test_missing_argument() {
    Tester::new(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(1);
        "#,
    )
    .expect_error(
        |e| matches!(e, TypeCheckerError::MissingArgument { param_name, .. } if param_name == "b"),
    )
    .run();
}

#[test]
fn test_too_many_arguments() {
    Tester::new(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(1, 2, 3);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::TooManyArguments {
                expected: 2,
                found: 3,
                ..
            }
        )
    })
    .run();
}

#[test]
fn test_too_many_arguments_zero_params() {
    Tester::new(
        r#"
        func foo(): void {}
        foo(1);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::TooManyArguments {
                expected: 0,
                found: 1,
                ..
            }
        )
    })
    .run();
}

#[test]
fn test_duplicate_parameter_name() {
    // Duplicate parameters produce a Redeclaration error (not DuplicateArgument which is for invocation)
    Tester::new(r#"func foo(x: number, x: string): void {}"#)
        .expect_error(|e| matches!(e, TypeCheckerError::Redeclaration { name, .. } if name == "x"))
        .run();
}

#[test]
fn test_undefined_named_parameter() {
    Tester::new(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(a: 1, c: 2);
        "#).expect_error(
        |e| matches!(e, TypeCheckerError::UndefinedParameter { param_name, .. } if param_name == "c"),
    ).run();
}

#[test]
fn test_positional_after_named_argument() {
    Tester::new(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(a: 1, 2);
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::PositionalArgumentAfterNamed { .. }))
    .run();
}

#[test]
fn test_parameter_type_mismatch() {
    Tester::new(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(1, true);
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::TypeMismatch { .. }
                | TypeCheckerError::ComplexTypeMismatch { .. }
                | TypeCheckerError::ComplexTypeMismatchWithOrigins { .. }
        )
    })
    .run();
}

#[test]
fn test_return_type_mismatch() {
    Tester::new(
        r#"
        func get_number(): number {
            return "string";
        }
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::TypeMismatch { .. }
                | TypeCheckerError::ComplexTypeMismatch { .. }
                | TypeCheckerError::ComplexTypeMismatchWithOrigins { .. }
        )
    })
    .run();
}

#[test]
fn test_missing_return_statement() {
    Tester::new(
        r#"
        func add(a: number, b: number): number {
            // Missing return statement
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::MissingReturnStatement { .. }))
    .run();
}

#[test]
fn test_missing_return_one_branch() {
    Tester::new(
        r#"
        func foo(x: number): number {
            if x > 0 {
                return x;
            }
            // Missing else branch return
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::MissingReturnStatement { .. }))
    .run();
}

#[test]
fn test_invalid_return_outside_function() {
    Tester::new("return 5;")
        .expect_error(|e| matches!(e, TypeCheckerError::InvalidReturnOutsideFunction { .. }))
        .run();
}

#[test]
fn test_invalid_return_in_global_block() {
    Tester::new(
        r#"
        {
            return 10;
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::InvalidReturnOutsideFunction { .. }))
    .run();
}
