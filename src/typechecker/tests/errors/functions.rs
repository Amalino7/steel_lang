use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_callee_not_callable() {
    assert_type_error(
        r#"
        let a = 10;
        a(1);
        "#,
        |e| matches!(e, TypeCheckerError::CalleeIsNotCallable { .. }),
    );
}

#[test]
fn test_string_not_callable() {
    assert_type_error(
        r#"
        let s = "hello";
        s();
        "#,
        |e| matches!(e, TypeCheckerError::CalleeIsNotCallable { .. }),
    );
}

#[test]
fn test_missing_argument() {
    assert_type_error(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(1);
        "#,
        |e| matches!(e, TypeCheckerError::MissingArgument { param_name, .. } if param_name == "b"),
    );
}

#[test]
fn test_too_many_arguments() {
    assert_type_error(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(1, 2, 3);
        "#,
        |e| matches!(e, TypeCheckerError::TooManyArguments { expected: 2, found: 3, .. }),
    );
}

#[test]
fn test_too_many_arguments_zero_params() {
    assert_type_error(
        r#"
        func foo(): void {}
        foo(1);
        "#,
        |e| matches!(e, TypeCheckerError::TooManyArguments { expected: 0, found: 1, .. }),
    );
}

#[test]
fn test_duplicate_parameter_name() {
    // Duplicate parameters produce a Redeclaration error (not DuplicateArgument)
    assert_type_error(
        r#"func foo(x: number, x: string): void {}"#,
        |e| matches!(e, TypeCheckerError::Redeclaration { name, .. } if name == "x"),
    );
}

#[test]
fn test_undefined_named_parameter() {
    assert_type_error(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(a: 1, c: 2);
        "#,
        |e| matches!(e, TypeCheckerError::UndefinedParameter { param_name, .. } if param_name == "c"),
    );
}

#[test]
fn test_positional_after_named_argument() {
    assert_type_error(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(a: 1, 2);
        "#,
        |e| matches!(e, TypeCheckerError::PositionalArgumentAfterNamed { .. }),
    );
}

#[test]
fn test_parameter_type_mismatch() {
    assert_type_error(
        r#"
        func add(a: number, b: number): number { return a + b; }
        let x = add(1, true);
        "#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatchWithOrigins { .. }
            )
        },
    );
}

#[test]
fn test_return_type_mismatch() {
    assert_type_error(
        r#"
        func get_number(): number {
            return "string";
        }
        "#,
        |e| {
            matches!(
                e,
                TypeCheckerError::TypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatch { .. }
                    | TypeCheckerError::ComplexTypeMismatchWithOrigins { .. }
            )
        },
    );
}

#[test]
fn test_missing_return_statement() {
    assert_type_error(
        r#"
        func add(a: number, b: number): number {
            // Missing return statement
        }
        "#,
        |e| matches!(e, TypeCheckerError::MissingReturnStatement { .. }),
    );
}

#[test]
fn test_missing_return_one_branch() {
    assert_type_error(
        r#"
        func foo(x: number): number {
            if x > 0 {
                return x;
            }
            // Missing else branch return
        }
        "#,
        |e| matches!(e, TypeCheckerError::MissingReturnStatement { .. }),
    );
}

#[test]
fn test_invalid_return_outside_function() {
    assert_type_error("return 5;", |e| {
        matches!(e, TypeCheckerError::InvalidReturnOutsideFunction { .. })
    });
}

#[test]
fn test_invalid_return_in_global_block() {
    assert_type_error(
        r#"
        {
            return 10;
        }
        "#,
        |e| matches!(e, TypeCheckerError::InvalidReturnOutsideFunction { .. }),
    );
}
