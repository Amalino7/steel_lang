use crate::execute_source;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_basic_types() {
    assert_typechecks(
        r#"
        let a = 5;
        let b = 10.0;
        let c = "Hello World!";
        let d = true;
        let e = false;
        let f = 5 + 10;
        let g = 5 - 10;
        "#,
    );
}

#[test]
fn test_function_call() {
    assert_typechecks(
        r#"
        func add(a: string, b: string): string {
            return a + b;
        }

        let result = add("5", "10");

        if true {
            result + result;
        }

        func f1() {
            f2();
        }
        func f2() {
            f1();
        }
        "#,
    );
}

#[test]
fn test_function_correct_return_if_else() {
    assert_typechecks(
        r#"
        func add(a: number, b: number): number {
            if a > b {
                a / 4;
            } else {
                return b;
            }

            return 10;
        }
        "#,
    );
}

#[test]
fn test_function_correct_return_both_branches() {
    assert_typechecks(
        r#"
        func add(a: number, b: number): number {
            if a > b {
                return a / 4;
            } else {
                return b;
            }
        }
        "#,
    );
}

#[test]
fn test_nested_function_correct_return() {
    assert_typechecks(
        r#"
        func add(a: number, b: number): number {
            func add2(a: number, b: number): number {
                return add(a, b);
            }

            return add2(a, b);
        }
        "#,
    );
}

#[test]
fn test_complex_function_types() {
    assert_typechecks(
        r#"
        func foo(a: number, _b: func(): string): func(number): number {
            func bar(c: number): number {
                return a + c;
            }
            return bar;
        }
        func str(): string { return "hello"; }

        let res = foo(10, str);
        let sum = res(5) + 10;
        "#,
    );
}

#[test]
fn test_variable_shadowing_types() {
    execute_source(
        r#"
        let a: number = 10;
        {
            let a: string = "shadow";
            a = a + "ed";
        }
        let b = a + 5;
        "#,
        false,
        "check",
        true,
    );
}

#[test]
fn test_assign_void() {
    execute_source(
        r#"
        func noReturn(): void {
            return;
        }
        let x = noReturn();
        "#,
        false,
        "check",
        true,
    );
}

#[test]
fn test_multiple_errors_collected() {
    Tester::new(
        r#"
            let a: number = "hello"; // Error 1: Type mismatch
            b(1);                     // Error 2: Undefined variable 'b'
            return 10;                // Error 3: Return outside function
            "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::TypeMismatch { .. }
        )
    })
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedVariable { .. }))
    .expect_error(|e| matches!(e, TypeCheckerError::InvalidReturnOutsideFunction { .. }))
    .run();
}
