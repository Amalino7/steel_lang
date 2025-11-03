#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::typechecker::error::TypeCheckerError;
    use crate::typechecker::TypeChecker;

    #[test]
    fn test_type_checker() {
        let source = r#"
            let a = 5;
            let b = 10.0;
            let c = "Hello World!";
            let d = true;
            let e = false;
            let f = 5 + 10;
            let g = 5 - 10;"#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        checker
            .check(ast.as_mut_slice())
            .expect("Type checker failed.");
        println!("{:#?}", ast);
        println!(
            "{}",
            ast.iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        );
    }
    #[test]
    fn test_type_checker_function_call() {
        let source = r#"
            func add(a:string, b:string): string {
                return a + b;
            }

            let result = add("5", "10");

            if true {
                result + result;
            }

            func f1(){
                f2();
            }
            func f2(){
                f1();
            }
            "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        checker
            .check(ast.as_mut_slice())
            .expect("Type checker failed.");
        println!("{:#?}", ast);
        println!(
            "{}",
            ast.iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        );
    }

    #[test]
    fn test_tc_undefined_variable() {
        let source = "let a = b;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        let res = checker.check(ast.as_mut_slice());
        match res {
            Err(errors) => {
                assert!(
                    errors
                        .iter()
                        .any(|e| matches!(e, TypeCheckerError::UndefinedVariable { .. }))
                );
            }
            _ => panic!("Expected UndefinedVariable error"),
        }
    }

    #[test]
    fn test_tc_callee_not_function() {
        let source = r#"
            let a = 10;
            a(1);
        "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        let res = checker.check(ast.as_mut_slice());
        match res {
            Err(errors) => {
                assert!(
                    errors
                        .iter()
                        .any(|e| matches!(e, TypeCheckerError::CalleeIsNotAFunction { .. }))
                );
            }
            _ => panic!("Expected CalleeIsNotAFunction error"),
        }
    }

    #[test]
    fn test_tc_arity_mismatch() {
        let source = r#"
            func f(a:number): number { return a; }
            let x = f();
        "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        let res = checker.check(ast.as_mut_slice());
        match res {
            Err(errors) => {
                assert!(
                    errors
                        .iter()
                        .any(|e| matches!(e, TypeCheckerError::IncorrectArity { .. }))
                );
            }
            _ => panic!("Expected IncorrectArity error"),
        }
    }

    #[test]
    fn test_tc_type_mismatch_in_let() {
        let source = "let a: number = true;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        let res = checker.check(ast.as_mut_slice());
        match res {
            Err(errors) => {
                assert!(
                    errors
                        .iter()
                        .any(|e| matches!(e, TypeCheckerError::TypeMismatch { .. }))
                );
            }
            _ => panic!("Expected TypeMismatch error"),
        }
    }

    #[test]
    fn test_tc_invalid_return_outside_function() {
        let source = "return 5;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        let res = checker.check(ast.as_mut_slice());
        match res {
            Err(errors) => {
                assert!(
                    errors.iter().any(|e| matches!(
                        e,
                        TypeCheckerError::InvalidReturnOutsideFunction { .. }
                    ))
                );
            }
            _ => panic!("Expected InvalidReturnOutsideFunction error"),
        }
    }

    #[test]
    fn test_tc_param_type_mismatch() {
        let source = r#"
            func add(a:number, b:number): number { return a + b; }
            let x = add(1, true);
        "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        let res = checker.check(ast.as_mut_slice());
        match res {
            Err(errors) => {
                assert!(
                    errors.iter().any(|e| matches!(
                        e,
                        TypeCheckerError::FunctionParameterTypeMismatch { .. }
                    ))
                );
            }
            _ => panic!("Expected FunctionParameterTypeMismatch error"),
        }
    }

    #[test]
    fn test_tc_multiple_errors() {
        let source = r#"
                let a: number = "hello"; // Error 1: Type mismatch
                b(1);                     // Error 2: Undefined variable 'b'
                return 10;                // Error 3: Return outside function
                let c = 5 + "str";        // Error 4: Binary op type mismatch
                let c = 4;
                c();                      // Error 5: Callee is not a function
            "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().expect("Parser failed.");
        let mut checker = TypeChecker::new();
        let errors = checker.check(ast.as_mut_slice()).unwrap_err();

        assert!(
            errors.len() >= 5,
            "Expected at least 5 errors, got {}",
            errors.len()
        );

        for err in &errors {
            println!("{}", err);
        }

        // Check for specific error types
        let mut found_type_mismatch = false;
        let mut found_undefined_variable = false;
        let mut found_invalid_return = false;
        let mut found_callee_is_not_function = false;

        for error in errors {
            match error {
                TypeCheckerError::TypeMismatch { .. } => found_type_mismatch = true,
                TypeCheckerError::UndefinedVariable { .. } => found_undefined_variable = true,
                TypeCheckerError::InvalidReturnOutsideFunction { .. } => {
                    found_invalid_return = true
                }
                TypeCheckerError::CalleeIsNotAFunction { .. } => {
                    found_callee_is_not_function = true;
                }
                _ => {}
            }
        }
        assert!(found_type_mismatch, "Expected TypeMismatch error");
        assert!(found_undefined_variable, "Expected UndefinedVariable error");
        assert!(
            found_invalid_return,
            "Expected InvalidReturnOutsideFunction error"
        );
        assert!(
            found_callee_is_not_function,
            "Expected CalleeIsNotAFunction error"
        );
    }
}
