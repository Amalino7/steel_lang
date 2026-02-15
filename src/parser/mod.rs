pub mod ast;
pub mod error;
mod expressions;
mod statements;

use crate::parser::error::ParserError;
use crate::scanner::Scanner;
use crate::scanner::TokenType as TokT;
use crate::scanner::{Token, TokenType};
use ast::Stmt;
use std::mem;

pub struct Parser<'src> {
    scanner: Scanner<'src>,
    previous_token: Token<'src>,
    current_token: Token<'src>,
    next_token: Token<'src>,
    errors: Vec<ParserError<'src>>,
}
macro_rules! check_token_type {
    ($parser:expr, $( $token_type:pat $(,)?)*) => {
        match $parser.current_token.token_type {
            $( $token_type )|+  => true,
            _ => false,
        }
    };
}

use check_token_type;
macro_rules! match_token_type {
    ($parser:expr, $( $token_type:pat $(,)?)*) => {
        if check_token_type!($parser, $( $token_type ),+ ) {
            $parser.advance()?;
            true
        }
        else {
            false
        }
    };
}

use match_token_type;

macro_rules! check_next_token_type {
    ($parser:expr, $( $token_type:pat $(,)?)*) => {
        match $parser.next_token.token_type {
            $( $token_type )|+  => true,
            _ => false,
        }
    };
}
use check_next_token_type;

impl<'src> Parser<'src> {
    pub fn new(mut scanner: Scanner<'src>) -> Self {
        let start_token = scanner.next_token();
        let next = scanner.next_token();
        Parser {
            scanner,
            previous_token: start_token.clone(),
            current_token: start_token,
            next_token: next,
            errors: Vec::new(),
        }
    }
    fn advance(&mut self) -> Result<(), ParserError<'src>> {
        self.previous_token = self.current_token.clone();
        self.current_token = self.next_token.clone();
        self.next_token = self.scanner.next_token();
        if self.current_token.token_type == TokenType::UnexpectedSymbolError {
            Err(ParserError::UnexpectedToken {
                found: self.current_token.clone(),
                message: "Invalid token".to_string(),
            })
        } else if self.current_token.token_type == TokenType::Error {
            Err(ParserError::ScannerError {
                token: self.current_token.clone(),
            })
        } else {
            Ok(())
        }
    }

    fn consume(
        &mut self,
        token_type: TokenType,
        message: impl Into<String>,
    ) -> Result<(), ParserError<'src>> {
        if self.current_token.token_type == token_type {
            self.advance()
        } else {
            Err(ParserError::MissingToken {
                expected: token_type,
                message: message.into(),
                found_token: self.current_token.clone(),
            })
        }
    }

    fn error_current(&mut self, message: impl Into<String>) -> ParserError<'src> {
        ParserError::ParseError {
            token: self.current_token.clone(),
            message: message.into(),
        }
    }
    fn error_previous(&mut self, message: impl Into<String>) -> ParserError<'src> {
        ParserError::ParseError {
            token: self.previous_token.clone(),
            message: message.into(),
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt<'src>>, Vec<ParserError<'src>>> {
        let mut statements = vec![];
        while !self.scanner.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }
        if !self.errors.is_empty() {
            Err(mem::take(&mut self.errors))
        } else {
            Ok(statements)
        }
    }

    fn synchronize(&mut self) {
        while !self.scanner.is_at_end() {
            if self.previous_token.token_type == TokenType::Semicolon {
                return;
            }
            if check_token_type!(
                self,
                TokT::Func,
                TokT::RightBrace,
                TokT::Match,
                TokT::Let,
                TokT::If,
                TokT::While,
                TokT::Return,
                TokT::Enum,
                TokT::Struct,
                TokT::Impl,
            ) {
                return;
            }
            let _ = self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    fn parse_snapshot(source: &str) -> String {
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        match parser.parse() {
            Ok(stmts) => stmts
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join("\n"),
            Err(errors) => errors
                .iter()
                .map(|e| format!("ERROR: {}", e.message()))
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }

    // Expression tests
    #[test]
    fn test_arithmetic_expressions() {
        let source = "let x = 1 + 2 * 3 - 4 / 2;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_unary_expressions() {
        let source = "let x = -5;\nlet y = !true;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
        assert!(output.contains("let y"));
    }

    #[test]
    fn test_logical_expressions() {
        let source = "let x = true and false or true;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_comparison_expressions() {
        let source = "let x = 5 > 3;\nlet y = 10 <= 20;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
        assert!(output.contains("let y"));
    }

    #[test]
    fn test_assignment() {
        let source = "let x = 10;\nx = 20;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_compound_assignment() {
        let source = "let x = 10;\nx += 5;\nx -= 3;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_function_call() {
        let source = "println(\"hello\");";
        let output = parse_snapshot(source);
        assert!(output.contains("println"));
    }

    #[test]
    fn test_member_access() {
        let source = "let x = obj.field;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_index_access() {
        let source = "let x = arr[0];";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_tuple_literal() {
        let source = "let x = (1, 2, 3);";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_list_literal() {
        let source = "let x = [1, 2, 3];";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_map_literal() {
        let source = r#"let x = ["a": 1, "b": 2];"#;
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_force_unwrap() {
        let source = "let x = maybe!;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_safe_navigation() {
        let source = "let x = obj?.field;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_is_type() {
        let source = "let x = val is string;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_null_coalescing() {
        let source = "let x = a ?? b;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    // Statement tests
    #[test]
    fn test_let_with_type() {
        let source = "let x: number = 42;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
        assert!(output.contains("number"));
    }

    #[test]
    fn test_let_inferred() {
        let source = "let x = 42;";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
    }

    #[test]
    fn test_if_statement() {
        let source = "if x > 0 { let y = 1; }";
        let output = parse_snapshot(source);
        assert!(output.contains("if"));
    }

    #[test]
    fn test_if_else_statement() {
        let source = "if x > 0 { let y = 1; } else { let y = 2; }";
        let output = parse_snapshot(source);
        assert!(output.contains("if"));
        assert!(output.contains("else"));
    }

    #[test]
    fn test_while_statement() {
        let source = "while x > 0 { x = x - 1; }";
        let output = parse_snapshot(source);
        assert!(output.contains("while"));
    }

    #[test]
    fn test_return_statement() {
        let source = "func foo(): void { return; }";
        let output = parse_snapshot(source);
        assert!(output.contains("func"));
        assert!(output.contains("return"));
    }

    #[test]
    fn test_block_statement() {
        let source = "{ let x = 1; let y = 2; }";
        let output = parse_snapshot(source);
        assert!(output.contains("let x"));
        assert!(output.contains("let y"));
    }

    #[test]
    fn test_match_statement() {
        let source = "match x { .Some(val) => { let y = val; } .None => { let y = 0; } }";
        let output = parse_snapshot(source);
        assert!(output.contains("match"));
    }

    // Declaration tests
    #[test]
    fn test_function_declaration() {
        let source = "func add(a: number, b: number): number { return a + b; }";
        let output = parse_snapshot(source);
        assert!(output.contains("func"));
        assert!(output.contains("add"));
    }

    #[test]
    fn test_struct_declaration() {
        let source = "struct Point { x: number, y: number }";
        let output = parse_snapshot(source);
        assert!(output.contains("struct"));
        assert!(output.contains("Point"));
    }

    #[test]
    fn test_enum_unit_variant() {
        let source = "enum Color { Red, Green, Blue }";
        let output = parse_snapshot(source);
        assert!(output.contains("enum"));
        assert!(output.contains("Color"));
    }

    #[test]
    fn test_enum_tuple_variant() {
        let source = "enum Option { Some(number), None }";
        let output = parse_snapshot(source);
        assert!(output.contains("enum"));
        assert!(output.contains("Option"));
    }

    #[test]
    fn test_enum_struct_variant() {
        let source = "enum Result { Ok { value: number }, Err { msg: string } }";
        let output = parse_snapshot(source);
        assert!(output.contains("enum"));
        assert!(output.contains("Result"));
    }

    #[test]
    fn test_impl_block() {
        let source = "impl Point { func new(): Point { return Point { x: 0, y: 0 }; } }";
        let output = parse_snapshot(source);
        assert!(output.contains("impl"));
        assert!(output.contains("Point"));
    }

    #[test]
    fn test_interface_declaration() {
        let source = "interface Printable { func print(self): void; }";
        let output = parse_snapshot(source);
        assert!(output.contains("interface"));
        assert!(output.contains("Printable"));
    }

    #[test]
    fn test_generic_function() {
        let source = "func identity<T>(x: T): T { return x; }";
        let output = parse_snapshot(source);
        assert!(output.contains("func"));
        assert!(output.contains("identity"));
    }

    // Error message tests
    #[test]
    fn test_error_missing_semicolon() {
        let source = "let a = 10";
        let output = parse_snapshot(source);
        assert!(output.contains("ERROR"));
        assert!(output.contains("Missing ';'"));
    }

    #[test]
    fn test_error_missing_right_paren() {
        let source = "let a = (10 + 2;";
        let output = parse_snapshot(source);
        assert!(output.contains("ERROR"));
        assert!(output.contains("Missing ')'"));
    }

    #[test]
    fn test_error_missing_right_brace() {
        let source = "func foo(): void { let x = 1;";

        let output = parse_snapshot(source);
        assert!(output.contains("ERROR"));
        assert!(output.contains("Missing '}'"));
    }

    #[test]
    fn test_error_invalid_expression() {
        let source = "let x = };";
        let output = parse_snapshot(source);
        assert!(output.contains("ERROR"));
        assert!(output.contains("Expected expression"));
    }

    #[test]
    fn test_error_invalid_assignment_target() {
        let source = "5 = 10;";
        let output = parse_snapshot(source);
        assert!(output.contains("ERROR"));
        assert!(output.contains("Invalid assignment target"));
    }

    // Error recovery tests
    #[test]
    fn test_multiple_errors_in_source() {
        let source = "let a = 10\nlet b = 20\nlet c = 30;";
        let output = parse_snapshot(source);
        // Should report multiple errors for missing semicolons
        let error_count = output.matches("ERROR").count();
        assert!(error_count >= 2, "Should report multiple errors");
    }

    #[test]
    fn test_errors_inside_block() {
        let source = r#"
        func foo(): void {
            let x = 10
            let y = 20
            let z = 30;
        }
        "#;
        let output = parse_snapshot(source);
        // Should report errors for missing semicolons inside the block
        let error_count = output.matches("ERROR").count();
        assert!(
            error_count >= 2,
            "Should report multiple errors inside block"
        );
    }

    #[test]
    fn test_recovery_continues_after_block_error() {
        let source = r#"
        func foo(): void {
            let x = 10
        }
        let y = 42;
        "#;
        let output = parse_snapshot(source);
        println!("{}", output);
    }
    #[test]
    fn test_parser_error_missing_semicolon() {
        let source = "let a = 10";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse();
        assert!(res.is_err(), "Parser should error on missing semicolon");
    }

    #[test]
    fn test_parser_error_missing_right_paren() {
        let source = "let a = (10 + 2;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        assert!(
            parser.parse().is_err(),
            "Parser should error on missing right parenthesis"
        );
    }

    #[test]
    fn test_parser_else_if() {
        let source = r#"
        if (a > 0) {
            println("a is greater than 0");
        } else if (a < 0) {
            println("a is less than 0");
        } else {
            println("a is equal to 0");
        }"#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let _ast = parser.parse().expect("Failed to parse.");
    }
}
