use crate::typechecker::TypeChecker;
use std::env::args;
use std::fs;

mod compiler;
mod parser;
mod scanner;
mod token;
mod typechecker;
mod vm;

fn main() {
    let source_path = args().nth(1).expect("No source file provided.");
    let source = fs::read_to_string(source_path).expect("Failed to read source file.");
    let mode = args().nth(2).unwrap_or_else(|| "run".to_string());

    let mut scanner = scanner::Scanner::new(&source);
    if mode == "lex" {
        loop {
            let tok = scanner.next_token();
            println!("{:?}", tok);
            if tok.token_type == token::TokenType::EOF {
                break;
            }
        }
        return;
    }

    let mut parser = parser::Parser::new(scanner);
    let mut ast = parser.parse().expect("Failed to parse source file.");

    if mode == "parse" {
        println!("{:#?}", ast);
        return;
    }

    let mut type_checker = TypeChecker::new();
    type_checker
        .check(ast.as_mut_slice())
        .expect("Failed to type check source file.");

    if mode == "check" {
        println!("{:#?}", ast);
        return;
    }

    if mode == "run" {
        return;
    }
}
