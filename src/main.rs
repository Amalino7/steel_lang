use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::stdlib::get_natives;
use crate::typechecker::TypeChecker;
use crate::vm::disassembler::disassemble_chunk;
use crate::vm::VM;
use std::env::args;
use std::fs;

mod compiler;
mod parser;
mod scanner;
mod stdlib;
mod token;
mod typechecker;
mod vm;

pub fn execute_source(source: &str, debug: bool, mode: &str, force: bool) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);

    let ast = parser.parse();
    if !force && let Err(e) = &ast {
        for err in e {
            println!("{}", err);
        }
        return;
    }
    let mut ast = ast.unwrap();

    if mode == "parse" {
        if debug {
            println!("=== AST ===");
            println!("{:#?}", ast);
            println!("=============");
        }
        return;
    }

    let natives = get_natives();
    let mut typechecker = TypeChecker::new_with_natives(&natives);

    let analysis = typechecker.check(&mut ast);
    if !force && let Err(e) = &analysis {
        for err in e {
            println!("{}", err);
        }
        return;
    }

    let analysis = analysis.unwrap();

    if mode == "check" {
        println!("Type checking has passed.");
        if debug {
            println!("=== AST ===");
            println!("{:#?}", ast);
            println!("=============");
            println!("=== Type analysis ===");
            println!("Type analysis: {:#?}", analysis);
            println!("====================");
        }
        return;
    }

    let compiler = Compiler::new(analysis, "main".to_string());
    let func = compiler.compile(&ast);

    if debug {
        println!("=== Disassembly ===");
        disassemble_chunk(&func.chunk, "main_script");
        println!("===================");
    }

    let mut vm = VM::new(analysis.global_count);
    vm.set_native_functions(natives);

    let result = vm.run(func);

    println!("Result: {}", result);
}

fn main() {
    let source_path = args().nth(1).expect("No source file provided.");
    let source = fs::read_to_string(source_path).expect("Failed to read source file.");
    let mode = args().nth(2).unwrap_or_else(|| "run".to_string());
    let force = args().any(|arg| arg == "-f");
    let debug = args().any(|arg| arg == "-d");

    execute_source(source.as_str(), debug, mode.as_str(), force);
}
