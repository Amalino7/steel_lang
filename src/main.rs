#![allow(clippy::uninlined_format_args)]

use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::stdlib::get_natives;
use crate::typechecker::type_ast::StmtKind;
use crate::typechecker::TypeChecker;
use crate::vm::disassembler::disassemble_chunk;
use crate::vm::gc::GarbageCollector;
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
            println!("{err}");
        }
        return;
    }
    let ast = ast.unwrap();

    if mode == "parse" {
        if debug {
            println!("=== AST ===");
            ast.iter().for_each(|e| println!("{}", e));
            println!("=============");
        }
        return;
    }

    let natives = get_natives();
    let mut typechecker = TypeChecker::new_with_natives(&natives);

    let analysis = typechecker.check(&ast);
    if !force && let Err(e) = &analysis {
        for err in e {
            println!("{err}");
            let error_line = err.get_line() as usize - 1;
            source.lines().enumerate().for_each(|(i, line)| {
                if error_line == i {
                    println!("{line}");
                }
            });
            println!("======================");
        }
        return;
    }

    let typed_ast = analysis.unwrap();

    if mode == "check" {
        println!("Type checking has passed.");
        if debug {
            println!("=== AST ===");
            println!("{ast:#?}");
            println!("=============");
            println!("=== Type analysis ===");
            println!("Typed ast: {typed_ast:#?}");
            println!("====================");
        }
        return;
    }

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let func = compiler.compile(&typed_ast);

    if debug {
        println!("=== Disassembly ===");
        disassemble_chunk(&func.chunk, "main_script");
        println!("===================");
    }
    let global_count = match typed_ast.kind {
        StmtKind::Global { global_count, .. } => global_count,
        _ => panic!("Global statement expected"),
    };

    let mut vm = VM::new(global_count as usize, gc);
    vm.set_native_functions(natives);

    let res = vm.run(func);
    match res {
        Ok(_) => {}
        Err(err) => {
            println!("{err}");
        }
    }
}

fn identity<T>(x: T) -> T {
    x
}
fn main() {
    let source_path = args().nth(1).expect("No source file provided.");
    let mut source = fs::read_to_string(source_path).expect("Failed to read source file.");
    source += stdlib::get_prelude();
    let mode = args().nth(2).unwrap_or_else(|| "run".to_string());
    let force = args().any(|arg| arg == "-f");
    let debug = args().any(|arg| arg == "-d");

    execute_source(source.as_str(), debug, mode.as_str(), force);
}
