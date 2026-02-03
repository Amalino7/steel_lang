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
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::env::args;
use std::fs;

mod compiler;
mod parser;
mod scanner;
mod stdlib;
mod typechecker;
mod vm;

pub fn run_file(file_name: &str, source: &str, debug: bool, mode: &str, force: bool) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);

    let ast = parser.parse();
    if !force && let Err(errors) = &ast {
        for err in errors {
            let span = err.span();
            let span_range = span.start..span.end;

            Report::build(ReportKind::Error, file_name, span.start)
                .with_message("Syntax Error")
                .with_label(
                    Label::new((file_name, span_range))
                        .with_message(err.message())
                        .with_color(Color::Red),
                )
                .finish()
                .print((file_name, Source::from(source)))
                .unwrap();
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
            let span = err.span();
            let span_range = span.start..span.end;
            Report::build(ReportKind::Error, file_name, span.start)
                .with_message("Syntactic Error")
                .with_label(
                    Label::new((file_name, span_range))
                        .with_message(err.message())
                        .with_color(Color::Red),
                )
                .finish()
                .print((file_name, Source::from(source)))
                .unwrap();
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
pub fn execute_source(source: &str, debug: bool, mode: &str, force: bool) {
    run_file("test.steel", source, debug, mode, force);
}

fn main() {
    let source_path = args().nth(1).expect("No source file provided.");
    let mut source = fs::read_to_string(source_path.clone()).expect("Failed to read source file.");
    source += stdlib::get_prelude();
    let mode = args().nth(2).unwrap_or_else(|| "run".to_string());
    let force = args().any(|arg| arg == "-f");
    let debug = args().any(|arg| arg == "-d");

    run_file(&source_path, source.as_str(), debug, mode.as_str(), force);
}
