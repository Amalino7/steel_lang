#![allow(clippy::uninlined_format_args)]

pub mod compiler;
pub mod parser;
pub mod scanner;
pub mod stdlib;
pub mod typechecker;
pub mod vm;

use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::stdlib::{get_natives, get_prelude};
use crate::typechecker::core::ast::StmtKind;
use crate::typechecker::TypeChecker;
use crate::vm::gc::{GarbageCollector, Gc};
use crate::vm::value::Function;
use crate::vm::VM;
use ariadne::{Color, Config, IndexType, Label, Report, ReportKind, Source};

pub fn run_file(file_name: &str, source: &str, debug: bool, mode: &str, force: bool) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);

    let ast = parser.parse();
    if !force && let Err(errors) = &ast {
        for err in errors {
            let span = err.span();
            let span_range = span.start..span.end;

            Report::build(ReportKind::Error, file_name, span.start)
                .with_config(Config::default().with_index_type(IndexType::Byte))
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
    if !force && let Err(errors) = &analysis {
        for err in errors {
            err.create_report(file_name)
                .print((file_name, Source::from(source)))
                .unwrap();
        }
        return;
    }

    let (typed_ast, warnings) = analysis.unwrap();

    for warning in &warnings {
        warning
            .create_report(file_name)
            .print((file_name, Source::from(source)))
            .unwrap();
    }

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
    let func = compiler.compile(0, &typed_ast);

    if debug {
        println!("=== Disassembly ===");
        vm::disassembler::disassemble_chunk(&func.chunk, "main_script");
        println!("===================");
    }
    let global_count = match typed_ast.kind {
        StmtKind::Global { global_count, .. } => global_count,
        _ => panic!("Global statement expected"),
    };
    drop(typechecker);
    let mut vm = VM::new(global_count as usize, &mut gc);
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

/// A compiled Steel program that can be run multiple times. Used in benching
pub struct SteelProgram {
    func: Gc<Function>,
    gc: GarbageCollector,
    global_count: usize,
}

impl SteelProgram {
    /// Compiles the code to avoid that overhead when benching the vm
    pub fn compile(source: &str) -> Self {
        let mut full_source = source.to_string();
        full_source.push_str(get_prelude());

        let scanner = Scanner::new(&full_source);
        let mut parser = Parser::new(scanner);
        let ast = parser.parse().expect("SteelProgram: parse failed");

        let natives = get_natives();
        let mut typechecker = TypeChecker::new_with_natives(&natives);
        let (typed_ast, _warnings) = typechecker
            .check(&ast)
            .expect("SteelProgram: type-check failed");

        let global_count = match &typed_ast.kind {
            StmtKind::Global { global_count, .. } => *global_count as usize,
            _ => panic!("SteelProgram: expected Global statement"),
        };

        let mut gc = GarbageCollector::new();
        let compiler = Compiler::new("main".to_string(), &mut gc);
        let func = compiler.compile(0, &typed_ast);

        SteelProgram {
            func,
            gc,
            global_count,
        }
    }

    pub fn run_once(&mut self) {
        let func = self.func;
        let mut vm = VM::new(self.global_count, &mut self.gc);
        vm.set_native_functions(get_natives());
        vm.run(func).expect("SteelProgram: runtime error");
        drop(vm);
        self.gc.collect_roots(self.func);
    }
}
