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

pub struct PhaseTimings {
    pub scan_parse: std::time::Duration,
    pub type_checking: std::time::Duration,
    pub compilation: std::time::Duration,
    pub execution: std::time::Duration,
}

impl PhaseTimings {
    fn new() -> Self {
        Self {
            scan_parse: std::time::Duration::ZERO,
            type_checking: std::time::Duration::ZERO,
            compilation: std::time::Duration::ZERO,
            execution: std::time::Duration::ZERO,
        }
    }

    pub fn print(&self) {
        let total = self.scan_parse + self.type_checking + self.compilation + self.execution;
        println!("\n=== Phase Timings ===");
        println!("Scan + Parse:  {:>8.3}ms", self.scan_parse.as_secs_f64() * 1000.0);
        println!("Type checking: {:>8.3}ms", self.type_checking.as_secs_f64() * 1000.0);
        println!("Compilation:   {:>8.3}ms", self.compilation.as_secs_f64() * 1000.0);
        println!("Execution:     {:>8.3}ms", self.execution.as_secs_f64() * 1000.0);
        println!("---------------------");
        println!("Total:         {:>8.3}ms", total.as_secs_f64() * 1000.0);
        println!("=====================");
    }
}

pub fn run_file(file_name: &str, source: &str, debug: bool, mode: &str, force: bool, show_timings: bool) {
    let mut timings = PhaseTimings::new();

    let t = std::time::Instant::now();
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let ast = parser.parse();
    timings.scan_parse = t.elapsed();

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
        if show_timings {
            timings.print();
        }
        return;
    }

    let natives = get_natives();
    let mut typechecker = TypeChecker::new_with_natives(&natives);

    let t = std::time::Instant::now();
    let analysis = typechecker.check(&ast);
    timings.type_checking = t.elapsed();

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
        if show_timings {
            timings.print();
        }
        return;
    }

    let mut gc = GarbageCollector::new();
    let t = std::time::Instant::now();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let func = compiler.compile(0, &typed_ast);
    timings.compilation = t.elapsed();

    if debug {
        println!("=== Disassembly ===");
        vm::disassembler::disassemble_chunk(&func.chunk, "main_script");
        println!("===================");
    }
    let (global_count, extern_fns) = match &typed_ast.kind {
        StmtKind::Global {
            global_count,
            extern_fns,
            ..
        } => (*global_count, extern_fns.clone()),
        _ => panic!("Global statement expected"),
    };
    drop(typechecker);
    let mut vm = VM::new(global_count as usize, &mut gc);
    vm.set_natives_by_name(&natives, &extern_fns);

    let t = std::time::Instant::now();
    let res = vm.run(func);
    timings.execution = t.elapsed();

    match res {
        Ok(_) => {}
        Err(err) => {
            println!("{err}");
        }
    }

    if show_timings {
        timings.print();
    }
}

pub fn execute_source(source: &str, debug: bool, mode: &str, force: bool) {
    let full_source = format!("{}{}", source, get_prelude());
    run_file("test.steel", &full_source, debug, mode, force, false);
}

/// A compiled Steel program that can be run multiple times. Used in benching
pub struct SteelProgram {
    func: Gc<Function>,
    gc: GarbageCollector,
    global_count: usize,
    extern_fns: Vec<(Box<str>, u16)>,
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

        let (global_count, extern_fns) = match &typed_ast.kind {
            StmtKind::Global {
                global_count,
                extern_fns,
                ..
            } => (*global_count as usize, extern_fns.clone()),
            _ => panic!("SteelProgram: expected Global statement"),
        };

        let mut gc = GarbageCollector::new();
        let compiler = Compiler::new("main".to_string(), &mut gc);
        let func = compiler.compile(0, &typed_ast);

        SteelProgram {
            func,
            gc,
            global_count,
            extern_fns,
        }
    }

    pub fn run_once(&mut self) {
        let func = self.func;
        let natives = get_natives();
        let mut vm = VM::new(self.global_count, &mut self.gc);
        vm.set_natives_by_name(&natives, &self.extern_fns);
        vm.run(func).expect("SteelProgram: runtime error");
        drop(vm);
        self.gc.collect_roots(self.func);
    }
}
