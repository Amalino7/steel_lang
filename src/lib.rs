#![allow(clippy::uninlined_format_args)]

pub mod cli;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Run,
    Parse,
    Check,
}

/// Controls ANSI colour output in diagnostic messages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorChoice {
    Auto,
    Always,
    Never,
}

impl ColorChoice {
    fn should_color(self) -> bool {
        use std::io::IsTerminal;
        match self {
            ColorChoice::Always => true,
            ColorChoice::Never => false,
            ColorChoice::Auto => std::io::stdout().is_terminal(),
        }
    }
}

/// Extra information to emit alongside normal output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmitTarget {
    Ast,
    Types,
    Bytecode,
}

/// Outcome of running a Steel program.
#[derive(Debug)]
pub enum RunResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl RunResult {
    pub fn exit_code(&self) -> i32 {
        match self {
            RunResult::Ok => 0,
            RunResult::CompileError => 1,
            RunResult::RuntimeError => 2,
        }
    }
}

/// Configuration for running a Steel program.
pub struct RunConfig<'a> {
    pub file_name: &'a str,
    pub source: &'a str,
    pub mode: Mode,
    pub debug: bool,
    pub force: bool,
    pub include_prelude: bool,
    pub color: ColorChoice,
    pub emit: Vec<EmitTarget>,
    pub error_limit: Option<usize>,
}

/// Output returned by [`run`], containing the result and per-phase timings.
pub struct RunOutput {
    pub result: RunResult,
    pub timings: PhaseTimings,
}

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
        println!(
            "Scan + Parse:  {:>8.3}ms",
            self.scan_parse.as_secs_f64() * 1000.0
        );
        println!(
            "Type checking: {:>8.3}ms",
            self.type_checking.as_secs_f64() * 1000.0
        );
        println!(
            "Compilation:   {:>8.3}ms",
            self.compilation.as_secs_f64() * 1000.0
        );
        println!(
            "Execution:     {:>8.3}ms",
            self.execution.as_secs_f64() * 1000.0
        );
        println!("---------------------");
        println!("Total:         {:>8.3}ms", total.as_secs_f64() * 1000.0);
        println!("=====================");
    }
}

pub fn run(config: &RunConfig) -> RunOutput {
    let owned;
    let source = if config.include_prelude {
        owned = format!("{}{}", config.source, get_prelude());
        owned.as_str()
    } else {
        config.source
    };
    run_inner(config, source)
}

fn run_inner(config: &RunConfig, source: &str) -> RunOutput {
    fn error_limit(total: usize, limit: Option<usize>) {
        if let Some(limit) = limit
            && total > limit
        {
            eprintln!(
                "... and {} more error(s) (--error-limit {})",
                total - limit,
                limit
            );
        }
    }

    let mut timings = PhaseTimings::new();
    let use_color = config.color.should_color();
    let emit_ast = config.debug || config.emit.contains(&EmitTarget::Ast);
    let emit_types = config.debug || config.emit.contains(&EmitTarget::Types);
    let emit_bytecode = config.debug || config.emit.contains(&EmitTarget::Bytecode);
    let ariadne_config = Config::default()
        .with_index_type(IndexType::Byte)
        .with_color(use_color);

    let t = std::time::Instant::now();
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let ast = parser.parse();
    timings.scan_parse = t.elapsed();

    if !config.force
        && let Err(errors) = &ast
    {
        let limit = config.error_limit.unwrap_or(usize::MAX);
        for err in errors.iter().take(limit) {
            let span = err.span();
            let span_range = span.start..span.end;

            Report::build(ReportKind::Error, config.file_name, span.start)
                .with_config(ariadne_config)
                .with_message("Syntax Error")
                .with_label(
                    Label::new((config.file_name, span_range))
                        .with_message(err.message())
                        .with_color(Color::Red),
                )
                .finish()
                .print((config.file_name, Source::from(source)))
                .unwrap();
        }
        error_limit(errors.len(), config.error_limit);

        return RunOutput {
            result: RunResult::CompileError,
            timings,
        };
    }
    let ast = ast.unwrap();

    if emit_ast {
        println!("=== AST ===");
        ast.iter().for_each(|e| println!("{}", e));
        println!("===========");
    }

    if config.mode == Mode::Parse {
        return RunOutput {
            result: RunResult::Ok,
            timings,
        };
    }

    let natives = get_natives();
    let mut typechecker = TypeChecker::new_with_natives(&natives);

    let t = std::time::Instant::now();
    let analysis = typechecker.check(&ast);
    timings.type_checking = t.elapsed();

    if !config.force
        && let Err(errors) = &analysis
    {
        let limit = config.error_limit.unwrap_or(usize::MAX);
        for err in errors.iter().take(limit) {
            err.create_report(config.file_name, ariadne_config)
                .print((config.file_name, Source::from(source)))
                .unwrap();
        }
        error_limit(errors.len(), config.error_limit);
        return RunOutput {
            result: RunResult::CompileError,
            timings,
        };
    }

    let (typed_ast, warnings) = analysis.unwrap();

    for warning in &warnings {
        warning
            .create_report(config.file_name, ariadne_config)
            .print((config.file_name, Source::from(source)))
            .unwrap();
    }

    if emit_types {
        println!("=== Typed AST ===");
        println!("{typed_ast:#?}");
        println!("=================");
    }

    if config.mode == Mode::Check {
        println!("Type checking has passed.");
        return RunOutput {
            result: RunResult::Ok,
            timings,
        };
    }

    let mut gc = GarbageCollector::new();
    let t = std::time::Instant::now();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let func = compiler.compile(0, &typed_ast);
    timings.compilation = t.elapsed();

    if emit_bytecode {
        println!("=== Bytecode ===");
        vm::disassembler::disassemble_chunk(&func.chunk, "main_script");
        println!("================");
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
        Ok(_) => RunOutput {
            result: RunResult::Ok,
            timings,
        },
        Err(err) => {
            println!("{err}");
            RunOutput {
                result: RunResult::RuntimeError,
                timings,
            }
        }
    }
}

pub fn execute_source(source: &str, debug: bool, mode: Mode, force: bool) -> RunResult {
    run(&RunConfig {
        file_name: "test.steel",
        source,
        mode,
        debug,
        force,
        include_prelude: true,
        color: ColorChoice::Auto,
        emit: vec![],
        error_limit: None,
    })
    .result
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
