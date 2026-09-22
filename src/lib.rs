#![allow(clippy::uninlined_format_args)]

pub mod cli;
pub mod compiler;
mod diagnostics;
pub mod parser;
pub mod resolver;
pub mod scanner;
pub mod stdlib;
pub mod typechecker;
pub mod vm;

use crate::resolver::resolution::Source;
use crate::stdlib::NativeDef;
use crate::vm::gc::{GarbageCollector, Gc};
use crate::vm::value::Function;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Run,
    Check,
    Compile,
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
#[derive(Debug, PartialEq)]
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
    pub source: Source<'a>,
    pub mode: Mode,
    pub debug: bool,
    pub include_prelude: bool,
    pub diagnostics: bool,
    pub color: ColorChoice,
    pub emit: Vec<EmitTarget>,
    pub error_limit: Option<usize>,
}

impl<'a> RunConfig<'a> {
    pub fn new(
        file_name: &'a str,
        source: Source<'a>,
        mode: Mode,
        diagnostics: bool,
    ) -> RunConfig<'a> {
        RunConfig {
            file_name,
            source,
            mode,
            debug: false,
            include_prelude: true,
            diagnostics,
            color: ColorChoice::Auto,
            emit: vec![],
            error_limit: None,
        }
    }
}

/// Output returned by [`run`], containing the result and per-phase timings.
pub struct RunOutput {
    pub result: RunResult,
    pub timings: PhaseTimings,
    pub program: Option<CompiledProgram>,
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

/// A compiled Steel program
pub struct CompiledProgram {
    pub func: Gc<Function>,
    pub gc: GarbageCollector,
    pub global_count: usize,
    pub extern_fns: Vec<(Box<str>, u16)>,
    pub natives: Vec<NativeDef>,
}
