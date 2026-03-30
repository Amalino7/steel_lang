use clap::{Parser, ValueEnum};
use steel_lang::run_file;
use steel_lang::stdlib;
use std::fs;

/// The Steel language runtime
#[derive(Parser)]
#[command(name = "steel", version, about, long_about = None)]
struct Cli {
    /// Source file to execute
    file: String,

    /// Execution mode
    #[arg(value_enum, default_value_t = Mode::Run)]
    mode: Mode,

    /// Force execution despite parse or type errors
    #[arg(short, long)]
    force: bool,

    /// Show debug output (AST, disassembly)
    #[arg(short, long)]
    debug: bool,

    /// Print per-phase timing information after execution
    #[arg(short, long)]
    timings: bool,
}

#[derive(ValueEnum, Clone)]
enum Mode {
    /// Scan, parse, type-check, compile and run
    Run,
    /// Scan and parse only (prints AST with --debug)
    Parse,
    /// Scan, parse and type-check only
    Check,
}

fn main() {
    let cli = Cli::parse();

    let mut source = fs::read_to_string(&cli.file)
        .unwrap_or_else(|e| {
            eprintln!("steel: cannot read '{}': {}", cli.file, e);
            std::process::exit(1);
        });
    source += stdlib::get_prelude();

    let mode = match cli.mode {
        Mode::Run => "run",
        Mode::Parse => "parse",
        Mode::Check => "check",
    };

    run_file(&cli.file, source.as_str(), cli.debug, mode, cli.force, cli.timings);
}
