use crate::{run, ColorChoice, EmitTarget, Mode, RunConfig};
use clap::{Parser, ValueEnum};
use std::fs;
use std::path::Path;

/// The Steel language runtime
#[derive(Parser)]
#[command(name = "steel", version, about, long_about = None)]
pub struct Cli {
    /// Source file to execute
    file: String,

    /// Execution mode
    #[arg(value_enum, default_value_t = CliMode::Run)]
    mode: CliMode,

    /// Suppress error reporting and continue past parse/type errors.
    #[arg(short, long)]
    force: bool,

    /// Show debug output (AST, disassembly, typed AST). Equivalent to --emit ast,bytecode,types.
    #[arg(short, long)]
    debug: bool,

    /// Print per-phase timing information after execution
    #[arg(short, long)]
    time: bool,

    /// Run without the standard library prelude
    #[arg(long)]
    no_stdlib: bool,

    /// Control ANSI colour output in diagnostics
    #[arg(long, value_enum, default_value_t = CliColor::Auto)]
    color: CliColor,

    /// Emit extra information: ast, types, bytecode (comma-separated or repeated)
    #[arg(long, value_enum, value_delimiter = ',')]
    emit: Vec<CliEmit>,

    /// Maximum number of errors to display
    #[arg(long, value_name = "N")]
    error_limit: Option<usize>,

    /// Re-run the program whenever the source file changes
    #[arg(short = 'w', long)]
    watch: bool,
}

#[derive(ValueEnum, Clone)]
enum CliMode {
    /// Scan, parse, type-check, compile and run
    Run,
    /// Scan and parse only (prints AST with --debug or --emit ast)
    Parse,
    /// Scan, parse and type-check only
    Check,
}

impl From<CliMode> for Mode {
    fn from(m: CliMode) -> Self {
        match m {
            CliMode::Run => Mode::Run,
            CliMode::Parse => Mode::Parse,
            CliMode::Check => Mode::Check,
        }
    }
}

#[derive(ValueEnum, Clone, Copy)]
enum CliColor {
    /// Emit colour when stdout is a TTY
    Auto,
    /// Always emit colour
    Always,
    /// Never emit colour
    Never,
}

impl From<CliColor> for ColorChoice {
    fn from(c: CliColor) -> Self {
        match c {
            CliColor::Auto => ColorChoice::Auto,
            CliColor::Always => ColorChoice::Always,
            CliColor::Never => ColorChoice::Never,
        }
    }
}

#[derive(ValueEnum, Clone, Copy, PartialEq, Eq)]
enum CliEmit {
    /// Untyped AST produced by the parser
    Ast,
    /// Typed AST produced by the type checker
    Types,
    /// Bytecode disassembly produced by the compiler
    Bytecode,
}

impl From<CliEmit> for EmitTarget {
    fn from(e: CliEmit) -> Self {
        match e {
            CliEmit::Ast => EmitTarget::Ast,
            CliEmit::Types => EmitTarget::Types,
            CliEmit::Bytecode => EmitTarget::Bytecode,
        }
    }
}

fn build_config<'a>(cli: &'a Cli, source: &'a str) -> RunConfig<'a> {
    RunConfig {
        file_name: &cli.file,
        source,
        mode: cli.mode.clone().into(),
        debug: cli.debug,
        force: cli.force,
        include_prelude: !cli.no_stdlib,
        color: cli.color.into(),
        emit: cli.emit.iter().copied().map(EmitTarget::from).collect(),
        error_limit: cli.error_limit,
    }
}

pub fn watch_loop(cli: &Cli) {
    use notify::{EventKind, RecursiveMode, Watcher};
    use std::sync::mpsc;

    let source = read_source(&cli.file);
    let output = run(&build_config(cli, &source));
    if cli.time {
        output.timings.print();
    }

    let (tx, rx) = mpsc::channel::<notify::Result<notify::Event>>();
    let mut watcher = notify::recommended_watcher(tx).unwrap_or_else(|e| {
        eprintln!("steel: cannot start file watcher: {}", e);
        std::process::exit(1);
    });
    watcher
        .watch(Path::new(&cli.file), RecursiveMode::NonRecursive)
        .unwrap_or_else(|e| {
            eprintln!("steel: cannot watch '{}': {}", cli.file, e);
            std::process::exit(1);
        });

    loop {
        match rx.recv() {
            Ok(Ok(event)) if matches!(event.kind, EventKind::Modify(_) | EventKind::Create(_)) => {
                // Drain any rapid follow-up events (editors often emit several on save).
                std::thread::sleep(std::time::Duration::from_millis(50));
                while rx.try_recv().is_ok() {}

                eprintln!("\n=== {} changed ===\n", cli.file);
                let source = read_source(&cli.file);
                let output = run(&build_config(cli, &source));
                if cli.time {
                    output.timings.print();
                }
            }
            Ok(Err(e)) => eprintln!("steel: watch error: {}", e),
            Ok(_) => {}
            Err(_) => break,
        }
    }
}

pub fn read_source(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("steel: cannot read '{}': {}", path, e);
        std::process::exit(1);
    })
}

pub fn handle_input() {
    let cli = Cli::parse();
    if cli.watch {
        watch_loop(&cli);
    } else {
        let source = read_source(&cli.file);
        let output = run(&build_config(&cli, &source));
        if cli.time {
            output.timings.print();
        }
        std::process::exit(output.result.exit_code());
    }
}
