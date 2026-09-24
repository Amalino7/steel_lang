use crate::resolver::ModuleResolver;
use crate::resolver::new_pipeline::pipeline;
use crate::resolver::resolution::Source;
use crate::{ColorChoice, EmitTarget, Mode, RunConfig};
use clap::{Parser, ValueEnum};
use notify::{EventKind, RecursiveMode, Watcher};
use std::sync::mpsc;

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

#[derive(ValueEnum, Clone, PartialEq)]
enum CliMode {
    /// Scan, parse, type-check, compile, and run
    Run,
    /// Scan, parse, and type-check only
    Check,
}

impl From<CliMode> for Mode {
    fn from(m: CliMode) -> Self {
        match m {
            CliMode::Run => Mode::Run,
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

fn build_config(cli: &Cli) -> RunConfig<'_> {
    RunConfig {
        file_name: &cli.file,
        source: Source::EntryFile(cli.file.as_ref()),
        mode: cli.mode.clone().into(),
        debug: cli.debug,
        include_prelude: !cli.no_stdlib,
        color: cli.color.into(),
        diagnostics: true,
        emit: cli.emit.iter().copied().map(EmitTarget::from).collect(),
        error_limit: cli.error_limit,
    }
}

pub fn watch_loop(cli: &Cli) {
    let config = build_config(cli);
    let graph = ModuleResolver::new().resolve_source(&config.source);

    let mut paths: Vec<_> = graph.modules.iter().map(|info| info.path.clone()).collect();

    let output = pipeline(&config);

    if cli.time {
        output.timings.print();
    }

    let (tx, rx) = mpsc::channel::<notify::Result<notify::Event>>();
    let mut watcher = notify::recommended_watcher(tx).unwrap_or_else(|e| {
        eprintln!("steel: cannot start file watcher: {}", e);
        std::process::exit(1);
    });

    for path in paths.iter() {
        watcher
            .watch(path.as_path(), RecursiveMode::NonRecursive)
            .unwrap_or_else(|e| {
                eprintln!("steel: cannot watch '{}': {}", cli.file, e);
                std::process::exit(1);
            });
    }

    loop {
        match rx.recv() {
            Ok(Ok(event)) if matches!(event.kind, EventKind::Modify(_) | EventKind::Create(_)) => {
                // Drain any rapid follow-up events (editors often emit several on save).
                std::thread::sleep(std::time::Duration::from_millis(50));
                while rx.try_recv().is_ok() {}
                let graph = ModuleResolver::new().resolve_source(&config.source);

                let new_paths: Vec<_> =
                    graph.modules.iter().map(|info| info.path.clone()).collect();

                if new_paths != paths {
                    for path in paths.iter() {
                        watcher.unwatch(path.as_path()).unwrap_or_else(|e| {
                            eprintln!("steel: cannot watch '{}': {}", cli.file, e);
                            std::process::exit(1);
                        });
                    }

                    for new_path in new_paths.iter() {
                        watcher
                            .watch(new_path.as_path(), RecursiveMode::NonRecursive)
                            .unwrap_or_else(|e| {
                                eprintln!("steel: cannot watch '{}': {}", cli.file, e);
                                std::process::exit(1);
                            });
                    }
                    paths = new_paths;
                }

                let output = pipeline(&config);

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

pub fn handle_input() {
    let cli = Cli::parse();

    if cli.watch {
        watch_loop(&cli);
    } else {
        let output = pipeline(&build_config(&cli));
        if cli.time {
            output.timings.print();
        }
        std::process::exit(output.result.exit_code());
    }
}
