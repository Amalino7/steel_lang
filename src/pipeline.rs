use crate::compiler::Compiler;
use crate::diagnostics::DiagnosticContext;
use crate::diagnostics::ariadne::{cache, render};
use crate::diagnostics::emitter::{DiagnosticEmitter, DiagnosticSink};
use crate::parser::{Parser, printer};
use crate::resolver::{ModuleGraph, ModuleId, ModuleResolver};
use crate::scanner::Scanner;
use crate::stdlib::{NativeDef, get_natives, get_prelude};
use crate::typechecker::Exports;
use crate::typechecker::system::TypeSystem;
use crate::typechecker::{GlobalIdGenerator, TypeChecker};
use crate::vm::VM;
use crate::vm::gc::{GarbageCollector, Gc};
use crate::vm::value::Function;
use crate::{CompiledProgram, EmitTarget, Mode, PhaseTimings, RunConfig, RunOutput, RunResult, vm};
use ariadne::{Config, IndexType};
use std::time::Instant;

struct GlobalCtx {
    sys: TypeSystem,
    gc: GarbageCollector,
    id_generator: GlobalIdGenerator,
    diagnostics: DiagnosticEmitter,
    timings: PhaseTimings,
    compiled_files: Vec<Gc<Function>>,
    extern_fns: Vec<(Box<str>, u16)>,
}

pub fn pipeline(config: &RunConfig) -> RunOutput {
    let timings = PhaseTimings::new();
    let mut ctx = GlobalCtx {
        sys: TypeSystem::new(),
        gc: GarbageCollector::new(),
        id_generator: GlobalIdGenerator::new(),
        diagnostics: DiagnosticEmitter::new(),
        timings,
        compiled_files: vec![],
        extern_fns: vec![],
    };

    let mut res = if config.include_prelude {
        ModuleResolver::new_with_prelude(get_prelude())
    } else {
        ModuleResolver::new()
    };
    if config.portable {
        res.portable();
    }
    let graph = match res.resolve_source(&config.source) {
        Ok(graph) => graph,
        Err(err) => {
            if config.diagnostics {
                eprintln!("error: {}", err.message());
            }
            return RunOutput {
                program: None,
                result: RunResult::CompileError,
                timings: ctx.timings,
            };
        }
    };
    for err in res.errors {
        ctx.diagnostics.emit(
            err,
            &DiagnosticContext {
                type_system: &ctx.sys,
            },
        );
    }

    let natives = get_natives();

    // One entry per module, in graph order: modules only import earlier ones, and std comes first.
    let mut surfaces: Vec<Option<Exports>> = Vec::with_capacity(graph.modules.len());

    for module in &graph.modules {
        let errors_before = ctx.diagnostics.error_count();
        let exports = process_file(config, module.id, &natives, &mut ctx, &graph, &surfaces);
        surfaces.push(exports);

        // User modules are never checked against a broken std.
        if module.is_std && ctx.diagnostics.error_count() > errors_before {
            if config.diagnostics {
                render_diagnostics(config, &mut ctx, &graph);
                eprintln!("internal error: standard library failed to compile");
            }
            return RunOutput {
                program: None,
                result: RunResult::CompileError,
                timings: ctx.timings,
            };
        }
    }

    let has_errors = ctx.diagnostics.has_errors();
    if config.diagnostics {
        render_diagnostics(config, &mut ctx, &graph);
    }

    if has_errors {
        return RunOutput {
            program: None,
            result: RunResult::CompileError,
            timings: ctx.timings,
        };
    }

    if config.mode == Mode::Check {
        println!("Type checking has passed.");
        return RunOutput {
            program: None,
            result: RunResult::Ok,
            timings: ctx.timings,
        };
    }

    let compiler = Compiler::new("main".to_string(), &mut ctx.gc);
    let global_func = compiler.compile_many(ctx.compiled_files);

    let emit_bytecode = config.debug || config.emit.contains(&EmitTarget::Bytecode);
    if emit_bytecode {
        println!("=== Bytecode ===");
        vm::disassembler::disassemble_chunk(&global_func.chunk, "module_script");
        println!("================");
    }

    if config.mode == Mode::Compile {
        return RunOutput {
            program: Some(CompiledProgram {
                func: global_func,
                gc: ctx.gc,
                global_count: ctx.id_generator.count(),
                extern_fns: ctx.extern_fns,
                natives,
            }),
            result: RunResult::Ok,
            timings: ctx.timings,
        };
    }

    let t = Instant::now();
    let mut vm = VM::new(ctx.id_generator.count(), &mut ctx.gc);
    vm.set_natives_by_name(&natives, &ctx.extern_fns);

    let res = vm.run(global_func);

    ctx.timings.execution = t.elapsed();

    if let Err(err) = res {
        println!("{err}");
        return RunOutput {
            program: None,
            result: RunResult::RuntimeError,
            timings: ctx.timings,
        };
    }

    RunOutput {
        result: RunResult::Ok,
        program: None,
        timings: ctx.timings,
    }
}

/// Renders and clears every diagnostic collected so far, up to `--error-limit`.
fn render_diagnostics(config: &RunConfig, ctx: &mut GlobalCtx, graph: &ModuleGraph) {
    let ariadne_config = Config::default()
        .with_index_type(IndexType::Byte)
        .with_color(config.color.should_color());
    let mut cache = cache(graph);

    let limit = config.error_limit.unwrap_or(usize::MAX);
    let diagnostics = ctx.diagnostics.take_diagnostics();
    for diag in diagnostics.iter().take(limit) {
        // A closed stderr must not panic the compiler.
        let _ = render(diag, ariadne_config, graph).eprint(&mut cache);
    }
    if diagnostics.len() > limit {
        eprintln!(
            "... and {} more error(s) (--error-limit {})",
            diagnostics.len() - limit,
            limit
        );
    }
}

fn process_file(
    config: &RunConfig,
    mod_id: ModuleId,
    natives: &[NativeDef],
    ctx: &mut GlobalCtx,
    graph: &ModuleGraph,
    surfaces: &[Option<Exports>],
) -> Option<Exports> {
    let info = &graph.modules[mod_id.0 as usize];
    let mod_name = &info.name;
    let src = &info.source;
    let file_id = info.file_id;
    let is_std = info.is_std;

    // The standard library is only shown with `--debug`: every user would see it on every run.
    let emit_ast = config.debug || (!is_std && config.emit.contains(&EmitTarget::Ast));
    let emit_types = config.debug || (!is_std && config.emit.contains(&EmitTarget::Types));

    let t = Instant::now();
    let scanner = Scanner::new(src, file_id);
    let mut parser = Parser::new(scanner);

    let file = match parser.parse() {
        Ok(file) => file,
        Err(errs) => {
            for err in errs {
                ctx.diagnostics.emit(
                    err,
                    &DiagnosticContext {
                        type_system: &ctx.sys,
                    },
                )
            }
            return None;
        }
    };

    ctx.timings.scan_parse += t.elapsed();

    if emit_ast {
        println!("=== AST ===");
        print!("{}", printer::file_to_string(&file, false));
        println!("===========");
    }

    let t = Instant::now();
    let mut ty_checker = TypeChecker::new(
        natives,
        &mut ctx.sys,
        &ctx.id_generator,
        graph,
        surfaces,
        file_id,
    );

    match ty_checker.check(&file) {
        Ok((typed_file, warn)) => {
            // Std warnings are noise for users; only `--debug` shows them.
            for warn in warn.into_iter().filter(|_| !is_std || config.debug) {
                ctx.diagnostics.emit(
                    warn,
                    &DiagnosticContext {
                        type_system: &ctx.sys,
                    },
                )
            }
            ctx.timings.type_checking += t.elapsed();

            if emit_types {
                println!("=== Typed AST ===");
                println!("{:#?}", typed_file.file_ast);
                println!("=================");
            }

            // Nothing is compiled once anything failed, so a module never reaches the compiler next to errors.
            if config.mode == Mode::Check || ctx.diagnostics.has_errors() {
                return Some(typed_file.exports);
            }

            let t = Instant::now();

            let compiler = Compiler::new(mod_name.to_string(), &mut ctx.gc);
            ctx.compiled_files
                .push(compiler.compile(typed_file.reserved, &typed_file.file_ast));
            ctx.extern_fns.extend(typed_file.extern_fns);

            ctx.timings.compilation += t.elapsed();
            Some(typed_file.exports)
        }
        Err((errs, export)) => {
            for err in errs {
                ctx.diagnostics.emit(
                    err,
                    &DiagnosticContext {
                        type_system: &ctx.sys,
                    },
                )
            }
            Some(*export)
        }
    }
}
