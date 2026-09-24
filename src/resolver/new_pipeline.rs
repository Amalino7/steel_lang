use crate::compiler::Compiler;
use crate::diagnostics::DiagnosticContext;
use crate::diagnostics::ariadne::{cache, render};
use crate::diagnostics::emitter::{DiagnosticEmitter, DiagnosticSink};
use crate::parser::Parser;
use crate::resolver::{Exports, FileId, ModuleGraph, ModuleResolver};
use crate::scanner::Scanner;
use crate::stdlib::{NativeDef, get_natives, get_prelude};
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
    global_export: Option<Exports>,
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
        global_export: None,
        compiled_files: vec![],
        extern_fns: vec![],
    };

    let mut res = ModuleResolver::new();
    let mut graph_failed = false;
    let mut graph = res.resolve_source(&config.source);
    let errors = res.errors;

    if !errors.is_empty() {
        if config.diagnostics {
            for err in errors {
                eprintln!("{}", err.message());
            }
        }
        graph_failed = true;
    }

    let natives = get_natives();

    ctx.global_export = if config.include_prelude {
        let prelude = get_prelude();
        Some(process_file(
            config,
            "prelude",
            prelude,
            FileId(0),
            &natives,
            &mut ctx,
            &graph,
        ))
    } else {
        None
    };

    for idx in 0..graph.modules.len() {
        let exports = process_file(
            config,
            &graph.modules[idx].name,
            &graph.modules[idx].source,
            graph.modules[idx].file_id,
            &[],
            &mut ctx,
            &graph,
        );
        graph.modules[idx].exports = exports;
    }

    let ariadne_config = Config::default()
        .with_index_type(IndexType::Byte)
        .with_color(config.color.should_color());

    if ctx.diagnostics.has_errors() || graph_failed {
        if config.diagnostics {
            let mut cache = cache(&graph);

            let limit = config.error_limit.unwrap_or(usize::MAX);
            let diagnostics = ctx.diagnostics.take_diagnostics();
            for (idx, diag) in diagnostics.iter().enumerate() {
                if limit < idx {
                    eprintln!(
                        "... and {} more error(s) (--error-limit {})",
                        diagnostics.len() - limit,
                        limit
                    );
                }
                render(diag, ariadne_config, &graph)
                    .eprint(&mut cache)
                    .unwrap();
            }
        }

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

fn process_file(
    config: &RunConfig,
    mod_name: &str,
    src: &str,
    file_id: FileId,
    natives: &[NativeDef],
    ctx: &mut GlobalCtx,
    graph: &ModuleGraph,
) -> Exports {
    let emit_ast = config.debug || config.emit.contains(&EmitTarget::Ast);
    let emit_types = config.debug || config.emit.contains(&EmitTarget::Types);

    let t = Instant::now();
    let scanner = Scanner::new(src, file_id.0);
    let mut parser = Parser::new(scanner);

    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errs) => {
            for err in errs {
                ctx.diagnostics.emit(
                    err,
                    &DiagnosticContext {
                        type_system: &ctx.sys,
                    },
                )
            }
            vec![]
        }
    };

    ctx.timings.scan_parse += t.elapsed();

    if emit_ast {
        println!("=== AST ===");
        ast.iter().for_each(|e| println!("{}", e));
        println!("===========");
    }

    let t = Instant::now();
    let mut ty_checker = TypeChecker::new(natives, &mut ctx.sys, &ctx.id_generator, graph);

    match ty_checker.check(&ast, ctx.global_export.as_ref()) {
        Ok((typed_file, warn)) => {
            for warn in warn {
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

            if config.mode == Mode::Check {
                return typed_file.exports;
            }

            let t = Instant::now();

            let compiler = Compiler::new(mod_name.to_string(), &mut ctx.gc);
            ctx.compiled_files
                .push(compiler.compile(typed_file.reserved as u8, &typed_file.file_ast));
            ctx.extern_fns.extend(typed_file.extern_fns);

            ctx.timings.compilation += t.elapsed();
            typed_file.exports
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
            export
        }
    }
}
