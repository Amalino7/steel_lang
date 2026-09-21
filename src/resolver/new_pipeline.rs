use crate::compiler::Compiler;
use crate::diagnostics::DiagnosticContext;
use crate::diagnostics::ariadne::render;
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
use ariadne::{Config, IndexType};
use std::path::Path;

struct GlobalCtx {
    sys: TypeSystem,
    gc: GarbageCollector,
    id_generator: GlobalIdGenerator,
    diagnostics: DiagnosticEmitter,
    compiled_files: Vec<Gc<Function>>,
    extern_fns: Vec<(Box<str>, u16)>,
}

pub fn pipeline(entry_file: &Path) {
    let mut res = ModuleResolver::new();
    let mut graph = res.resolve(entry_file).unwrap(); // TODO errors
    let mut ctx = GlobalCtx {
        sys: TypeSystem::new(),
        gc: GarbageCollector::new(),
        id_generator: GlobalIdGenerator::new(),
        diagnostics: DiagnosticEmitter::new(),
        compiled_files: vec![],
        extern_fns: vec![],
    };

    let natives = get_natives();

    let exports = {
        let prelude = get_prelude();
        process_file(prelude, FileId(0), &natives, &mut ctx, &graph, None)
    };

    for idx in 0..graph.modules.len() {
        let exports = process_file(
            &graph.modules[idx].source,
            graph.modules[idx].file_id,
            &[],
            &mut ctx,
            &graph,
            Some(&exports),
        );
        graph.modules[idx].exports = exports;
    }

    let ariadne_config = Config::default()
        .with_index_type(IndexType::Byte)
        .with_color(true);

    for diag in ctx.diagnostics.take_diagnostics() {
        render(&diag, ariadne_config, &graph);
    }

    let compiler = Compiler::new("main".to_string(), &mut ctx.gc);
    let global_func = compiler.compile_many(ctx.compiled_files);

    let mut vm = VM::new(ctx.id_generator.count(), &mut ctx.gc);
    vm.set_natives_by_name(&natives, &ctx.extern_fns);

    vm.run(global_func).unwrap();
}

fn process_file(
    src: &str,
    file_id: FileId,
    natives: &[NativeDef],
    ctx: &mut GlobalCtx,
    graph: &ModuleGraph,
    exports: Option<&Exports>,
) -> Exports {
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

    let mut ty_checker = TypeChecker::new(&natives, &mut ctx.sys, &ctx.id_generator, graph);

    match ty_checker.check(&ast, exports) {
        Ok((typed_file, warn)) => {
            for warn in warn {
                ctx.diagnostics.emit(
                    warn,
                    &DiagnosticContext {
                        type_system: &ctx.sys,
                    },
                )
            }

            let compiler = Compiler::new("prelude".to_string(), &mut ctx.gc);
            ctx.compiled_files
                .push(compiler.compile(typed_file.reserved as u8, &typed_file.file_ast));
            ctx.extern_fns.extend(typed_file.extern_fns);
            typed_file.exports
        }
        Err(errs) => {
            for err in errs {
                ctx.diagnostics.emit(
                    err,
                    &DiagnosticContext {
                        type_system: &ctx.sys,
                    },
                )
            }
            Exports::new()
        }
    }
}
