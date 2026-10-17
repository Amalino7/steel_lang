use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::ParsedFile;
use crate::scanner::Span;
use crate::stdlib::NativeDef;
use crate::typechecker::scope::binding::{Binding, Origin};
use crate::typechecker::scope::types::TypeScopeManager;
use crate::typechecker::scope::variables::Declaration;
use core::ast::{StmtKind, TypedStmt};
use core::error::{TypeCheckerError, TypeCheckerWarning};
use inference::InferenceContext;
use resolver::TypeResolver;
use scope::manager::{ScopeKind, ScopeManager};
use std::mem::take;
use system::TypeSystem;

mod check;
pub mod core;
mod flow_analysis;
pub mod id_issuer;
pub(crate) mod inference;
pub(crate) mod method_table;
mod methods;
pub(crate) mod modules;
mod refinements;
pub(crate) mod resolver;
pub(crate) mod scope;
mod similarity;
pub(crate) mod system;
#[cfg(test)]
mod tests;

use crate::resolver::{FileId, ModuleGraph};
use crate::typechecker::core::ast::FunctionBody;
use crate::typechecker::core::types::Type;
pub use crate::typechecker::id_issuer::{GlobalId, GlobalIdGenerator};
pub use crate::typechecker::modules::exports::Exports;
pub use core::types::Symbol;

pub struct TypeChecker<'ctx> {
    sys: &'ctx mut TypeSystem,
    scopes: ScopeManager<'ctx>,
    id_generator: &'ctx GlobalIdGenerator,
    module_graph: &'ctx ModuleGraph,
    surfaces: &'ctx [Option<Exports>],
    type_scopes: TypeScopeManager,
    natives: &'ctx [NativeDef],
    errors: Vec<TypeCheckerError>,
    warnings: Vec<TypeCheckerWarning>,
    infer_ctx: InferenceContext,
    file_id: FileId,
}

/// On failure the exports are still returned (boxed: they are large) so dependents can keep checking.
pub type CheckResult =
    Result<(TypedFile, Vec<TypeCheckerWarning>), (Vec<TypeCheckerError>, Box<Exports>)>;

#[derive(Debug)]
pub struct TypedFile {
    pub exports: Exports,
    pub reserved: u8,
    pub file_ast: FunctionBody,
    pub extern_fns: Vec<(Box<str>, u16)>,
}

impl<'ctx> TypeChecker<'ctx> {
    pub fn new(
        natives: &'ctx [NativeDef],
        sys: &'ctx mut TypeSystem,
        id_generator: &'ctx GlobalIdGenerator,
        module_graph: &'ctx ModuleGraph,
        surfaces: &'ctx [Option<Exports>],
        file_id: FileId,
    ) -> Self {
        let mut ty_manager = TypeScopeManager::new();
        ty_manager
            .declare_global(
                "List".into(),
                Binding::new(sys.view_builtins().list_id.into(), Origin::Builtin),
            )
            .expect("List is declared first, into an empty scope");
        ty_manager
            .declare_global(
                "Map".into(),
                Binding::new(sys.view_builtins().map_id.into(), Origin::Builtin),
            )
            .expect("Map is declared right after List, which has a different name");

        TypeChecker {
            type_scopes: ty_manager,
            sys,
            scopes: ScopeManager::new(id_generator),
            natives,
            errors: vec![],
            warnings: vec![],
            infer_ctx: InferenceContext::new(),
            id_generator,
            module_graph,
            surfaces,
            file_id,
        }
    }

    pub fn check(&mut self, file: &ParsedFile<'ctx>) -> CheckResult {
        let ast = &file.body[..];
        self.scopes.begin_scope(ScopeKind::Global);

        self.import_std();

        let mut typed_ast = vec![];

        let native_slots = self.register_globals();

        self.imports(&file.imports);
        // first types like structs and interfaces are declared
        let tasks = self.declare_global_types(ast);
        // define types, fields of structs and enums are defined and interface reqs
        self.define_types(tasks);

        // then global functions are declared
        let global_functions = self.declare_global_functions(ast, &mut typed_ast);

        for stmt in ast.iter() {
            typed_ast.push(self.check_stmt(stmt));
        }

        self.define_global_functions(global_functions, &mut typed_ast);

        let module_span = file
            .imports
            .first()
            .map(|i| i.span())
            .or_else(|| ast.first().map(|s| s.span()))
            .unwrap_or_default();
        let reserved = self.limit_locals(self.scopes.max_index(), module_span);

        let mut extern_fns = collect_extern_fns(&typed_ast);
        // Merge in name→slot pairs for vararg natives registered via register_globals.
        extern_fns.extend(native_slots);

        self.check_unreachable(&typed_ast);

        let exports = self.get_exports();
        if self.errors.is_empty() {
            self.check_leaks(&exports);
        }

        if !self.errors.is_empty() {
            Err((take(&mut self.errors), Box::new(exports)))
        } else {
            Ok((
                TypedFile {
                    exports,
                    reserved,
                    file_ast: FunctionBody::Block(Box::new(TypedStmt {
                        span: typed_ast
                            .first()
                            .map(|s| s.span)
                            .unwrap_or(Span::default())
                            .merge(typed_ast.last().map(|s| s.span).unwrap_or(Span::default())),

                        kind: StmtKind::Global { stmts: typed_ast },
                        type_info: Type::Void,
                    })),
                    extern_fns,
                },
                take(&mut self.warnings),
            ))
        }
    }

    fn import_std(&mut self) {
        let Some(std) = self.module_graph.std_module() else {
            return;
        };
        if std.file_id == self.file_id {
            return;
        }
        let exports = self
            .surfaces
            .get(std.id.0 as usize)
            .and_then(Option::as_ref)
            .expect("the pipeline stops before user modules when std fails to compile");
        self.import_all(exports, Span::new(0, 0, 0, self.file_id));
    }

    /// Whether the module being checked is the standard library.
    fn is_std(&self) -> bool {
        self.module_graph
            .file_to_module_id
            .get(&self.file_id)
            .and_then(|id| self.module_graph.modules.get(id.0 as usize))
            .is_some_and(|module| module.is_std)
    }

    /// An `extern func` is std-only and must name an existing native (`Type.method` for `impl` externs).
    pub(crate) fn check_extern(&mut self, native_name: &str, span: Span) {
        if !self.is_std() {
            self.report(TypeCheckerError::ExternOutsideStd { span });
        } else if !self.natives.iter().any(|native| native.name == native_name) {
            self.report(TypeCheckerError::UnknownNative {
                name: native_name.to_string(),
                span,
            });
        }
    }

    /// Registers only natives that carry an explicit type (e.g. vararg functions).
    /// Returns name->slot pairs so the VM can bind them by name.
    fn register_globals(&mut self) -> Vec<(Box<str>, u16)> {
        if !self.is_std() {
            return vec![];
        }

        let mut slots = vec![];
        for native in self.natives.iter() {
            if let Some(ty) = &native.type_ {
                // Natives are part of the prelude surface.
                let decl = Declaration::function(native.name.into(), ty.clone(), Span::builtin())
                    .public(true);
                let resolved = self
                    .scopes
                    .declare(decl)
                    .expect("Failed to register global");
                if let ResolvedVar::Global(idx) = resolved {
                    slots.push((native.name.into(), idx));
                }
            }
        }
        slots
    }

    fn res(&self) -> TypeResolver<'_> {
        TypeResolver::new(self.sys, &self.type_scopes)
    }

    pub(crate) fn report(&mut self, err: TypeCheckerError) {
        self.errors.push(err);
    }

    pub(crate) fn warn(&mut self, warning: TypeCheckerWarning) {
        self.warnings.push(warning);
    }
}

/// Local slots a frame can address: the compiler emits slots and `Reserve` amounts as a `u8`.
pub(crate) const MAX_LOCALS: usize = u8::MAX as usize;

fn collect_extern_fns(stmts: &[TypedStmt]) -> Vec<(Box<str>, u16)> {
    let mut result = vec![];
    for stmt in stmts {
        if let StmtKind::ExternFunction {
            name,
            target: ResolvedVar::Global(idx),
        } = &stmt.kind
        {
            result.push((name.clone(), *idx))
        }
    }
    result
}
