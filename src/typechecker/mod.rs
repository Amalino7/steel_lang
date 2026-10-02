use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Stmt;
use crate::scanner::Span;
use crate::stdlib::NativeDef;
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
mod modules;
mod refinements;
pub(crate) mod resolver;
pub(crate) mod scope;
mod similarity;
pub(crate) mod system;
#[cfg(test)]
mod tests;

use crate::resolver::{Exports, FileId, ModuleGraph};
use crate::typechecker::core::ast::FunctionBody;
use crate::typechecker::core::types::Type;
pub use crate::typechecker::id_issuer::{GlobalId, GlobalIdGenerator};
pub use core::types::Symbol;

pub struct TypeChecker<'ctx> {
    sys: &'ctx mut TypeSystem,
    scopes: ScopeManager<'ctx>,
    id_generator: &'ctx GlobalIdGenerator,
    module_graph: &'ctx ModuleGraph,
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
    pub reserved: u16,
    pub file_ast: FunctionBody,
    pub extern_fns: Vec<(Box<str>, u16)>,
}

impl<'ctx> TypeChecker<'ctx> {
    pub fn new(
        natives: &'ctx [NativeDef],
        sys: &'ctx mut TypeSystem,
        id_generator: &'ctx GlobalIdGenerator,
        module_graph: &'ctx ModuleGraph,
        file_id: FileId,
    ) -> Self {
        let mut ty_manager = TypeScopeManager::new();
        ty_manager
            .declare_global("List".into(), sys.view_builtins().list_id.into())
            .expect("Map Should be empty!");
        ty_manager
            .declare_global("Map".into(), sys.view_builtins().map_id.into())
            .expect("Map Should be empty!");

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
            file_id,
        }
    }

    pub fn check(&mut self, ast: &[Stmt<'ctx>], exports: Option<&Exports>) -> CheckResult {
        self.scopes.begin_scope(ScopeKind::Global);

        if let Some(exports) = exports {
            self.import_all(exports, system::PRELUDE_FILE, None);
        }

        let mut typed_ast = vec![];

        let native_slots = self.register_globals();

        self.imports(ast);
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

        let reserved = self.scopes.max_index() as u16;

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

    /// Registers only natives that carry an explicit type (e.g. vararg functions).
    /// Returns name->slot pairs so the VM can bind them by name.
    fn register_globals(&mut self) -> Vec<(Box<str>, u16)> {
        let mut slots = vec![];
        for native in self.natives.iter() {
            if let Some(ty) = &native.type_ {
                // Natives are part of the prelude surface; their default span is file 0.
                let decl = Declaration::function(native.name.into(), ty.clone(), Span::default())
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
