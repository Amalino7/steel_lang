use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Stmt;
use crate::scanner::Span;
use crate::stdlib::NativeDef;
use crate::typechecker::scope::types::TypeScopeManager;
use crate::typechecker::scope::variables::Declaration;
use core::ast::{StmtKind, TypedStmt};
use core::error::{TypeCheckerError, TypeCheckerWarning};
use core::types::Type;
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
mod refinements;
pub(crate) mod resolver;
mod scope;
mod similarity;
pub(crate) mod system;
#[cfg(test)]
mod tests;

pub use crate::typechecker::id_issuer::{GlobalId, GlobalIdGenerator};
pub use core::types::Symbol;

pub struct TypeChecker<'ctx> {
    sys: &'ctx mut TypeSystem,
    scopes: ScopeManager<'ctx>,
    id_generator: &'ctx GlobalIdGenerator,
    type_scopes: TypeScopeManager,
    natives: &'ctx [NativeDef],
    errors: Vec<TypeCheckerError>,
    warnings: Vec<TypeCheckerWarning>,
    infer_ctx: InferenceContext,
}

impl<'ctx> TypeChecker<'ctx> {
    pub fn new(
        natives: &'ctx [NativeDef],
        sys: &'ctx mut TypeSystem,
        id_generator: &'ctx GlobalIdGenerator,
    ) -> Self {
        let mut ty_manager = TypeScopeManager::new();
        ty_manager.declare_global("List".into(), sys.view_builtins().list_id.into());
        ty_manager.declare_global("Map".into(), sys.view_builtins().map_id.into());

        TypeChecker {
            type_scopes: ty_manager,
            sys,
            scopes: ScopeManager::new(&id_generator),
            natives,
            errors: vec![],
            warnings: vec![],
            infer_ctx: InferenceContext::new(),
            id_generator,
        }
    }

    pub fn check(
        &mut self,
        ast: &[Stmt<'ctx>],
    ) -> Result<(TypedStmt, Vec<TypeCheckerWarning>), Vec<TypeCheckerError>> {
        self.scopes.begin_scope(ScopeKind::Global);
        let mut typed_ast = vec![];

        let native_slots = self.register_globals();

        // first types like structs and interfaces are declared
        let tasks = self.declare_global_types(ast);
        // define types, fields of structs and enums are defined and interface reqs
        self.define_types(tasks);

        // then global functions are declared
        let global_functions = self.declare_global_functions(ast, &mut typed_ast);

        self.define_global_functions(global_functions, &mut typed_ast);

        for stmt in ast.iter() {
            typed_ast.push(self.check_stmt(stmt));
        }

        let global_count = self.scopes.global_size();
        let reserved = self.scopes.max_index() as u16;

        let mut extern_fns = collect_extern_fns(&typed_ast);
        // Merge in name→slot pairs for vararg natives registered via register_globals.
        extern_fns.extend(native_slots);

        self.check_unreachable(&typed_ast);
        if !self.errors.is_empty() {
            Err(take(&mut self.errors))
        } else {
            Ok((
                TypedStmt {
                    kind: StmtKind::Global {
                        global_count,
                        stmts: typed_ast,
                        reserved,
                        extern_fns,
                    },
                    span: Span::default(),
                    type_info: Type::Void,
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
                let decl = Declaration::function(native.name.into(), ty.clone(), Span::default());
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
        TypeResolver::new(&self.sys, &self.type_scopes)
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
