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
pub(crate) mod inference;
mod refinements;
pub(crate) mod resolver;
mod return_analysis;
mod scope;
mod similarity;
pub(crate) mod system;
#[cfg(test)]
mod tests;

pub use core::types::Symbol;

pub struct TypeChecker<'src> {
    sys: TypeSystem,
    scopes: ScopeManager,
    type_scopes: TypeScopeManager,
    natives: &'src [NativeDef],
    errors: Vec<TypeCheckerError>,
    warnings: Vec<TypeCheckerWarning>,
    infer_ctx: InferenceContext,
}

impl<'src> Default for TypeChecker<'src> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'src> TypeChecker<'src> {
    // This is used for testing purposes only.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::new_with_natives(&[])
    }

    pub fn new_with_natives(natives: &'src [NativeDef]) -> Self {
        TypeChecker {
            type_scopes: TypeScopeManager::new(),
            sys: TypeSystem::new(),
            scopes: ScopeManager::new(),
            natives,
            errors: vec![],
            warnings: vec![],
            infer_ctx: InferenceContext::new(),
        }
    }

    pub fn check(
        &mut self,
        ast: &[Stmt<'src>],
    ) -> Result<(TypedStmt, Vec<TypeCheckerWarning>), Vec<TypeCheckerError>> {
        self.scopes.begin_scope(ScopeKind::Global);
        let mut typed_ast = vec![];

        let native_slots = self.register_globals(self.natives);

        // first types like structs and interfaces are declared
        self.declare_global_types(ast);
        // then global functions are declared and interfaces defined
        self.declare_global_functions(ast);
        // finally, fields of structs and enums are defined
        self.define_global_structs(ast);
        self.define_enum_variants(ast);

        for stmt in ast.iter() {
            typed_ast.push(self.check_stmt(stmt));
        }

        let global_count = self.scopes.global_size();
        let reserved = self.scopes.end_scope() as u16;

        let mut extern_fns = collect_extern_fns(&typed_ast);
        // Merge in name→slot pairs for vararg natives registered via register_globals.
        extern_fns.extend(native_slots);

        self.check_returns(&typed_ast);
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
    /// Returns name→slot pairs so the VM can bind them by name.
    fn register_globals(&mut self, natives: &[NativeDef]) -> Vec<(Box<str>, u16)> {
        let mut slots = vec![];
        for native in natives.iter() {
            if let Some(ty) = &native.type_ {
                let decl =
                    Declaration::function(native.name.into(), ty.clone(), Span::default());
                let resolved = self.scopes.declare(decl).expect("Failed to register global");
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
        match &stmt.kind {
            StmtKind::ExternFunction {
                name,
                target: ResolvedVar::Global(idx),
            } => result.push((name.clone(), *idx)),
            StmtKind::Impl { methods, .. } => {
                for method in methods.iter() {
                    if let StmtKind::ExternFunction {
                        name,
                        target: ResolvedVar::Global(idx),
                    } = &method.kind
                    {
                        result.push((name.clone(), *idx));
                    }
                }
            }
            _ => {}
        }
    }
    result
}
