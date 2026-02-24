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

        self.register_globals(self.natives);

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
                    },
                    span: Span::default(),
                    type_info: Type::Void,
                },
                take(&mut self.warnings),
            ))
        }
    }

    fn register_globals(&mut self, natives: &[NativeDef]) {
        for native in natives.iter() {
            let decl =
                Declaration::function(native.name.into(), native.type_.clone(), Span::default());
            self.scopes
                .declare(decl)
                .expect("Failed to register global");
        }
    }

    fn res(&self) -> TypeResolver {
        TypeResolver::new(&self.sys, &self.type_scopes)
    }
}
