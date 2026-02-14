use crate::parser::ast::Stmt;
use crate::scanner::Span;
use crate::stdlib::NativeDef;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::type_ast::{StmtKind, TypedStmt};
use crate::typechecker::type_resolver::TypeResolver;
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::Type;
use scope::scope_manager::{ScopeKind, ScopeManager};
use std::mem::take;
use std::rc::Rc;

mod arguments;
mod declarations;
pub mod error;
mod expressions;
mod get_handles;
mod inference;
mod operators;
mod pattern_matching;
mod refinements;
mod return_analysis;
mod scope;
mod statements;
mod tests;
pub mod type_ast;
mod type_resolver;
mod type_system;
mod type_system_old;
pub mod types;

#[derive(Debug, PartialEq, Clone)]
enum FunctionContext {
    None,
    Function(Type, Span),
}
pub type Symbol = Rc<str>;
pub struct TypeChecker<'src> {
    current_function: FunctionContext,
    sys: TypeSystem,
    scopes: ScopeManager,
    natives: &'src [NativeDef],
    errors: Vec<TypeCheckerError>,
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
            current_function: FunctionContext::None,
            sys: TypeSystem::new(),
            scopes: ScopeManager::new(),
            natives,
            errors: vec![],
            infer_ctx: InferenceContext::new(),
        }
    }

    pub fn check(&mut self, ast: &[Stmt<'src>]) -> Result<TypedStmt, Vec<TypeCheckerError>> {
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
            Ok(TypedStmt {
                kind: StmtKind::Global {
                    global_count,
                    stmts: typed_ast,
                    reserved,
                },
                span: Span::default(),
                type_info: Type::Void,
            })
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
        TypeResolver::new(&self.sys, &self.scopes)
    }
}
