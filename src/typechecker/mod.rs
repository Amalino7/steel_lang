use crate::parser::ast::Stmt;
use crate::stdlib::NativeDef;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::{ScopeManager, ScopeType};
use crate::typechecker::type_ast::{StmtKind, Type, TypedStmt};
use crate::typechecker::type_system::TypeSystem;
use std::mem::replace;
use std::rc::Rc;

pub mod error;
mod expressions;
mod get_handles;
mod globals;
mod refinements;
mod return_analysis;
mod scope_manager;
mod statements;
mod tests;
pub mod type_ast;
mod type_system;

#[derive(Debug, PartialEq, Clone)]
enum FunctionContext {
    None,
    Function(Type),
}
pub type Symbol = Rc<str>;

pub struct TypeChecker<'src> {
    current_function: FunctionContext,
    sys: TypeSystem,
    scopes: ScopeManager,
    natives: &'src [NativeDef],
    errors: Vec<TypeCheckerError>,
}

impl<'src> TypeChecker<'src> {
    // This is used for testing purposes only.
    #[allow(dead_code)]
    pub fn new() -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            sys: TypeSystem::new(),
            scopes: ScopeManager::new(),
            natives: &[],
            errors: vec![],
        }
    }

    pub fn new_with_natives(natives: &'src [NativeDef]) -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            sys: TypeSystem::new(),
            scopes: ScopeManager::new(),
            natives,
            errors: vec![],
        }
    }

    pub fn check(&mut self, ast: &[Stmt<'src>]) -> Result<TypedStmt, Vec<TypeCheckerError>> {
        self.scopes.begin_scope(ScopeType::Global);
        let mut typed_ast = vec![];

        self.register_globals(self.natives);

        // first types like structs and interfaces are declared
        self.declare_global_types(ast);
        // then global functions are declared and interfaces defined
        self.declare_global_functions(ast);
        // finally, fields of structs and enums are defined
        self.define_struct_fields(ast);
        self.define_enum_variants(ast);

        for stmt in ast.iter() {
            match self.check_stmt(stmt) {
                Ok(stmt) => {
                    typed_ast.push(stmt);
                }
                Err(e) => {
                    self.errors.push(e);
                }
            }
        }

        let global_count = self.scopes.global_size();

        self.check_returns(&typed_ast);
        if !self.errors.is_empty() {
            Err(replace(&mut self.errors, vec![]))
        } else {
            Ok(TypedStmt {
                kind: StmtKind::Global {
                    global_count,
                    stmts: typed_ast,
                },
                line: 1,
                type_info: Type::Void,
            })
        }
    }

    fn register_globals(&mut self, natives: &[NativeDef]) {
        for native in natives.iter() {
            self.scopes
                .declare(native.name.into(), native.type_.clone())
                .expect("Failed to register global");
        }
    }
}
