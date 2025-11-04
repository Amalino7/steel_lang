use crate::parser::ast::{Stmt, Type};
use crate::typechecker::error::TypeCheckerError;
use std::collections::HashMap;

pub mod error;
mod expressions;
mod return_analysis;
mod statements;
mod tests;

#[derive(Debug, PartialEq, Clone)]
enum FunctionContext {
    None,
    Function(Type),
}

struct VariableContext<'src> {
    type_: Type,
    name: &'src str,
    index: usize,
}

impl<'src> VariableContext<'src> {
    fn new(name: &'src str, type_: Type, index: usize) -> Self {
        VariableContext { type_, name, index }
    }
}

struct Scope<'src> {
    variables: HashMap<&'src str, VariableContext<'src>>,
    depth: usize,
}

pub struct TypeChecker<'src> {
    current_function: FunctionContext,
    variable_scope: Vec<Scope<'src>>,
}

impl<'src> TypeChecker<'src> {
    pub fn new() -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            variable_scope: vec![],
        }
    }
    pub fn check(&mut self, ast: &mut [Stmt<'src>]) -> Result<(), Vec<TypeCheckerError>> {
        let mut errors = vec![];
        self.begin_scope();
        self.declare_global_functions(ast, &mut errors); // First pass declare all global functions.

        for stmt in ast.iter_mut() {
            if let Err(e) = self.check_stmt(stmt) {
                errors.push(e);
            }
        }
        self.end_scope();

        self.check_returns(ast, &mut errors);
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }
    fn begin_scope(&mut self) {
        self.variable_scope.push(Scope {
            variables: Default::default(),
            depth: self.variable_scope.len(),
        });
    }
    fn end_scope(&mut self) {
        self.variable_scope.pop();
    }

    fn declare_variable(
        &mut self,
        name: &'src str,
        type_info: Type,
    ) -> Result<(), TypeCheckerError> {
        if let Some(scope) = self.variable_scope.last_mut() {
            scope.variables.insert(
                name,
                VariableContext::new(name, type_info, scope.variables.len()),
            );
        }
        Ok(())
    }

    fn lookup_variable(&mut self, name: &str) -> Option<&VariableContext> {
        for scope in self.variable_scope.iter().rev() {
            if let Some(var) = scope.variables.get(name) {
                return Some(var);
            }
        }
        None
    }
}
