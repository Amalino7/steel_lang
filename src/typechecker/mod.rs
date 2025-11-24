use crate::compiler::analysis::{AnalysisInfo, ResolvedVar};
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

#[derive(Debug, PartialEq, Clone)]
enum ScopeType {
    Global,
    Function,
    Block,
}
struct Scope<'src> {
    variables: HashMap<&'src str, VariableContext<'src>>,
    scope_type: ScopeType,
    last_index: usize,
}

pub struct TypeChecker<'src> {
    current_function: FunctionContext,
    variable_scope: Vec<Scope<'src>>,
    analysis_info: AnalysisInfo,
}

impl<'src> TypeChecker<'src> {
    pub fn new() -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            variable_scope: vec![],
            analysis_info: AnalysisInfo::new(),
        }
    }
    pub fn check(
        &mut self,
        ast: &mut [Stmt<'src>],
    ) -> Result<&AnalysisInfo, Vec<TypeCheckerError>> {
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
            Ok(&self.analysis_info)
        }
    }
    fn begin_function_scope(&mut self) {
        self.variable_scope.push(Scope {
            variables: Default::default(),
            scope_type: ScopeType::Function,
            last_index: 0,
        });
    }
    fn begin_scope(&mut self) {
        if let Some(scope) = self.variable_scope.last() {
            self.variable_scope.push(Scope {
                variables: Default::default(),
                scope_type: ScopeType::Block,
                last_index: if scope.scope_type != ScopeType::Global {
                    scope.last_index
                } else {
                    0
                },
            })
        } else {
            self.variable_scope.push(Scope {
                variables: Default::default(),
                scope_type: ScopeType::Global,
                last_index: 0,
            })
        }
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
                VariableContext::new(name, type_info, scope.last_index),
            );
            scope.last_index += 1;
        }
        Ok(())
    }

    fn lookup_variable(&self, name: &str) -> Option<(&VariableContext, ResolvedVar)> {
        let mut is_closure = false;
        for scope in self.variable_scope.iter().rev() {
            if let Some(var) = scope.variables.get(name) {
                return if scope.scope_type == ScopeType::Global {
                    Some((&var, ResolvedVar::Global(var.index)))
                } else if is_closure {
                    Some((&var, ResolvedVar::Closure(0)))
                } else {
                    Some((&var, ResolvedVar::Local(var.index)))
                };
            }

            if scope.scope_type == ScopeType::Function {
                is_closure = true;
            }
        }
        None
    }
}
