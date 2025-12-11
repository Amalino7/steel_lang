use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Stmt;
use crate::stdlib::NativeDef;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{StmtKind, StructType, Type, TypedStmt};
use std::borrow::Cow;
use std::collections::HashMap;

pub mod error;
mod expressions;
mod return_analysis;
mod statements;
mod tests;
pub mod type_ast;

#[derive(Debug, PartialEq, Clone)]
enum FunctionContext {
    None,
    Function(Type),
}

struct VariableContext<'src> {
    type_info: Type,
    name: &'src str,
    index: usize,
}

impl<'src> VariableContext<'src> {
    fn new(name: &'src str, type_info: Type, index: usize) -> Self {
        VariableContext {
            type_info,
            name,
            index,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum ScopeType {
    Global,
    Function,
    Block,
}
struct Scope<'src> {
    variables: HashMap<Cow<'src, str>, VariableContext<'src>>,
    scope_type: ScopeType,
    last_index: usize,
}

pub struct TypeChecker<'src> {
    current_function: FunctionContext,
    structs: HashMap<&'src str, StructType>,
    variable_scope: Vec<Scope<'src>>,
    natives: &'src [NativeDef],
    closures: Vec<Cow<'src, str>>,
}

impl<'src> TypeChecker<'src> {
    // This is used for testing purposes only.
    #[allow(dead_code)]
    pub fn new() -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            structs: HashMap::new(),
            variable_scope: vec![],
            natives: &[],
            closures: vec![],
        }
    }
    pub fn new_with_natives(natives: &'src [NativeDef]) -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            structs: HashMap::new(),
            variable_scope: vec![],
            natives,
            closures: vec![],
        }
    }

    pub fn check(&mut self, ast: &[Stmt<'src>]) -> Result<TypedStmt, Vec<TypeCheckerError>> {
        let mut errors = vec![];

        self.declare_global_structs(ast, &mut errors);

        self.begin_scope();
        let mut typed_ast = vec![];

        self.register_globals(self.natives);
        self.declare_global_functions(ast, &mut errors); // First pass declare all global functions.
        for stmt in ast.iter() {
            match self.check_stmt(stmt) {
                Ok(stmt) => {
                    typed_ast.push(stmt);
                }
                Err(e) => {
                    errors.push(e);
                }
            }
        }
        let global_count = self.variable_scope.last().unwrap().variables.len() as u32;
        self.end_scope();

        self.check_returns(&typed_ast, &mut errors);
        if !errors.is_empty() {
            Err(errors)
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
    fn register_globals(&mut self, natives: &[NativeDef]) {
        for native in natives.iter() {
            self.declare_variable(native.name, native.type_.clone())
                .expect("Failed to register global");
        }
    }

    fn declare_mangled(
        &mut self,
        mangled_name: String,
        surface_name: &'src str,
        type_info: Type,
    ) -> Result<(), TypeCheckerError> {
        if let Some(scope) = self.variable_scope.last_mut() {
            scope.variables.insert(
                Cow::Owned(mangled_name),
                VariableContext::new(surface_name, type_info, scope.last_index),
            );
            scope.last_index += 1;
        }
        Ok(())
    }

    fn declare_variable(
        &mut self,
        name: &'src str,
        type_info: Type,
    ) -> Result<(), TypeCheckerError> {
        if let Some(scope) = self.variable_scope.last_mut() {
            scope.variables.insert(
                Cow::Borrowed(name),
                VariableContext::new(name, type_info, scope.last_index),
            );
            scope.last_index += 1;
        }
        Ok(())
    }

    fn lookup_variable(
        &mut self,
        name: Cow<'src, str>,
    ) -> Option<(&VariableContext<'src>, ResolvedVar)> {
        let mut is_closure = false;
        for scope in self.variable_scope.iter().rev() {
            if let Some(var) = scope.variables.get(&name) {
                return if scope.scope_type == ScopeType::Global {
                    Some((&var, ResolvedVar::Global(var.index as u16)))
                } else if is_closure {
                    // Find if the closure is already declared.
                    for (i, closure) in self.closures.iter().enumerate() {
                        if *closure == name {
                            return Some((&var, ResolvedVar::Closure(i as u8)));
                        }
                    }
                    self.closures.push(name);
                    Some((&var, ResolvedVar::Closure((self.closures.len() - 1) as u8)))
                } else {
                    Some((&var, ResolvedVar::Local(var.index as u8)))
                };
            }

            if scope.scope_type == ScopeType::Function {
                is_closure = true;
            }
        }
        None
    }

    fn does_type_exist(&self, name: &str) -> bool {
        if self.structs.contains_key(name)
            || name == "string"
            || name == "number"
            || name == "boolean"
        {
            true
        } else {
            false
        }
    }
}
