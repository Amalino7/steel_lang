use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Stmt;
use crate::stdlib::NativeDef;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{
    ExprKind, InterfaceType, StmtKind, StructType, Type, TypedExpr, TypedStmt,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

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
pub type Symbol = Rc<str>;

#[derive(Clone)]
struct VariableContext {
    type_info: Type,
    name: Symbol,
    index: usize,
}

impl VariableContext {
    fn new(name: Symbol, type_info: Type, index: usize) -> Self {
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
struct Scope {
    variables: HashMap<Symbol, VariableContext>,
    scope_type: ScopeType,
    last_index: usize,
}

pub struct TypeChecker<'src> {
    current_function: FunctionContext,
    structs: HashMap<Symbol, StructType>,
    interfaces: HashMap<Symbol, InterfaceType>,
    impls: HashSet<(Symbol, Symbol)>,
    variable_scope: Vec<Scope>,
    natives: &'src [NativeDef],
    closures: Vec<Symbol>,
}

impl<'src> TypeChecker<'src> {
    // This is used for testing purposes only.
    #[allow(dead_code)]
    pub fn new() -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashSet::new(),
            variable_scope: vec![],
            natives: &[],
            closures: vec![],
        }
    }

    pub fn new_with_natives(natives: &'src [NativeDef]) -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashSet::new(),
            variable_scope: vec![],
            natives,
            closures: vec![],
        }
    }

    pub fn check(&mut self, ast: &[Stmt<'src>]) -> Result<TypedStmt, Vec<TypeCheckerError>> {
        let mut errors = vec![];

        self.declare_global_structs(ast, &mut errors);
        self.declare_global_interfaces(ast, &mut errors);
        self.declare_global_impls(ast, &mut errors);

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
            self.declare_variable(native.name.into(), native.type_.clone())
                .expect("Failed to register global");
        }
    }

    fn declare_variable(&mut self, name: Symbol, type_info: Type) -> Result<(), TypeCheckerError> {
        if let Some(scope) = self.variable_scope.last_mut() {
            scope.variables.insert(
                name.clone(),
                VariableContext::new(name, type_info, scope.last_index),
            );
            scope.last_index += 1;
        }
        Ok(())
    }

    fn lookup_variable(&mut self, name: &str) -> Option<(VariableContext, ResolvedVar)> {
        let mut is_closure = false;
        for scope in self.variable_scope.iter().rev() {
            if let Some(var) = scope.variables.get(name).cloned() {
                return if scope.scope_type == ScopeType::Global {
                    let index = var.index as u16;
                    Some((var, ResolvedVar::Global(index)))
                } else if is_closure {
                    // Find if the closure is already declared.
                    for (i, closure) in self.closures.iter().enumerate() {
                        if closure.as_ref() == name {
                            return Some((var, ResolvedVar::Closure(i as u8)));
                        }
                    }
                    self.closures.push(name.into());
                    Some((var, ResolvedVar::Closure((self.closures.len() - 1) as u8)))
                } else {
                    let index = var.index as u8;
                    Some((var, ResolvedVar::Local(index)))
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
            || self.interfaces.contains_key(name)
            || name == "string"
            || name == "number"
            || name == "boolean"
            || name == "void"
        {
            true
        } else {
            false
        }
    }

    fn verify_assignment(
        &mut self,
        expected_type: &Type,
        expr: TypedExpr,
        line: u32,
    ) -> Result<TypedExpr, TypeCheckerError> {
        if *expected_type == expr.ty {
            return Ok(expr);
        }

        if let (Type::Interface(iface_name), other) = (expected_type, expr.ty.clone()) {
            if let Some(name) = other.get_name() {
                if self.impls.contains(&(name.into(), iface_name.clone())) {
                    return self.create_upcast(iface_name, name, expr, line);
                }
            }
        }

        Err(TypeCheckerError::TypeMismatch {
            expected: expected_type.clone(),
            found: expr.ty,
            line,
            message: "Type mismatch in assignment or argument passing.",
        })
    }

    fn create_upcast(
        &mut self,
        iface_name: &Symbol,
        struct_name: &str,
        expr: TypedExpr,
        line: u32,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let iface_def = self
            .interfaces
            .get(iface_name)
            .ok_or(TypeCheckerError::UndefinedType {
                name: iface_name.to_string(),
                line,
            })?;

        let methods_info = iface_def
            .methods
            .iter()
            .map(|(name, (idx, _))| (name.clone(), *idx))
            .collect::<Vec<_>>();

        let mut slots: Vec<(usize, ResolvedVar)> = Vec::with_capacity(methods_info.len());

        for (method_name, idx) in &methods_info {
            let mangled = format!("{}:{}.{}", struct_name, iface_name, method_name);

            // First try the mangled name
            let result = self.lookup_variable(&mangled);

            // If not found, try the inherent name
            let (_ctx, resolved) = match result {
                Some(var) => var,
                None => {
                    let inherent = format!("{}.{}", struct_name, method_name);
                    self.lookup_variable(&inherent)
                        .ok_or(TypeCheckerError::UndefinedMethod {
                            line,
                            found: Type::Struct(Symbol::from(struct_name)),
                            method_name: method_name.clone(),
                        })?
                }
            };

            slots.push((*idx, resolved));
        }

        slots.sort_by_key(|(i, _)| *i);
        let vtable = slots.into_iter().map(|(_, r)| r).collect::<Vec<_>>();

        Ok(TypedExpr {
            ty: Type::Interface(iface_name.clone()),
            line: expr.line,
            kind: ExprKind::InterfaceUpcast {
                expr: Box::new(expr),
                interface: iface_name.clone(),
                vtable: vtable.into_boxed_slice(),
            },
        })
    }
}
