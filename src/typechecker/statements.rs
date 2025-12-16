use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Stmt;
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{StmtKind, Type, TypedStmt};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::{FunctionContext, Symbol, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_stmt(&mut self, stmt: &Stmt<'src>) -> Result<TypedStmt, TypeCheckerError> {
        match stmt {
            Stmt::Expression(expr) => Ok(TypedStmt {
                kind: StmtKind::Expression(self.infer_expression(expr)?),
                line: stmt.get_line(),
                type_info: Type::Void,
            }),
            Stmt::Let {
                identifier,
                value,
                type_info,
            } => {
                let value_node = self.infer_expression(value)?;
                let declared_type = Type::from_ast(type_info, &self.sys)?;

                if declared_type == Type::Unknown {
                    self.scopes
                        .declare(identifier.lexeme.into(), value_node.ty.clone())?;
                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            target: self.scopes.lookup(identifier.lexeme).unwrap().1,
                            value: value_node,
                        },
                        type_info: Type::Void,
                        line: identifier.line,
                    })
                } else {
                    let coerced_value =
                        self.sys
                            .verify_assignment(&declared_type, value_node, identifier.line)?;

                    self.scopes
                        .declare(identifier.lexeme.into(), declared_type)?;

                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            target: self.scopes.lookup(identifier.lexeme).unwrap().1,
                            value: coerced_value,
                        },
                        type_info: Type::Void,
                        line: identifier.line,
                    })
                }
            }
            Stmt::Impl {
                interfaces,
                name,
                methods,
            } => {
                let mut typed_methods = vec![];

                // define methods
                for method in methods {
                    match method {
                        Stmt::Function {
                            name: func_name,
                            params,
                            body,
                            type_,
                        } => {
                            let type_info = Type::from_method_ast(type_, name, &self.sys)?;

                            let primary_mangled = format!("{}.{}", name.lexeme, func_name.lexeme);
                            let typed_method = self.check_function(
                                func_name,
                                params,
                                body,
                                type_info,
                                primary_mangled.into(),
                            )?;
                            typed_methods.push(typed_method);
                        }
                        _ => unreachable!(),
                    }
                }
                // generate vtables
                let mut vtables = vec![];
                for interface in interfaces {
                    // check if interface exists
                    let Some(interface_type) = self.sys.get_interface(interface.lexeme) else {
                        continue;
                    };

                    let mut vtable = std::iter::repeat(ResolvedVar::Local(0))
                        .take(interface_type.methods.len())
                        .collect::<Vec<_>>();
                    let mut missing_methods = vec![];

                    for (method_name, (location, method_type)) in interface_type.methods.iter() {
                        let impl_method_name = format!("{}.{}", name.lexeme, method_name);

                        if let Some((resolved_type, method_location)) =
                            self.scopes.lookup(&impl_method_name)
                        {
                            if TypeSystem::implement_method(&resolved_type.type_info, method_type) {
                                vtable[*location] = method_location;
                            } else {
                                missing_methods.push(format!("{} (type mismatch)", method_name));
                            }
                        } else {
                            missing_methods.push(method_name.clone());
                        }
                    }

                    if !missing_methods.is_empty() {
                        return Err(TypeCheckerError::DoesNotImplementInterface {
                            missing_methods,
                            interface: interface.lexeme.to_string(),
                            line: interface.line,
                        });
                    }
                    self.sys
                        .define_impl(name.lexeme, interface_type.name.clone());

                    vtables.push(vtable);
                }

                Ok(TypedStmt {
                    kind: StmtKind::Impl {
                        methods: typed_methods.into(),
                        vtables: vtables.into(),
                    },
                    line: name.line,
                    type_info: Type::Void,
                })
            }
            Stmt::Block { body, brace_token } => {
                self.scopes.begin_scope(ScopeType::Block);
                let stmts = body
                    .iter()
                    .map(|stmt| self.check_stmt(stmt))
                    .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

                let variable_count = self.scopes.scope_size() as u8;
                self.scopes.end_scope();

                Ok(TypedStmt {
                    kind: StmtKind::Block {
                        body: stmts,
                        variable_count,
                    },
                    type_info: Type::Void,
                    line: brace_token.line,
                })
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.infer_expression(condition)?;
                let then_branch = self.check_stmt(then_branch)?;
                let else_branch = if let Some(else_branch) = else_branch {
                    Some(Box::new(self.check_stmt(else_branch)?))
                } else {
                    None
                };
                if cond_type.ty != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_type.ty,
                        line: condition.get_line(),
                        message: "If condition must be a boolean.",
                    });
                }

                Ok(TypedStmt {
                    kind: StmtKind::If {
                        condition: cond_type,
                        then_branch: Box::new(then_branch),
                        else_branch,
                    },
                    type_info: Type::Void,
                    line: condition.get_line(),
                })
            }
            Stmt::While { condition, body } => {
                let cond_type = self.infer_expression(condition)?;
                let body = self.check_stmt(body)?;
                if cond_type.ty != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_type.ty,
                        line: condition.get_line(),
                        message: "While condition must be a boolean.",
                    });
                }

                Ok(TypedStmt {
                    kind: StmtKind::While {
                        condition: cond_type,
                        body: Box::new(body),
                    },
                    type_info: Type::Void,
                    line: condition.get_line(),
                })
            }
            Stmt::Function {
                name,
                params,
                body,
                type_,
            } => {
                // global functions are already declared
                let type_ = Type::from_ast(type_, &self.sys)?;
                if !self.scopes.is_global() {
                    self.scopes.declare(name.lexeme.into(), type_.clone())?;
                }

                self.check_function(name, params, body, type_, name.lexeme.into())
            }
            Stmt::Return(expr) => {
                let return_expr = self.infer_expression(expr)?;
                if let FunctionContext::Function(func_return_type) = self.current_function.clone() {
                    let coerced_return = self.sys.verify_assignment(
                        &func_return_type,
                        return_expr,
                        expr.get_line(),
                    )?;

                    Ok(TypedStmt {
                        kind: StmtKind::Return(coerced_return),
                        line: expr.get_line(),
                        type_info: Type::Void,
                    })
                } else {
                    Err(TypeCheckerError::InvalidReturnOutsideFunction {
                        line: expr.get_line(),
                    })
                }
            }

            Stmt::Struct { name, .. } | Stmt::Interface { name, .. } => {
                // structs already defined
                if !self.scopes.is_global() {
                    Err(TypeCheckerError::StructOutsideOfGlobalScope {
                        name: name.lexeme.to_string(),
                        line: name.line,
                    })
                } else {
                    Ok(TypedStmt {
                        kind: StmtKind::StructDecl {},
                        line: name.line,
                        type_info: Type::Void,
                    })
                }
            }
        }
    }

    fn check_function(
        &mut self,
        name: &Token<'src>,
        params: &Vec<Token<'src>>,
        body: &Vec<Stmt<'src>>,
        type_: Type,
        full_name: Symbol,
    ) -> Result<TypedStmt, TypeCheckerError> {
        let enclosing_function_context = self.current_function.clone();
        if let Type::Function(func) = &type_ {
            self.current_function = FunctionContext::Function(func.return_type.clone());

            let prev_closures = self.scopes.clear_closures();
            self.scopes.begin_scope(ScopeType::Function);

            self.scopes.declare(name.lexeme.into(), type_.clone())?;
            // Declare parameters.
            for (i, param) in params.iter().enumerate() {
                self.scopes
                    .declare(param.lexeme.into(), func.param_types[i].clone())?;
            }

            let func_body = body
                .iter()
                .map(|stmt| self.check_stmt(stmt))
                .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

            self.scopes.end_scope();
            self.current_function = enclosing_function_context;

            let (_, func_location) = self
                .scopes
                .lookup(full_name.as_ref())
                .expect("Variable was just added to the scope.");

            let old_closures = self.scopes.return_closures(prev_closures);

            let mut captures = vec![];
            for clos_var in old_closures {
                let (_, var_ctx) = self
                    .scopes
                    .lookup(clos_var.as_ref())
                    .expect("Variable should exist in upper scope.");
                captures.push(var_ctx);
            }
            Ok(TypedStmt {
                kind: StmtKind::Function {
                    target: func_location,
                    name: Box::from(String::from(name.lexeme)),
                    body: Box::from(TypedStmt {
                        kind: StmtKind::Block {
                            body: func_body,
                            variable_count: 0,
                        },
                        line: name.line,
                        type_info: Type::Void,
                    }),
                    captures: Box::from(captures),
                },
                type_info: type_,
                line: name.line,
            })
        } else {
            unreachable!()
        }
    }
}
