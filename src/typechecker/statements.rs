use crate::parser::ast::Stmt;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{StmtKind, Type, TypedStmt};
use crate::typechecker::{FunctionContext, TypeChecker};
use std::mem::replace;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_functions(
        &mut self,
        ast: &[Stmt<'src>],
        errors: &mut Vec<TypeCheckerError>,
    ) {
        for stmt in ast.iter() {
            if let Stmt::Function {
                name,
                params: _,
                type_: type_info,
                body: _,
            } = stmt
            {
                let res = self.declare_variable(
                    name.lexeme,
                    Type::from_ast(type_info).expect("Structs not yet supported."),
                );

                if let Err(e) = res {
                    errors.push(e);
                }
            }
        }
    }

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

                let type_info =
                    Type::from_ast(type_info).ok_or(TypeCheckerError::UndefinedType {
                        name: type_info.to_string(),
                        line: identifier.line,
                    })?;

                if type_info == Type::Unknown {
                    self.declare_variable(identifier.lexeme, value_node.ty.clone())?;
                } else if type_info == value_node.ty {
                    self.declare_variable(identifier.lexeme, type_info)?;
                } else {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: type_info.clone(),
                        found: value_node.ty,
                        line: identifier.line,
                        message: "Expected the same type but found something else.",
                    });
                }

                Ok(TypedStmt {
                    kind: StmtKind::Let {
                        target: self
                            .lookup_variable(identifier.lexeme)
                            .expect("variable just declared")
                            .1,
                        value: value_node,
                    },
                    type_info: Type::Void,
                    line: identifier.line,
                })
            }
            Stmt::Block { body, brace_token } => {
                self.begin_scope();
                let stmts = body
                    .iter()
                    .map(|stmt| self.check_stmt(stmt))
                    .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

                let variable_count = self.variable_scope.last().unwrap().variables.len() as u8;
                self.end_scope();

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
                let type_ = Type::from_ast(type_).expect("Structs not yet supported.");
                if self.variable_scope.len() != 1 {
                    self.declare_variable(name.lexeme, type_.clone())?;
                }

                let enclosing_function_context = self.current_function.clone();
                if let Type::Function(func) = &type_ {
                    self.current_function = FunctionContext::Function(func.return_type.clone());

                    let prev_closures = replace(&mut self.closures, vec![]);
                    self.begin_function_scope();

                    self.declare_variable(name.lexeme, type_.clone())?;
                    // Declare parameters.
                    for (i, param) in params.iter().enumerate() {
                        self.declare_variable(param.lexeme, func.param_types[i].clone())?;
                    }

                    let func_body = body
                        .iter()
                        .map(|stmt| self.check_stmt(stmt))
                        .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

                    self.end_scope();
                    self.current_function = enclosing_function_context;

                    let (_, func_location) = self
                        .lookup_variable(name.lexeme)
                        .expect("Variable was just added to the scope.");

                    let old_closures = replace(&mut self.closures, prev_closures);
                    let mut captures = vec![];
                    for clos_var in old_closures {
                        let (_, var_ctx) = self
                            .lookup_variable(clos_var)
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
            Stmt::Return(expr) => {
                let return_type = self.infer_expression(expr)?;
                if let FunctionContext::Function(func_return_type) = &self.current_function {
                    if return_type.ty != *func_return_type {
                        Err(TypeCheckerError::TypeMismatch {
                            expected: func_return_type.clone(),
                            found: return_type.ty.clone(),
                            line: expr.get_line(),
                            message: "Mismatched return types. Expected a function that returns a different type than the function that is being called.",
                        })
                    } else {
                        Ok(TypedStmt {
                            kind: StmtKind::Return(return_type),
                            line: expr.get_line(),
                            type_info: Type::Void,
                        })
                    }
                } else {
                    Err(TypeCheckerError::InvalidReturnOutsideFunction {
                        line: expr.get_line(),
                    })
                }
            }
        }
    }
}
