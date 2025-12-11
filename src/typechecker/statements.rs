use crate::parser::ast::Stmt;
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{StmtKind, StructType, Type, TypedStmt};
use crate::typechecker::{FunctionContext, TypeChecker};
use std::borrow::Cow;
use std::collections::HashMap;
use std::mem::replace;
use std::rc::Rc;

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
                let func_type = Type::from_ast(type_info, &self.structs);
                if let Err(e) = func_type {
                    errors.push(e);
                    continue;
                }

                let res = self.declare_variable(name.lexeme, func_type.unwrap());

                if let Err(e) = res {
                    errors.push(e);
                }
            }
        }
    }

    pub(crate) fn declare_global_structs(
        &mut self,
        ast: &[Stmt<'src>],
        errors: &mut Vec<TypeCheckerError>,
    ) {
        // declare structs by name
        for stmt in ast.iter() {
            if let Stmt::Struct { name, .. } = stmt {
                self.structs.insert(
                    name.lexeme,
                    StructType {
                        name: Rc::new(name.lexeme.to_string()),
                        fields: HashMap::new(),
                    },
                );
            }
        }

        // define fields for structs
        for stmt in ast {
            if let Stmt::Struct { name, fields } = stmt {
                let mut field_types = HashMap::new();
                for (i, (name, type_ast)) in fields.into_iter().enumerate() {
                    let field_type = Type::from_ast(&type_ast, &self.structs);
                    match field_type {
                        Ok(field_type) => {
                            field_types.insert(name.lexeme.to_string(), (i, field_type));
                        }
                        Err(err) => {
                            // Using Unknown to minimise the number of errors.
                            field_types.insert(name.lexeme.to_string(), (i, Type::Unknown));
                            errors.push(err);
                        }
                    }
                }
                let struct_def =
                    self.structs
                        .get_mut(name.lexeme)
                        .ok_or(TypeCheckerError::UndefinedType {
                            name: name.lexeme.to_string(),
                            line: name.line,
                        });
                match struct_def {
                    Ok(struct_def) => {
                        struct_def.fields = field_types;
                    }
                    Err(err) => errors.push(err),
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

                let type_info = Type::from_ast(type_info, &self.structs)?;

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
                            .lookup_variable(Cow::from(identifier.lexeme))
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
                let type_ = Type::from_ast(type_, &self.structs)?;
                if self.variable_scope.len() != 1 {
                    self.declare_variable(name.lexeme, type_.clone())?;
                }

                self.check_function(name, params, body, type_, Cow::Borrowed(name.lexeme))
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

            Stmt::Struct { name, .. } => {
                // structs already defined
                if self.variable_scope.len() != 1 {
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
            Stmt::Impl { name, methods } => {
                let mut typed_methods = vec![];
                for method in methods {
                    match method {
                        Stmt::Function {
                            name: func_name,
                            params,
                            body,
                            type_,
                        } => {
                            let type_info = Type::from_method_ast(type_, name, &self.structs)?;

                            let mangled_name = format!("{}.{}", name.lexeme, func_name.lexeme);
                            self.declare_mangled(
                                mangled_name.to_string(),
                                func_name.lexeme,
                                type_info.clone(),
                            )?;
                            let typed_method = self.check_function(
                                func_name,
                                params,
                                body,
                                type_info,
                                Cow::Owned(mangled_name),
                            )?;
                            typed_methods.push(typed_method);
                        }
                        _ => unreachable!(),
                    }
                }
                Ok(TypedStmt {
                    kind: StmtKind::Impl {
                        methods: typed_methods,
                    },
                    line: name.line,
                    type_info: Type::Void,
                })
            }
        }
    }

    fn check_function(
        &mut self,
        name: &Token<'src>,
        params: &Vec<Token<'src>>,
        body: &Vec<Stmt<'src>>,
        type_: Type,
        full_name: Cow<'src, str>,
    ) -> Result<TypedStmt, TypeCheckerError> {
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
                .lookup_variable(full_name)
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
}
