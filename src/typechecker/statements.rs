use crate::parser::ast::Stmt;
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{InterfaceType, StmtKind, StructType, Type, TypedStmt};
use crate::typechecker::{FunctionContext, Symbol, TypeChecker};
use std::collections::HashMap;
use std::mem::replace;
use std::ops::Deref;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_interfaces(
        &mut self,
        ast: &[Stmt<'src>],
        errors: &mut Vec<TypeCheckerError>,
    ) {
        // declare interface names
        for stmt in ast.iter() {
            if let Stmt::Interface { name, .. } = stmt {
                let name: Symbol = name.lexeme.into();
                self.interfaces.insert(
                    name.clone(),
                    InterfaceType {
                        name,
                        methods: HashMap::new(),
                    },
                );
            }
        }

        // define interface method signatures + stable method indices
        for stmt in ast.iter() {
            if let Stmt::Interface { name, methods } = stmt {
                let mut method_map: HashMap<String, (usize, Type)> = HashMap::new();
                for (i, sig) in methods.iter().enumerate() {
                    let ty = match Type::from_method_ast(
                        &sig.type_,
                        name,
                        &self.structs,
                        &self.interfaces,
                    ) {
                        Ok(t) => t,
                        Err(e) => {
                            errors.push(e);
                            Type::Unknown
                        }
                    };
                    method_map.insert(sig.name.lexeme.to_string(), (i, ty));
                }
                let iface = self
                    .interfaces
                    .get_mut(name.lexeme)
                    .expect("just declared interface");
                iface.methods = method_map;
            }
        }
    }

    pub(crate) fn declare_global_impls(
        &mut self,
        ast: &[Stmt<'src>],
        errors: &mut Vec<TypeCheckerError>,
    ) {
        for stmt in ast.iter() {
            if let Stmt::Impl {
                interfaces,
                name: type_name,
                methods,
            } = stmt
            {
                let name: Symbol = type_name.lexeme.into();
                if !self.does_type_exist(type_name.lexeme) {
                    errors.push(TypeCheckerError::UndefinedType {
                        name: type_name.lexeme.to_string(),
                        line: type_name.line,
                    });
                    continue;
                }

                for iface_tok in interfaces.iter() {
                    let Some(iface_def) = self.interfaces.get(iface_tok.lexeme) else {
                        errors.push(TypeCheckerError::UndefinedType {
                            name: iface_tok.lexeme.to_string(),
                            line: iface_tok.line,
                        });
                        continue;
                    };
                    self.impls.insert((name.clone(), iface_def.name.clone()));
                }

                for iface_tok in interfaces.iter() {
                    let Some(iface_def) = self.interfaces.get(iface_tok.lexeme) else {
                        continue;
                    };

                    let mut provided: HashMap<&str, &Stmt<'src>> = HashMap::new();
                    for m in methods.iter() {
                        if let Stmt::Function { name, .. } = m {
                            provided.insert(name.lexeme, m);
                        }
                    }

                    for (method_name, (_idx, required_ty)) in iface_def.methods.iter() {
                        let Some(provided_stmt) = provided.get(method_name.as_str()) else {
                            errors.push(TypeCheckerError::UndefinedMethod {
                                line: type_name.line,
                                found: Type::Struct(name.clone()),
                                method_name: method_name.clone(),
                            });
                            continue;
                        };

                        let Stmt::Function {
                            type_,
                            name: fn_name,
                            ..
                        } = provided_stmt
                        else {
                            continue;
                        };

                        let impl_ty = match Type::from_method_ast(
                            type_,
                            type_name,
                            &self.structs,
                            &self.interfaces,
                        ) {
                            Ok(t) => t,
                            Err(e) => {
                                errors.push(e);
                                Type::Unknown
                            }
                        };

                        let mut req_ty;
                        // Required signature is written using Self in the interface, so translate it too:
                        match required_ty {
                            Type::Function(func) => {
                                req_ty = func.deref().clone();
                                if func.param_types.len() > 0
                                    && func.param_types[0].get_name()
                                        == Some(iface_def.name.as_ref())
                                {
                                    req_ty.param_types[0] = Type::from_identifier(
                                        type_name,
                                        &self.structs,
                                        &self.interfaces,
                                    )
                                    .unwrap();
                                }
                            }
                            _ => unreachable!(),
                        }

                        if impl_ty != *required_ty && impl_ty != Type::Function(Rc::from(req_ty)) {
                            errors.push(TypeCheckerError::TypeMismatch {
                                expected: required_ty.clone(),
                                found: impl_ty,
                                line: fn_name.line,
                                message: "Impl method signature does not match interface signature.",
                            });
                        }
                    }
                }
            }
        }
    }

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
                let func_type = Type::from_ast(type_info, &self.structs, &self.interfaces);
                if let Err(e) = func_type {
                    errors.push(e);
                    continue;
                }

                let res = self.declare_variable(name.lexeme.into(), func_type.unwrap());

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
                    name.lexeme.into(),
                    StructType {
                        name: name.lexeme.into(),
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
                    let field_type = Type::from_ast(&type_ast, &self.structs, &self.interfaces);
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
                let declared_type = Type::from_ast(type_info, &self.structs, &self.interfaces)?;

                if declared_type == Type::Unknown {
                    self.declare_variable(identifier.lexeme.into(), value_node.ty.clone())?;
                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            target: self.lookup_variable(identifier.lexeme).unwrap().1,
                            value: value_node,
                        },
                        type_info: Type::Void,
                        line: identifier.line,
                    })
                } else {
                    let coerced_value =
                        self.verify_assignment(&declared_type, value_node, identifier.line)?;

                    self.declare_variable(identifier.lexeme.into(), declared_type)?;

                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            target: self.lookup_variable(identifier.lexeme).unwrap().1,
                            value: coerced_value,
                        },
                        type_info: Type::Void,
                        line: identifier.line,
                    })
                }
            }
            Stmt::Interface { name, .. } => {
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

            Stmt::Impl {
                interfaces,
                name,
                methods,
            } => {
                let mut typed_methods = vec![];

                for method in methods {
                    match method {
                        Stmt::Function {
                            name: func_name,
                            params,
                            body,
                            type_,
                        } => {
                            let type_info = Type::from_method_ast(
                                type_,
                                name,
                                &self.structs,
                                &self.interfaces,
                            )?;

                            if !interfaces.is_empty() {
                                let primary_mangled =
                                    format!("{}.{ }", name.lexeme, func_name.lexeme)
                                        .replace("{ }", "");
                                let typed_method = self.check_function(
                                    func_name,
                                    params,
                                    body,
                                    type_info,
                                    primary_mangled.into(),
                                )?;
                                typed_methods.push(typed_method);
                            } else {
                                let mangled_name: Symbol =
                                    format!("{}.{}", name.lexeme, func_name.lexeme).into();
                                self.declare_variable(mangled_name.clone(), type_info.clone())?;
                                let typed_method = self.check_function(
                                    func_name,
                                    params,
                                    body,
                                    type_info,
                                    mangled_name,
                                )?;
                                typed_methods.push(typed_method);
                            }
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
                let type_ = Type::from_ast(type_, &self.structs, &self.interfaces)?;
                if self.variable_scope.len() != 1 {
                    self.declare_variable(name.lexeme.into(), type_.clone())?;
                }

                self.check_function(name, params, body, type_, name.lexeme.into())
            }
            Stmt::Return(expr) => {
                let return_expr = self.infer_expression(expr)?;
                if let FunctionContext::Function(func_return_type) = self.current_function.clone() {
                    let coerced_return =
                        self.verify_assignment(&func_return_type, return_expr, expr.get_line())?;

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

            let prev_closures = replace(&mut self.closures, vec![]);
            self.begin_function_scope();

            self.declare_variable(name.lexeme.into(), type_.clone())?;
            // Declare parameters.
            for (i, param) in params.iter().enumerate() {
                self.declare_variable(param.lexeme.into(), func.param_types[i].clone())?;
            }

            let func_body = body
                .iter()
                .map(|stmt| self.check_stmt(stmt))
                .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

            self.end_scope();
            self.current_function = enclosing_function_context;

            let (_, func_location) = self
                .lookup_variable(full_name.as_ref())
                .expect("Variable was just added to the scope.");

            let old_closures = replace(&mut self.closures, prev_closures);
            let mut captures = vec![];
            for clos_var in old_closures {
                let (_, var_ctx) = self
                    .lookup_variable(clos_var.as_ref())
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
