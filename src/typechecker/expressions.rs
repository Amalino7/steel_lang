use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Expr, Literal};
use crate::token::{Token, TokenType};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::error::TypeCheckerError::AssignmentToCapturedVariable;
use crate::typechecker::type_ast::{
    BinaryOp, ExprKind, LogicalOp, StructType, Type, TypedExpr, UnaryOp,
};
use crate::typechecker::TypeChecker;
use std::cmp::min;
use std::collections::HashSet;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn infer_expression(
        &mut self,
        expr: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        match expr {
            Expr::Unary {
                operator,
                expression,
            } => {
                let typed_operand = self.infer_expression(expression)?;
                let operand_type = typed_operand.ty.clone();

                if operator.token_type == TokenType::Bang {
                    if operand_type == Type::Boolean {
                        Ok(TypedExpr {
                            ty: operand_type,
                            kind: ExprKind::Unary {
                                operator: UnaryOp::Not,
                                operand: Box::new(typed_operand),
                            },
                            line: operator.line,
                        })
                    } else {
                        Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Boolean,
                            found: operand_type,
                            line: operator.line,
                            message: "Expected boolean but found something else.",
                        })
                    }
                } else if operator.token_type == TokenType::Minus {
                    if operand_type == Type::Number {
                        Ok(TypedExpr {
                            ty: operand_type,
                            kind: ExprKind::Unary {
                                operator: UnaryOp::Negate,
                                operand: Box::new(typed_operand),
                            },
                            line: operator.line,
                        })
                    } else {
                        Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Number,
                            found: operand_type,
                            line: operator.line,
                            message: "Expected number but found something else.",
                        })
                    }
                } else {
                    unreachable!("Ast should be checked for invalid operators before this point.")
                }
            }
            Expr::Binary {
                operator,
                left,
                right,
            } => self.check_binary_expression(operator, left, right),

            Expr::Variable { name } => {
                let var = self.scopes.lookup(name.lexeme);
                if let Some((ctx, resolved)) = var {
                    Ok(TypedExpr {
                        ty: ctx.type_info.clone(),
                        kind: ExprKind::GetVar(resolved),
                        line: name.line,
                    })
                } else {
                    Err(TypeCheckerError::UndefinedVariable {
                        name: name.lexeme.to_string(),
                        line: name.line,
                    })
                }
            }
            Expr::Grouping { expression } => self.infer_expression(expression),
            Expr::Literal { literal, line } => {
                let ty = match literal {
                    Literal::Number(_) => Type::Number,
                    Literal::String(_) => Type::String,
                    Literal::Boolean(_) => Type::Boolean,
                    Literal::Void => Type::Void,
                    Literal::Nil => Type::Nil,
                };

                Ok(TypedExpr {
                    ty,
                    kind: ExprKind::Literal(literal.clone()),
                    line: *line,
                })
            }
            Expr::Assignment { identifier, value } => {
                let typed_value = self.infer_expression(value)?;
                let var_lookup = self.scopes.lookup(identifier.lexeme);

                if let Some((ctx, resolved)) = var_lookup {
                    if let ResolvedVar::Closure(_) = &resolved {
                        return Err(AssignmentToCapturedVariable {
                            name: ctx.name.to_string(),
                            line: identifier.line,
                        });
                    }

                    let coerced_value =
                        self.sys
                            .verify_assignment(&ctx.type_info, typed_value, identifier.line)?;

                    Ok(TypedExpr {
                        ty: coerced_value.ty.clone(),
                        kind: ExprKind::Assign {
                            target: resolved,
                            value: Box::new(coerced_value),
                        },
                        line: identifier.line,
                    })
                } else {
                    Err(TypeCheckerError::UndefinedVariable {
                        name: identifier.lexeme.to_string(),
                        line: identifier.line,
                    })
                }
            }
            Expr::Logical {
                operator,
                left,
                right,
            } if operator.token_type == TokenType::QuestionQuestion => {
                let left_typed = self.infer_expression(left)?;
                let right_typed = self.infer_expression(right)?;
                let left_inner = match &left_typed.ty {
                    Type::Optional(inner) => inner.as_ref(),
                    _ => {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Optional(Box::new(Type::Any)),
                            found: left_typed.ty,
                            line: operator.line,
                            message: "Cannot coalesce non-optional type.",
                        });
                    }
                };
                if &right_typed.ty == left_inner {
                    Ok(TypedExpr {
                        ty: left_inner.clone(),
                        kind: ExprKind::Logical {
                            left: Box::new(left_typed),
                            operator: LogicalOp::Coalesce,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: left_inner.clone(),
                        found: right_typed.ty,
                        line: operator.line,
                        message: "Cannot coalesce different types.",
                    })
                }
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left_typed = self.infer_expression(left)?;
                let right_typed = self.infer_expression(right)?;

                let left_type = left_typed.ty.clone();
                let right_type = right_typed.ty.clone();

                if left_type != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: left_type.clone(),
                        line: operator.line,
                        message: "Expected boolean but found something else.",
                    });
                }

                if right_type != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: right_type.clone(),
                        line: operator.line,
                        message: "Expected boolean but found something else.",
                    });
                }

                let op = match operator.token_type {
                    TokenType::And => LogicalOp::And,
                    TokenType::Or => LogicalOp::Or,
                    _ => unreachable!("Invalid logical operator"),
                };

                Ok(TypedExpr {
                    ty: Type::Boolean,
                    kind: ExprKind::Logical {
                        left: Box::new(left_typed),
                        operator: op,
                        right: Box::new(right_typed),
                    },
                    line: operator.line,
                })
            }
            Expr::Call {
                callee,
                arguments,
                safe,
            } => {
                let callee_typed = self.infer_expression(callee)?;

                let safe = if let ExprKind::MethodGet { safe, .. } = callee_typed.kind {
                    safe
                } else if let ExprKind::InterfaceMethodGet { safe, .. } = callee_typed.kind {
                    safe
                } else {
                    *safe
                };

                let lookup_type = if safe {
                    match &callee_typed.ty {
                        Type::Optional(inner) => inner.as_ref().clone(),
                        _ => {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: Type::Optional(Box::new(Type::Any)),
                                found: callee_typed.ty.clone(),
                                line: callee_typed.line,
                                message: "Cannot use safe of non-optional type. Use simply '()'",
                            });
                        }
                    }
                } else {
                    callee_typed.ty.clone()
                };

                if let Type::Function(func) = lookup_type {
                    // Check arity
                    if (func.param_types.len() != arguments.len() && !func.is_vararg)
                        || (func.is_vararg && arguments.len() < func.param_types.len())
                    {
                        return Err(TypeCheckerError::IncorrectArity {
                            callee_name: callee.to_string(),
                            expected: func.param_types.len(),
                            found: arguments.len(),
                            line: callee_typed.line,
                        });
                    }

                    let mut typed_args = Vec::with_capacity(arguments.len());

                    for (mut i, arg) in arguments.iter().enumerate() {
                        let arg_typed = self.infer_expression(arg)?;
                        i = min(i, func.param_types.len() - 1);

                        let expected_param_type = &func.param_types[i];
                        let found = arg_typed.ty.clone();

                        if *expected_param_type != Type::Any {
                            let coerced_arg = self
                                .sys
                                .verify_assignment(
                                    expected_param_type,
                                    arg_typed,
                                    callee_typed.line,
                                )
                                .map_err(|_| TypeCheckerError::FunctionParameterTypeMismatch {
                                    expected: expected_param_type.clone(),
                                    found,
                                    param_index: i,
                                    line: callee_typed.line,
                                })?;
                            typed_args.push(coerced_arg);
                        } else {
                            typed_args.push(arg_typed);
                        }
                    }

                    Ok(TypedExpr {
                        ty: func.return_type.clone().wrap_in_optional(),
                        line: callee_typed.line,
                        kind: ExprKind::Call {
                            callee: Box::new(callee_typed),
                            arguments: typed_args,
                            safe,
                        },
                    })
                } else {
                    Err(TypeCheckerError::CalleeIsNotAFunction {
                        found: callee_typed.ty,
                        line: callee_typed.line,
                    })
                }
            }
            Expr::StructInitializer { name, fields } => {
                let struct_type = self
                    .sys
                    .get_struct(name.lexeme)
                    .ok_or(TypeCheckerError::UndefinedType {
                        name: name.lexeme.to_string(),
                        line: name.line,
                        message: "Expected a struct type here.",
                    })?
                    .clone();

                let mut args = vec![];
                let mut defined_fields = HashSet::new();

                for (field_tok, expr) in fields {
                    let arg_expr = self.infer_expression(expr)?;

                    let (idx, expected_type) = match struct_type.fields.get(field_tok.lexeme) {
                        Some(f) => f.clone(),
                        None => {
                            return Err(TypeCheckerError::UndefinedField {
                                struct_name: struct_type.name.to_string(),
                                field_name: field_tok.lexeme.to_string(),
                                line: field_tok.line,
                            });
                        }
                    };

                    let coerced_arg =
                        self.sys
                            .verify_assignment(&expected_type, arg_expr, field_tok.line)?;

                    args.push((idx, coerced_arg));
                    defined_fields.insert(field_tok.lexeme);
                }

                // Sorting and Missing Field checks...
                args.sort_by(|a, b| a.0.cmp(&b.0));
                for field in struct_type.fields.keys() {
                    if !defined_fields.contains(field.as_str()) {
                        return Err(TypeCheckerError::MissingField {
                            struct_name: struct_type.name.to_string(),
                            field_name: field.to_string(),
                            line: name.line,
                        });
                    }
                }

                Ok(TypedExpr {
                    ty: Type::Struct(struct_type.name),
                    kind: ExprKind::StructInit {
                        name: Box::from(name.lexeme),
                        args: args.into_iter().map(|(_, arg)| arg).collect(),
                    },
                    line: name.line,
                })
            }
            Expr::Get {
                object,
                field,
                safe,
            } => {
                // Static method check
                if let Expr::Variable { name } = object.as_ref() {
                    if self.sys.does_type_exist(name.lexeme) {
                        return self.handle_static_method_access(name, field);
                    }
                }

                let object_typed = self.infer_expression(object)?;

                // Optional handling
                let lookup_type = if *safe {
                    match &object_typed.ty {
                        Type::Optional(inner) => inner.as_ref().clone(),
                        _ => {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: Type::Optional(Box::new(Type::Any)),
                                found: object_typed.ty.clone(),
                                line: field.line,
                                message: "Cannot access safe property of non-optional type. Use simply '.'",
                            });
                        }
                    }
                } else {
                    object_typed.ty.clone()
                };

                // 3. Perform Lookup & Create Inner Expression
                let mut result_expr = match &lookup_type {
                    Type::Struct(struct_name) => {
                        let struct_def = self
                            .sys
                            .get_struct(struct_name)
                            .expect("Should have errored earlier");

                        match struct_def.fields.get(field.lexeme) {
                            Some((idx, field_type)) => Ok(TypedExpr {
                                ty: field_type.clone(),
                                kind: ExprKind::GetField {
                                    object: Box::new(object_typed),
                                    index: *idx as u8,
                                    safe: *safe,
                                },
                                line: field.line,
                            }),
                            None => self.handle_instance_method(
                                field,
                                &lookup_type,
                                object_typed,
                                *safe,
                            ),
                        }
                    }

                    Type::Interface(iface_name) => {
                        let iface = self.sys.get_interface(iface_name).ok_or(
                            TypeCheckerError::UndefinedType {
                                name: iface_name.to_string(),
                                line: field.line,
                                message: "Expected an interface type here.",
                            },
                        )?;

                        let (idx, method_ty) = iface.methods.get(field.lexeme).ok_or(
                            TypeCheckerError::UndefinedMethod {
                                line: field.line,
                                found: Type::Interface(iface.name.clone()),
                                method_name: field.lexeme.to_string(),
                            },
                        )?;

                        let ty = match method_ty {
                            Type::Function(func) => {
                                if func.param_types.is_empty() {
                                    return Err(TypeCheckerError::UndefinedMethod {
                                        line: field.line,
                                        found: Type::Interface(iface.name.clone()),
                                        method_name: field.lexeme.to_string(),
                                    });
                                }
                                let params = func.param_types.iter().skip(1).cloned().collect();
                                Type::new_function(params, func.return_type.clone())
                            }
                            other => other.clone(),
                        };

                        Ok(TypedExpr {
                            ty,
                            kind: ExprKind::InterfaceMethodGet {
                                object: Box::new(object_typed),
                                method_index: *idx as u8,
                                safe: *safe,
                            },
                            line: field.line,
                        })
                    }
                    // Fallback for primitives
                    _ => self.handle_instance_method(field, &lookup_type, object_typed, *safe),
                }?;

                // 4. Wrap in Optional if necessary
                if *safe {
                    result_expr.ty = result_expr.ty.wrap_in_optional();
                }

                Ok(result_expr)
            }
            Expr::Set {
                object,
                field,
                value,
                safe,
            } => {
                let object = self.infer_expression(object)?;
                let value = self.infer_expression(value)?;

                let type_ = if *safe {
                    match &object.ty {
                        Type::Optional(inner) => inner.as_ref().clone(),
                        _ => {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: Type::Optional(Box::new(Type::Any)),
                                found: object.ty.clone(),
                                line: field.line,
                                message: "Cannot access safe property of non-optional type. Use simply '.'",
                            });
                        }
                    }
                } else {
                    object.ty.clone()
                };

                if let Type::Struct(struct_def) = type_ {
                    let struct_def = self
                        .sys
                        .get_struct(&struct_def)
                        .expect("Should have errored earlier");

                    let (field_idx, field_type) =
                        self.check_field_type(&struct_def, field, &value)?;

                    Ok(TypedExpr {
                        ty: field_type,
                        kind: ExprKind::SetField {
                            safe: *safe,
                            object: Box::new(object),
                            index: field_idx as u8,
                            value: Box::new(value),
                        },
                        line: field.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeHasNoFields {
                        found: type_,
                        line: object.line,
                    })
                }
            }

            Expr::ForceUnwrap { expression, line } => {
                let expr_typed = self.infer_expression(expression)?;
                match expr_typed.ty.clone() {
                    Type::Optional(inner) => Ok(TypedExpr {
                        ty: *inner,
                        kind: ExprKind::Unary {
                            operator: UnaryOp::Unwrap,
                            operand: Box::new(expr_typed),
                        },
                        line: *line,
                    }),
                    _ => Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Optional(Box::new(Type::Any)),
                        found: expr_typed.ty,
                        line: *line,
                        message: "Cannot force unwrap non-optional type.",
                    }),
                }
            }
        }
    }

    fn check_field_type(
        &self,
        struct_def: &StructType,
        field: &Token,
        field_value: &TypedExpr,
    ) -> Result<(usize, Type), TypeCheckerError> {
        let field_type = struct_def.fields.get(field.lexeme);
        match field_type {
            None => Err(TypeCheckerError::UndefinedField {
                struct_name: struct_def.name.to_string(),
                field_name: field.lexeme.to_string(),
                line: field.line,
            }),
            Some((id, field_type)) => {
                self.sys
                    .verify_assignment(field_type, field_value.clone(), field.line)?;
                Ok((*id, field_type.clone()))
            }
        }
    }

    fn check_binary_expression(
        &mut self,
        operator: &Token,
        left: &Expr<'src>,
        right: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let left_typed = self.infer_expression(left)?;
        let right_typed = self.infer_expression(right)?;

        let left_type = left_typed.ty.clone();
        let right_type = right_typed.ty.clone();

        match operator.token_type {
            TokenType::Minus | TokenType::Slash | TokenType::Star => {
                if left_type == Type::Number && right_type == Type::Number {
                    let op = match operator.token_type {
                        TokenType::Minus => BinaryOp::Subtract,
                        TokenType::Slash => BinaryOp::Divide,
                        TokenType::Star => BinaryOp::Multiply,
                        _ => unreachable!(),
                    };

                    Ok(TypedExpr {
                        ty: Type::Number,
                        kind: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: op,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Number,
                        found: if left_type != Type::Number {
                            left_type
                        } else {
                            right_type
                        },
                        line: operator.line,
                        message: "Expected number but found something else.",
                    })
                }
            }
            TokenType::Plus => {
                if left_type == Type::Number && right_type == Type::Number {
                    Ok(TypedExpr {
                        ty: Type::Number,
                        kind: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: BinaryOp::Add,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    })
                } else if left_type == Type::String && right_type == Type::String {
                    Ok(TypedExpr {
                        ty: Type::String,
                        kind: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: BinaryOp::Concat,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: left_type,
                        found: right_type,
                        line: operator.line,
                        message: "Operands to '+' must be both numbers or both strings.",
                    })
                }
            }
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => {
                if (left_type == Type::Number && right_type == Type::Number)
                    || (left_type == Type::String && right_type == Type::String)
                {
                    let op = match (operator.token_type.clone(), left_type) {
                        (TokenType::Greater, Type::String) => BinaryOp::GreaterString,
                        (TokenType::GreaterEqual, Type::String) => BinaryOp::GreaterEqualString,
                        (TokenType::Less, Type::String) => BinaryOp::LessString,
                        (TokenType::LessEqual, Type::String) => BinaryOp::LessEqualString,
                        (TokenType::GreaterEqual, Type::Number) => BinaryOp::GreaterEqualNumber,
                        (TokenType::Greater, Type::Number) => BinaryOp::GreaterNumber,
                        (TokenType::LessEqual, Type::Number) => BinaryOp::LessEqualNumber,
                        (TokenType::Less, Type::Number) => BinaryOp::LessNumber,
                        _ => unreachable!(),
                    };

                    Ok(TypedExpr {
                        ty: Type::Boolean,
                        kind: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: op,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Number,
                        found: left_type,
                        line: operator.line,
                        message: "Expected number but found something else.",
                    })
                }
            }
            TokenType::EqualEqual | TokenType::BangEqual => {
                if self.sys.can_compare(&left_type, &right_type) {
                    let op = match left_type {
                        Type::Number => BinaryOp::EqualEqualNumber,
                        Type::String => BinaryOp::EqualEqualString,
                        _ => BinaryOp::EqualEqual,
                    };

                    let binary_expr = TypedExpr {
                        ty: Type::Boolean,
                        kind: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: op,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    };

                    if operator.token_type == TokenType::BangEqual {
                        Ok(TypedExpr {
                            ty: Type::Boolean,
                            kind: ExprKind::Unary {
                                operator: UnaryOp::Not,
                                operand: Box::new(binary_expr),
                            },
                            line: operator.line,
                        })
                    } else {
                        Ok(binary_expr)
                    }
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: left_type,
                        found: right_type,
                        line: operator.line,
                        message: "Expected the same type but found something else.",
                    })
                }
            }
            _ => {
                unreachable!("Ast should be checked for invalid operators before this point.")
            }
        }
    }

    fn handle_static_method_access(
        &mut self,
        type_name: &Token,
        method_name: &Token,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mangled_name = format!("{}.{}", type_name.lexeme, method_name.lexeme);

        let method =
            self.scopes
                .lookup(mangled_name.as_str())
                .ok_or(TypeCheckerError::UndefinedMethod {
                    line: method_name.line,
                    found: Type::Struct(Rc::from(String::from(type_name.lexeme))),
                    method_name: method_name.lexeme.to_string(),
                })?;

        let (ctx, resolved_var) = method;

        Ok(TypedExpr {
            ty: ctx.type_info.clone(),
            kind: ExprKind::GetVar(resolved_var),
            line: method_name.line,
        })
    }

    fn handle_instance_method(
        &mut self,
        field: &Token,
        lookup_type: &Type,
        object_expr: TypedExpr,
        safe: bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let type_name = lookup_type
            .get_name()
            .ok_or(TypeCheckerError::TypeHasNoFields {
                found: lookup_type.clone(),
                line: object_expr.line,
            })?;

        let mangled_name = format!("{}.{}", type_name, field.lexeme);

        let method =
            self.scopes
                .lookup(mangled_name.as_str())
                .ok_or(TypeCheckerError::UndefinedMethod {
                    line: field.line,
                    found: lookup_type.clone(),
                    method_name: field.lexeme.to_string(),
                })?;

        let ty = match &method.0.type_info {
            Type::Function(func) => {
                let return_type = func.return_type.clone();

                if func.param_types.is_empty() || &func.param_types[0] != lookup_type {
                    return Err(TypeCheckerError::StaticMethodOnInstance {
                        method_name: field.lexeme.to_string(),
                        line: field.line,
                    });
                }

                if func.is_static {
                    return Err(TypeCheckerError::StaticMethodOnInstance {
                        method_name: field.lexeme.to_string(),
                        line: field.line,
                    });
                }

                let params = func.param_types.iter().skip(1).cloned().collect();
                Type::new_function(params, return_type)
            }
            ty => ty.clone(),
        };

        Ok(TypedExpr {
            ty,
            kind: ExprKind::MethodGet {
                object: Box::new(object_expr),
                method: method.1,
                safe,
            },
            line: field.line,
        })
    }
}
