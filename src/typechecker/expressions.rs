use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Expr, Literal};
use crate::token::{Token, TokenType};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::error::TypeCheckerError::AssignmentToCapturedVariable;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{BinaryOp, ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::{StructType, TupleType, Type};
use crate::typechecker::TypeChecker;
use std::collections::HashMap;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn infer_expression(
        &mut self,
        expr: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        match expr {
            Expr::TypeSpecialization { .. } => {
                todo!()
            }
            Expr::Is {
                expression,
                type_name,
            } => {
                let target = self.infer_expression(expression)?;
                let Type::Enum(enum_name, _) = &target.ty else {
                    return Err(TypeCheckerError::InvalidIsUsage {
                        line: type_name.line,
                        message: "Is can only be used on enum types.",
                    });
                };
                let enum_def = self
                    .sys
                    .get_enum(enum_name.as_ref())
                    .expect("Invalid enum Type return!");

                if !enum_def.variants.contains_key(type_name.lexeme) {
                    return Err(TypeCheckerError::InvalidIsUsage {
                        line: type_name.line,
                        message: "Enum variant does not exist.",
                    });
                }
                let variant_idx = enum_def.variants.get(type_name.lexeme).unwrap();

                Ok(TypedExpr {
                    ty: Type::Boolean,
                    kind: ExprKind::Is {
                        target: Box::new(target),
                        variant_idx: *variant_idx as u16,
                    },
                    line: type_name.line,
                })
            }
            Expr::Tuple { elements } => {
                let mut typed_elements = Vec::with_capacity(elements.len());
                let mut type_vec = vec![];
                for element in elements {
                    let el = self.infer_expression(element)?;
                    type_vec.push(el.ty.clone());
                    typed_elements.push(el);
                }

                let ty = Type::Tuple(Rc::new(TupleType { types: type_vec }));
                Ok(TypedExpr {
                    ty,
                    kind: ExprKind::Tuple {
                        elements: typed_elements,
                    },
                    line: elements[0].get_line(),
                })
            }
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
                        kind: ExprKind::GetVar(resolved, ctx.name.clone()),
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

                    let coerced_value = self.sys.verify_assignment(
                        &mut HashMap::new(),
                        &ctx.type_info,
                        typed_value,
                        identifier.line,
                    )?;

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
                            typed_refinements: vec![],
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
                let refinements = self.analyze_condition(&left_typed);

                self.scopes.begin_scope(ScopeType::Block);

                let refinements = if TokenType::And == operator.token_type {
                    refinements.true_path
                } else if TokenType::Or == operator.token_type {
                    refinements.false_path
                } else {
                    unreachable!("Invalid logical operator");
                };
                let mut typed_refinements = vec![];
                for (name, ty) in refinements {
                    if let Some(case) = self.scopes.refine(&name, ty) {
                        typed_refinements.push(case);
                    }
                }
                let right_typed = self.infer_expression(right)?;
                self.scopes.end_scope();

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
                        typed_refinements,
                    },
                    line: operator.line,
                })
            }
            Expr::Call {
                callee,
                arguments,
                safe,
            } => {
                // Handle args
                let mut inferred_args = Vec::with_capacity(arguments.len());

                for arg in arguments {
                    let typed_val = self.infer_expression(&arg.expr)?;

                    let label = arg.label.as_ref().map(|t| t.lexeme);
                    let line = arg
                        .label
                        .as_ref()
                        .map(|t| t.line)
                        .unwrap_or(callee.get_line());

                    inferred_args.push((label, typed_val, line));
                }
                // Handle Struct constructor
                if let Expr::Variable { name } = &**callee
                    && let Some(struct_def) = self.sys.get_struct(&name.lexeme)
                {
                    let owned_name = struct_def.name.clone();
                    let bound_args = self.sys.bind_arguments(
                        &name.lexeme,
                        &struct_def.ordered_fields,
                        inferred_args,
                        false,
                        callee.get_line(),
                    )?;

                    return Ok(TypedExpr {
                        ty: Type::Struct(owned_name.clone(), vec![].into()),
                        kind: ExprKind::StructInit {
                            name: Box::from(owned_name.to_string()),
                            args: bound_args.0,
                        },
                        line: callee.get_line(),
                    });
                }
                // Check for an Enum constructor pattern
                if let Expr::Get { object, field, .. } = callee.as_ref() {
                    if let Expr::Variable { name } = object.as_ref() {
                        if let Some(enum_def) = self.sys.get_enum(name.lexeme) {
                            if let Some((variant_idx, variant_type)) =
                                enum_def.get_variant(field.lexeme)
                            {
                                return self.handle_enum_call(
                                    &variant_type,
                                    variant_idx,
                                    inferred_args,
                                    field,
                                    enum_def,
                                    callee.get_line(),
                                );
                            }
                        }
                    }
                }

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

                // Check for Normal Function Call
                match lookup_type {
                    Type::Function(func) => {
                        let (bound_args, map) = self.sys.bind_arguments(
                            "function",
                            &func.params,
                            inferred_args,
                            func.is_vararg,
                            callee_typed.line,
                        )?;

                        let ret_type = if safe {
                            func.return_type.clone().wrap_in_optional()
                        } else {
                            func.return_type.clone()
                        };
                        let ret_type = TypeSystem::generic_to_concrete(ret_type, &map);

                        Ok(TypedExpr {
                            ty: ret_type,
                            line: callee_typed.line,
                            kind: ExprKind::Call {
                                callee: Box::new(callee_typed),
                                arguments: bound_args,
                                safe,
                            },
                        })
                    }
                    _ => Err(TypeCheckerError::CalleeIsNotCallable {
                        found: callee_typed.ty,
                        line: callee_typed.line,
                    }),
                }
            }
            Expr::Get {
                object,
                field,
                safe,
            } => {
                //TODO Ignores safe static access probably should be a warning
                if let Expr::Variable { name } = object.as_ref()
                    && self.sys.does_type_exist(name.lexeme)
                {
                    return self.resolve_static_access(name, field);
                }

                let object_typed = self.infer_expression(object)?;

                self.resolve_instance_access(object_typed, field, *safe)
            }
            Expr::Set {
                object,
                field,
                value,
                safe,
            } => {
                let object_typed = self.infer_expression(object)?;
                let value = self.infer_expression(value)?;

                let type_ = if *safe {
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

                if let Type::Tuple(tuple_type) = &type_ {
                    let idx = match field.lexeme.parse::<u8>() {
                        Ok(idx) => idx,
                        Err(err) => {
                            return Err(TypeCheckerError::InvalidTupleIndex {
                                tuple_type: type_,
                                index: err.to_string(),
                                line: field.line,
                            });
                        }
                    };
                    if idx >= tuple_type.types.len() as u8 {
                        return Err(TypeCheckerError::InvalidTupleIndex {
                            tuple_type: type_,
                            index: idx.to_string(),
                            line: field.line,
                        });
                    }

                    return Ok(TypedExpr {
                        ty: tuple_type.types[idx as usize].clone(),
                        kind: ExprKind::SetField {
                            object: Box::from(object_typed),
                            index: idx,
                            safe: *safe,
                            value: Box::new(self.sys.verify_assignment(
                                &mut HashMap::new(),
                                &tuple_type.types[idx as usize],
                                value,
                                field.line,
                            )?),
                        },
                        line: field.line,
                    });
                }

                if let Type::Struct(struct_def, generics) = type_ {
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
                            object: Box::new(object_typed),
                            index: field_idx as u8,
                            value: Box::new(value),
                        },
                        line: field.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeHasNoFields {
                        found: type_,
                        line: object_typed.line,
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
        let field_type = struct_def
            .fields
            .get(field.lexeme)
            .map(|id| (*id, struct_def.ordered_fields[*id].1.clone()));

        match field_type {
            None => Err(TypeCheckerError::UndefinedField {
                struct_name: struct_def.name.to_string(),
                field_name: field.lexeme.to_string(),
                line: field.line,
            }),
            Some((id, field_type)) => {
                self.sys.verify_assignment(
                    &mut HashMap::new(),
                    &field_type,
                    field_value.clone(),
                    field.line,
                )?;
                Ok((id, field_type))
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
}
