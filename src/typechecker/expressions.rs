use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Expr, Literal};
use crate::token::{Token, TokenType};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::error::TypeCheckerError::AssignmentToCapturedVariable;
use crate::typechecker::type_ast::{BinaryOp, ExprKind, Type, TypedExpr, UnaryOp};
use crate::typechecker::TypeChecker;

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
                            expr: ExprKind::Unary {
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
                            expr: ExprKind::Unary {
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
                let var = self.lookup_variable(name.lexeme);
                if let Some((ctx, resolved)) = var {
                    Ok(TypedExpr {
                        ty: ctx.type_info.clone(),
                        expr: ExprKind::GetVar(resolved),
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
                };

                Ok(TypedExpr {
                    ty,
                    expr: ExprKind::Literal(literal.clone()),
                    line: *line,
                })
            }
            Expr::Assignment { identifier, value } => {
                let typed_value = self.infer_expression(value)?;
                let var_lookup = self.lookup_variable(identifier.lexeme);

                if let Some((ctx, resolved)) = var_lookup {
                    if typed_value.ty != ctx.type_info {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: ctx.type_info.clone(),
                            found: typed_value.ty,
                            line: identifier.line,
                            message: "Expected the same type but found something else.",
                        });
                    }

                    if let ResolvedVar::Closure(_) = &resolved {
                        return Err(AssignmentToCapturedVariable {
                            name: ctx.name.to_string(),
                            line: identifier.line,
                        });
                    }

                    // Assignments evaluate to the assigned value
                    Ok(TypedExpr {
                        ty: typed_value.ty.clone(),
                        expr: ExprKind::Assign {
                            target: resolved,
                            value: Box::new(typed_value),
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
                    TokenType::And => BinaryOp::And,
                    TokenType::Or => BinaryOp::Or,
                    _ => unreachable!("Invalid logical operator"),
                };

                Ok(TypedExpr {
                    ty: Type::Boolean,
                    expr: ExprKind::Binary {
                        left: Box::new(left_typed),
                        operator: op,
                        right: Box::new(right_typed),
                    },
                    line: operator.line,
                })
            }
            Expr::Call { callee, arguments } => {
                let callee_typed = self.infer_expression(callee)?;
                if let Type::Function(func) = callee_typed.ty.clone() {
                    if func.param_types.len() != arguments.len() {
                        return Err(TypeCheckerError::IncorrectArity {
                            callee_name: callee.to_string(),
                            expected: func.param_types.len(),
                            found: arguments.len(),
                            line: callee_typed.line,
                        });
                    }

                    let mut typed_args = Vec::with_capacity(arguments.len());

                    for (i, arg) in arguments.iter().enumerate() {
                        let arg_typed = self.infer_expression(arg)?;

                        if func.param_types[i] != Type::Any && arg_typed.ty != func.param_types[i] {
                            return Err(TypeCheckerError::FunctionParameterTypeMismatch {
                                expected: func.param_types[i].clone(),
                                found: arg_typed.ty,
                                param_index: i,
                                line: arg_typed.line,
                            });
                        }
                        typed_args.push(arg_typed);
                    }

                    Ok(TypedExpr {
                        ty: func.return_type.clone(),
                        line: callee_typed.line,
                        expr: ExprKind::Call {
                            callee: Box::new(callee_typed),
                            arguments: typed_args,
                        },
                    })
                } else {
                    Err(TypeCheckerError::CalleeIsNotAFunction {
                        found: callee_typed.ty,
                        line: callee_typed.line,
                    })
                }
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
                        expr: ExprKind::Binary {
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
                        expr: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: BinaryOp::Add,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    })
                } else if left_type == Type::String && right_type == Type::String {
                    Ok(TypedExpr {
                        ty: Type::String,
                        expr: ExprKind::Binary {
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
                    let op = match operator.token_type {
                        TokenType::Greater => BinaryOp::Greater,
                        TokenType::GreaterEqual => BinaryOp::GreaterEqual,
                        TokenType::Less => BinaryOp::Less,
                        TokenType::LessEqual => BinaryOp::LessEqual,
                        _ => unreachable!(),
                    };

                    Ok(TypedExpr {
                        ty: Type::Boolean,
                        expr: ExprKind::Binary {
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
                if left_type == right_type {
                    let binary_expr = TypedExpr {
                        ty: Type::Boolean,
                        expr: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: BinaryOp::EqualEqual,
                            right: Box::new(right_typed),
                        },
                        line: operator.line,
                    };

                    if operator.token_type == TokenType::BangEqual {
                        Ok(TypedExpr {
                            ty: Type::Boolean,
                            expr: ExprKind::Unary {
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
