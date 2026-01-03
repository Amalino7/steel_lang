use crate::parser::ast::Expr;
use crate::token::{Token, TokenType};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{BinaryOp, ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::Type;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_unary_expression(
        &mut self,
        operator: &Token,
        expr: &Expr<'src>,
        expected_type: Option<&Type>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let typed_operand = self.check_expression(expr, expected_type)?;
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

    pub(crate) fn check_logical_expression(
        &mut self,
        operator: &Token,
        left: &Expr<'src>,
        right: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let left_typed = self.check_expression(left, Some(&Type::Boolean))?;
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
        let right_typed = self.check_expression(right, Some(&Type::Boolean))?;
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
    pub(crate) fn check_binary_expression(
        &mut self,
        operator: &Token,
        left: &Expr<'src>,
        right: &Expr<'src>,
        expected_type: Option<&Type>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        // TODO consider complex heuristic
        let left_typed = self.check_expression(left, None)?;
        let right_typed = self.check_expression(right, None)?;

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
                if TypeSystem::can_compare(&left_type, &right_type) {
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
