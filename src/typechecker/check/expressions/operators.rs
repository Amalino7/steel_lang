use crate::parser::ast::Expr;
use crate::scanner::{Span, Token, TokenType};
use crate::typechecker::core::ast::{BinaryOp, ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::core::error::{Operand, TypeCheckerError, TypeRequirement};
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::guards::ScopeGuard;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_unary_expression(
        &mut self,
        operator: &Token,
        expr: &Expr<'src>,
    ) -> TypedExpr {
        let (hint_ty, unary_op, op_str) = if operator.token_type == TokenType::Bang {
            (Type::Boolean, UnaryOp::Not, "!")
        } else if operator.token_type == TokenType::Minus {
            (Type::Number, UnaryOp::Negate, "-")
        } else {
            unreachable!("Ast should have checked for invalid operators before this point.")
        };

        let typed_operand = self.check_expression(expr, &hint_ty);
        let total_span = typed_operand.span.merge(operator.span);

        let is_success = self.check_operand(
            hint_ty.clone(),
            &typed_operand.ty,
            Operand::Unary,
            op_str,
            typed_operand.span,
        );
        if !is_success {
            return TypedExpr::new_blank(total_span);
        }

        TypedExpr {
            ty: hint_ty,
            kind: ExprKind::Unary {
                operator: unary_op,
                operand: Box::new(typed_operand),
            },
            span: total_span,
        }
    }

    pub(crate) fn check_logical_expression(
        &mut self,
        operator: &Token,
        left: &Expr<'src>,
        right: &Expr<'src>,
    ) -> TypedExpr {
        let op_str = if TokenType::And == operator.token_type {
            "and"
        } else {
            "or"
        };

        let left_typed = self.check_expression(left, &Type::Boolean);

        self.check_operand(
            Type::Boolean,
            &left_typed.ty,
            Operand::Lhs,
            op_str,
            left_typed.span,
        );

        let refinements = self.analyze_condition(&left_typed);
        let refinement_path = if TokenType::And == operator.token_type {
            refinements.true_path
        } else if TokenType::Or == operator.token_type {
            refinements.false_path
        } else {
            unreachable!("Invalid logical operator");
        };

        let (right_typed, typed_refinements) = {
            let mut guard = ScopeGuard::new(self, ScopeKind::Block);
            let mut typed_refinements = vec![];
            for (name, ty) in refinement_path {
                if let Some(case) = guard.scopes.refine(&name, ty) {
                    typed_refinements.push(case);
                }
            }
            let right_typed = guard.check_expression(right, &Type::Boolean);
            (right_typed, typed_refinements)
        };

        self.check_operand(
            Type::Boolean,
            &right_typed.ty,
            Operand::Rhs,
            op_str,
            right_typed.span,
        );

        let op = match operator.token_type {
            TokenType::And => LogicalOp::And,
            TokenType::Or => LogicalOp::Or,
            _ => unreachable!("Invalid logical operator"),
        };

        TypedExpr {
            ty: Type::Boolean,
            kind: ExprKind::Logical {
                left: Box::new(left_typed),
                operator: op,
                right: Box::new(right_typed),
                typed_refinements,
            },
            span: operator.span,
        }
    }

    pub(crate) fn check_binary_expression(
        &mut self,
        operator: &Token,
        left: &Expr<'src>,
        right: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        match operator.token_type {
            TokenType::Minus | TokenType::Slash | TokenType::Star => {
                let left_typed = self.check_expression(left, &Type::Number);
                let right_typed = self.check_expression(right, &Type::Number);

                let left_type = left_typed.ty.clone();
                let right_type = right_typed.ty.clone();
                let total_span = left_typed.span.merge(right_typed.span);

                if left_type == Type::Error || right_type == Type::Error {
                    return Ok(TypedExpr::black_with_type(Type::Number, total_span));
                }

                if left_type != Type::Number || right_type != Type::Number {
                    return Err(TypeCheckerError::InvalidOperandTypes {
                        operator: operator.lexeme.to_string(),
                        left: left_type,
                        right: right_type,
                        span: total_span,
                        help: "Arithmetic operators require both operands to be numbers.",
                    });
                }

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
                    span: total_span,
                })
            }

            TokenType::Plus => {
                let left_hint = match expected {
                    Type::Number | Type::String => expected.clone(),
                    _ => Type::Unknown,
                };
                let left_typed = self.check_expression(left, &left_hint);
                let right_hint = match &left_typed.ty {
                    Type::Number | Type::String => left_typed.ty.clone(),
                    _ => Type::Unknown,
                };
                let right_typed = self.check_expression(right, &right_hint);

                let left_type = left_typed.ty.clone();
                let right_type = right_typed.ty.clone();
                let total_span = left_typed.span.merge(right_typed.span);
                if left_type == Type::Error || right_type == Type::Error {
                    return Ok(TypedExpr::new_blank(total_span));
                }
                if left_type == Type::Number && right_type == Type::Number {
                    Ok(TypedExpr {
                        ty: Type::Number,
                        kind: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: BinaryOp::Add,
                            right: Box::new(right_typed),
                        },
                        span: total_span,
                    })
                } else if left_type == Type::String && right_type == Type::String {
                    Ok(TypedExpr {
                        ty: Type::String,
                        kind: ExprKind::Binary {
                            left: Box::new(left_typed),
                            operator: BinaryOp::Concat,
                            right: Box::new(right_typed),
                        },
                        span: total_span,
                    })
                } else {
                    Err(TypeCheckerError::InvalidOperandTypes {
                        operator: operator.lexeme.to_string(),
                        left: left_type,
                        right: right_type,
                        span: total_span,
                        help: "The '+' operator requires both operands to be numbers, or both to be strings (concatenation).",
                    })
                }
            }

            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => {
                let left_typed = self.check_expression(left, &Type::Unknown);
                let right_typed = self.check_expression(right, &left_typed.ty);

                let left_type = left_typed.ty.clone();
                let right_type = right_typed.ty.clone();
                let total_span = left_typed.span.merge(right_typed.span);

                if left_type == Type::Error || right_type == Type::Error {
                    return Ok(TypedExpr::black_with_type(Type::Boolean, total_span));
                }

                let cmp_compatible = (left_type == Type::Number || left_type == Type::String)
                    && left_type == right_type;
                if !cmp_compatible {
                    return Err(TypeCheckerError::InvalidOperandTypes {
                        operator: operator.lexeme.to_string(),
                        left: left_type,
                        right: right_type,
                        span: total_span,
                        help: "Comparison operators require both operands to be numbers or both to be strings.",
                    });
                }

                let op = match (operator.token_type.clone(), &left_type) {
                    (TokenType::Greater, Type::String) => BinaryOp::GreaterString,
                    (TokenType::GreaterEqual, Type::String) => BinaryOp::GreaterEqualString,
                    (TokenType::Less, Type::String) => BinaryOp::LessString,
                    (TokenType::LessEqual, Type::String) => BinaryOp::LessEqualString,
                    (TokenType::Greater, Type::Number) => BinaryOp::GreaterNumber,
                    (TokenType::GreaterEqual, Type::Number) => BinaryOp::GreaterEqualNumber,
                    (TokenType::Less, Type::Number) => BinaryOp::LessNumber,
                    (TokenType::LessEqual, Type::Number) => BinaryOp::LessEqualNumber,
                    _ => unreachable!("Invalid comparison operator"),
                };

                Ok(TypedExpr {
                    ty: Type::Boolean,
                    kind: ExprKind::Binary {
                        left: Box::new(left_typed),
                        operator: op,
                        right: Box::new(right_typed),
                    },
                    span: total_span,
                })
            }

            TokenType::EqualEqual | TokenType::BangEqual => {
                let left_typed = self.check_expression(left, &Type::Unknown);
                let right_typed = self.check_expression(right, &left_typed.ty);

                let left_type = left_typed.ty.clone();
                let right_type = right_typed.ty.clone();
                let total_span = left_typed.span.merge(right_typed.span);

                if !Type::can_compare(&left_type, &right_type) {
                    return Err(TypeCheckerError::InvalidOperandTypes {
                        operator: operator.lexeme.to_string(),
                        left: left_type,
                        right: right_type,
                        span: total_span,
                        help: "Values can only be compared with '==' and '!=' if they have compatible types.",
                    });
                }

                let op = match &left_type {
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
                    span: total_span,
                };
                if operator.token_type == TokenType::BangEqual {
                    Ok(TypedExpr {
                        ty: Type::Boolean,
                        kind: ExprKind::Unary {
                            operator: UnaryOp::Not,
                            operand: Box::new(binary_expr),
                        },
                        span: total_span,
                    })
                } else {
                    Ok(binary_expr)
                }
            }

            _ => {
                unreachable!("Ast should be checked for invalid operators before this point.")
            }
        }
    }

    pub(crate) fn check_operand(
        &mut self,
        expected: Type,
        found: &Type,
        operand: Operand,
        operator: &'static str,
        span: Span,
    ) -> bool {
        if self.infer_ctx.unify_types(&expected, found).is_err() {
            self.report(TypeCheckerError::OperatorConstraint {
                operator,
                operand,
                found: found.clone(),
                requirement: TypeRequirement::Exact(expected),
                span,
            });
            return false;
        }
        true
    }
}
