use crate::parser::ast::{self, Expr, Spanned};
use crate::scanner::Span;
use crate::typechecker::TypeChecker;
use crate::typechecker::core::ast::{BinaryOp, ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::core::error::{Operand, TypeCheckerError, TypeRequirement};
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::guards::ScopeGuard;
use crate::typechecker::scope::manager::ScopeKind;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_unary_expression(
        &mut self,
        op: Spanned<ast::UnaryOp>,
        expr: &Expr<'src>,
        total_span: Span,
    ) -> TypedExpr {
        let (hint_ty, unary_op) = match op.node {
            ast::UnaryOp::Not => (Type::Boolean, UnaryOp::Not),
            ast::UnaryOp::Negate => (Type::Number, UnaryOp::Negate),
        };
        let op_str = op.node.as_str();

        let typed_operand = self.check_expression(expr, &hint_ty);

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
        op: Spanned<ast::LogicalOp>,
        left: &Expr<'src>,
        right: &Expr<'src>,
        span: Span,
    ) -> TypedExpr {
        let op_str = op.node.as_str();

        let left_typed = self.check_expression(left, &Type::Boolean);

        self.check_operand(
            Type::Boolean,
            &left_typed.ty,
            Operand::Lhs,
            op_str,
            left_typed.span,
        );

        let refinements = self.analyze_condition(&left_typed);
        let refinement_path = match op.node {
            ast::LogicalOp::And => refinements.true_path,
            ast::LogicalOp::Or => refinements.false_path,
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

        let operator = match op.node {
            ast::LogicalOp::And => LogicalOp::And,
            ast::LogicalOp::Or => LogicalOp::Or,
        };

        TypedExpr {
            ty: Type::Boolean,
            kind: ExprKind::Logical {
                left: Box::new(left_typed),
                operator,
                right: Box::new(right_typed),
                typed_refinements,
            },
            span,
        }
    }

    /// An operand type error is reported on the operator.
    pub(crate) fn check_binary_expression(
        &mut self,
        op: Spanned<ast::BinaryOp>,
        left: &Expr<'src>,
        right: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        use ast::BinaryOp as Op;
        let op_str = op.node.as_str().to_string();
        match op.node {
            Op::Subtract | Op::Divide | Op::Multiply | Op::Modulo | Op::Power => {
                let left_typed = self.check_expression(left, &Type::Number);
                let right_typed = self.check_expression(right, &Type::Number);

                let left_type = left_typed.ty.clone();
                let right_type = right_typed.ty.clone();
                let total_span = left_typed.span.merge(right_typed.span);

                if left_type == Type::Error || right_type == Type::Error {
                    return Ok(TypedExpr::black_with_type(Type::Number, total_span));
                }

                if left_type != Type::Number || right_type != Type::Number {
                    let err = TypeCheckerError::new_invalid_operand(
                        op_str,
                        left_type,
                        right_type,
                        op.span,
                        "Arithmetic operators require both operands to be numbers.",
                    );
                    return Err(err);
                }

                let operator = match op.node {
                    Op::Subtract => BinaryOp::Subtract,
                    Op::Divide => BinaryOp::Divide,
                    Op::Multiply => BinaryOp::Multiply,
                    Op::Modulo => BinaryOp::Modulo,
                    _ => BinaryOp::Power,
                };
                Ok(TypedExpr {
                    ty: Type::Number,
                    kind: ExprKind::Binary {
                        left: Box::new(left_typed),
                        operator,
                        right: Box::new(right_typed),
                    },
                    span: total_span,
                })
            }

            Op::Add => {
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
                    let err = TypeCheckerError::new_invalid_operand(
                        op_str,
                        left_type,
                        right_type,
                        op.span,
                        "The '+' operator requires both operands to be numbers, or both to be strings (concatenation).",
                    );
                    Err(err)
                }
            }

            Op::Greater | Op::GreaterEqual | Op::Less | Op::LessEqual => {
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
                    let err = TypeCheckerError::new_invalid_operand(
                        op_str,
                        left_type,
                        right_type,
                        op.span,
                        "Comparison operators require both operands to be numbers or both to be strings.",
                    );
                    return Err(err);
                }

                let is_string = left_type == Type::String;
                let operator = match (op.node, is_string) {
                    (Op::Greater, true) => BinaryOp::GreaterString,
                    (Op::GreaterEqual, true) => BinaryOp::GreaterEqualString,
                    (Op::Less, true) => BinaryOp::LessString,
                    (Op::LessEqual, true) => BinaryOp::LessEqualString,
                    (Op::Greater, false) => BinaryOp::GreaterNumber,
                    (Op::GreaterEqual, false) => BinaryOp::GreaterEqualNumber,
                    (Op::Less, false) => BinaryOp::LessNumber,
                    _ => BinaryOp::LessEqualNumber,
                };

                Ok(TypedExpr {
                    ty: Type::Boolean,
                    kind: ExprKind::Binary {
                        left: Box::new(left_typed),
                        operator,
                        right: Box::new(right_typed),
                    },
                    span: total_span,
                })
            }

            Op::Equal | Op::NotEqual => {
                let left_typed = self.check_expression(left, &Type::Unknown);
                let right_typed = self.check_expression(right, &left_typed.ty);

                let left_type = left_typed.ty.clone();
                let right_type = right_typed.ty.clone();
                let total_span = left_typed.span.merge(right_typed.span);

                if !Type::can_compare(&left_type, &right_type) {
                    let err = TypeCheckerError::new_invalid_operand(
                        op_str,
                        left_type,
                        right_type,
                        op.span,
                        "Values can only be compared with '==' and '!=' if they have compatible types.",
                    );
                    return Err(err);
                }

                let operator = match &left_type {
                    Type::Number => BinaryOp::EqualEqualNumber,
                    Type::String => BinaryOp::EqualEqualString,
                    _ => BinaryOp::EqualEqual,
                };
                let binary_expr = TypedExpr {
                    ty: Type::Boolean,
                    kind: ExprKind::Binary {
                        left: Box::new(left_typed),
                        operator,
                        right: Box::new(right_typed),
                    },
                    span: total_span,
                };
                if op.node == Op::NotEqual {
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
