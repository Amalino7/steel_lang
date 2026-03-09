use crate::parser::ast::Expr;
use crate::scanner::Token;
use crate::typechecker::core::ast::{ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::core::error::{
    GenericError, Mismatch, MismatchContext, Operand, TypeCheckerError, TypeCheckerWarning,
    TypeRequirement,
};
use crate::typechecker::core::types::Type;
use crate::typechecker::system::make_substitution_map;
use crate::typechecker::TypeChecker;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_type_specialization(
        &mut self,
        expr: &Expr<'src>,
        callee: &Expr<'src>,
        generics_provided: &[crate::parser::ast::TypeAst<'src>],
    ) -> TypedExpr {
        let mut callee_typed = self.check_expression(callee, &Type::Unknown);
        let generic_args = match self.res().resolve_many(generics_provided) {
            Ok(args) => args,
            Err(err) => {
                self.report(err);
                return callee_typed;
            }
        };

        let report_err = |this: &mut Self, msg: String| {
            this.report(TypeCheckerError::Generic(
                GenericError::InvalidSpecification {
                    span: callee_typed.span,
                    message: msg,
                },
            ));
        };

        match &callee_typed.ty.clone() {
            Type::Function(func) => {
                if func.type_params.is_empty() {
                    report_err(
                        self,
                        "Cannot specialize generic on function without generics!".to_string(),
                    );
                    return callee_typed;
                }
                if func.type_params.len() != generic_args.len() {
                    report_err(
                        self,
                        format!(
                            "Expected {} generic arguments but got {}!",
                            func.type_params.len(),
                            generic_args.len(),
                        ),
                    );
                    return callee_typed;
                }
                let map = make_substitution_map(&func.type_params, &generic_args);
                let new_ty = callee_typed.ty.generic_to_concrete(&map);
                callee_typed.ty = new_ty;
                callee_typed.span = expr.span();
                callee_typed
            }
            Type::Metatype(type_name, generics) => {
                if !generics.is_empty() {
                    report_err(
                        self,
                        "Cannot specialize generic more than once!".to_string(),
                    );
                    return callee_typed;
                }
                let actual_generic_count = self.sys.get_generic_count_by_name(type_name);
                if generic_args.len() != actual_generic_count {
                    report_err(
                        self,
                        format!(
                            "Expected {} but got {} generic arguments!",
                            actual_generic_count,
                            generic_args.len(),
                        ),
                    );
                    callee_typed
                } else {
                    callee_typed.ty = Type::Metatype(type_name.clone(), Rc::from(generic_args));
                    callee_typed.span = expr.span();
                    callee_typed
                }
            }
            _ => {
                report_err(self, "Cannot specialize non-generic type!".to_string());
                callee_typed
            }
        }
    }

    pub(crate) fn check_is(
        &mut self,
        expr: &Expr<'src>,
        expression: &Expr<'src>,
        type_name: &Token<'src>,
    ) -> TypedExpr {
        let target = self.check_expression(expression, &Type::Unknown);
        let Type::Enum(enum_name, _) = &target.ty else {
            self.report(TypeCheckerError::InvalidIsUsage {
                span: type_name.span,
                message: "Is can only be used on enum types.",
            });
            return TypedExpr {
                ty: Type::Boolean,
                kind: ExprKind::NoOp,
                span: expr.span(),
            };
        };
        let enum_def = self
            .sys
            .get_enum(enum_name.as_ref())
            .expect("Invalid enum Type return!");

        let Some(&variant_idx) = enum_def.variants.get(type_name.lexeme) else {
            self.report(TypeCheckerError::InvalidIsUsage {
                span: type_name.span,
                message: "Enum variant does not exist.",
            });
            return TypedExpr::new_blank(expr.span());
        };

        TypedExpr {
            ty: Type::Boolean,
            kind: ExprKind::Is {
                target: Box::new(target),
                variant_idx: variant_idx as u16,
            },
            span: expr.span(),
        }
    }

    pub(crate) fn check_tuple(
        &mut self,
        expr: &Expr<'src>,
        elements: &[Expr<'src>],
        expected: &Type,
    ) -> TypedExpr {
        let mut typed_elements = Vec::with_capacity(elements.len());
        let mut type_vec = vec![];

        let expected_types = match *expected {
            Type::Tuple(ref t) => Some(&t.types),
            _ => None,
        };

        for (idx, element) in elements.iter().enumerate() {
            let target_inner = expected_types.and_then(|v| v.get(idx));
            let el = self.check_expression(element, target_inner.unwrap_or(&Type::Unknown));
            type_vec.push(el.ty.clone());
            typed_elements.push(el);
        }
        let ty = Type::new_tuple(type_vec);
        TypedExpr {
            ty,
            kind: ExprKind::Tuple {
                elements: typed_elements,
            },
            span: expr.span(),
        }
    }

    pub(crate) fn check_try(
        &mut self,
        expr: &Expr<'src>,
        operator: &Token<'src>,
        expression: &Expr<'src>,
    ) -> TypedExpr {
        let typed_expr = self.check_expression(expression, &Type::Unknown);
        let span = expr.span();
        let expr_ty = typed_expr.ty.clone();

        if let Type::Enum(name, instance) = &expr_ty
            && name.as_ref() == "Result"
        {
            let enum_def = self.sys.get_enum(name.as_ref()).unwrap();
            let (_, ok_type) = enum_def
                .get_variant_from_instance("Ok", instance)
                .expect("Result type missing");
            let (_, err_type) = enum_def
                .get_variant_from_instance("Err", instance)
                .expect("Result type missing");

            let Some((func_return_type, defined)) = self.scopes.return_type() else {
                self.report(TypeCheckerError::InvalidReturnOutsideFunction {
                    span: operator.span,
                });
                return TypedExpr::new_blank(span);
            };

            let provided_err = Type::Enum(name.clone(), vec![Type::Never, err_type].into());

            if let Err(err) = self.infer_ctx.unify_types(&func_return_type, &provided_err) {
                self.report(TypeCheckerError::TypeMismatch {
                    mismatch: Mismatch::enriched(
                        &func_return_type,
                        &provided_err,
                        err,
                        &self.infer_ctx,
                    ),
                    context: MismatchContext::Return,
                    primary_span: typed_expr.span,
                    defined_at: Some(defined),
                })
            }

            TypedExpr {
                ty: ok_type,
                kind: ExprKind::Try {
                    operand: Box::new(typed_expr),
                },
                span,
            }
        } else if expr_ty != Type::Error {
            self.report(TypeCheckerError::OperatorConstraint {
                operator: "try",
                operand: Operand::Unary,
                found: expr_ty,
                requirement: TypeRequirement::Structural("Result<T, E>"),
                span: operator.span,
            });
            TypedExpr::new_blank(span)
        } else {
            TypedExpr::new_blank(span)
        }
    }

    pub(crate) fn check_coalesce(
        &mut self,
        expr: &Expr<'src>,
        _operator: &Token<'src>,
        left: &Expr<'src>,
        right: &Expr<'src>,
        expected: &Type,
    ) -> TypedExpr {
        let wrapped_expected = if *expected == Type::Unknown {
            Type::Unknown
        } else {
            expected.clone().wrap_in_optional()
        };

        let left_typed = self.check_expression(left, &wrapped_expected);
        let right_typed = self.check_expression(right, expected);
        let span = expr.span();

        let left_inner = match &left_typed.ty {
            Type::Optional(inner) => inner.as_ref().clone(),
            Type::Error => return TypedExpr::new_blank(span),
            _ => {
                self.report(TypeCheckerError::OperatorConstraint {
                    operator: "??",
                    operand: Operand::Lhs,
                    found: left_typed.ty.clone(),
                    requirement: TypeRequirement::Structural("Optional<T>"),
                    span: left_typed.span,
                });
                return TypedExpr::new_blank(span);
            }
        };

        let right_typed =
            self.coerce_typed(right_typed, &left_inner, MismatchContext::Coalesce, None);

        TypedExpr {
            ty: left_inner,
            kind: ExprKind::Logical {
                left: Box::new(left_typed),
                operator: LogicalOp::Coalesce,
                right: Box::new(right_typed),
                typed_refinements: vec![],
            },
            span,
        }
    }

    pub(crate) fn check_force_unwrap(
        &mut self,
        expr: &Expr<'src>,
        expression: &Expr<'src>,
        operator: &Token<'src>,
        expected: &Type,
    ) -> TypedExpr {
        let expected_opt = if *expected == Type::Unknown {
            Type::Unknown
        } else {
            Type::Optional(Box::new(expected.clone()))
        };

        let expr_typed = self.check_expression(expression, &expected_opt);
        match expr_typed.ty.clone() {
            Type::Optional(inner) => TypedExpr {
                ty: *inner,
                kind: ExprKind::Unary {
                    operator: UnaryOp::Unwrap,
                    operand: Box::new(expr_typed),
                },
                span: expr.span(),
            },
            _ => {
                self.warn(TypeCheckerWarning::RedundantForceUnwrap {
                    span: operator.span,
                });
                expr_typed
            }
        }
    }

    pub(crate) fn check_list(
        &mut self,
        expr: &Expr<'src>,
        elements: &[Expr<'src>],
        _bracket_token: &Token<'src>,
        expected: &Type,
    ) -> TypedExpr {
        let inner_ty = expected
            .list_element()
            .cloned()
            .unwrap_or_else(|| self.infer_ctx.new_type_var());

        let mut typed_elements = Vec::with_capacity(elements.len());

        for element in elements.iter() {
            let typed =
                self.coerce_expression(element, &inner_ty, MismatchContext::ListElement, None);
            typed_elements.push(typed);
        }

        let final_ty = Type::new_list(inner_ty);

        let inferred = self.infer_ctx.substitute(&final_ty);

        TypedExpr {
            ty: inferred,
            kind: ExprKind::List {
                elements: typed_elements,
            },
            span: expr.span(),
        }
    }
}
