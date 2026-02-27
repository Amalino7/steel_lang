use crate::parser::ast::Expr;
use crate::typechecker::core::ast::{ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::core::error::{Recoverable, TypeCheckerError};
use crate::typechecker::core::types::Type;
use crate::typechecker::system::generics_to_map;
use crate::typechecker::TypeChecker;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_type_specialization(
        &mut self,
        expr: &Expr<'src>,
        callee: &Expr<'src>,
        generics_provided: &[crate::parser::ast::TypeAst<'src>],
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mut callee_typed = self.check_expression(callee, &Type::Unknown)?;
        let generic_args = self.res().resolve_many(generics_provided)?;
        match &mut callee_typed.ty {
            Type::Function(func) => {
                if func.type_params.len() != generic_args.len() {
                    return Err(TypeCheckerError::InvalidGenericSpecification {
                        span: callee_typed.span,
                        message: format!(
                            "Expected {} generic arguments but got {}!",
                            func.type_params.len(),
                            generic_args.len(),
                        ),
                    });
                }
                if func.type_params.is_empty() {
                    return Err(TypeCheckerError::InvalidGenericSpecification {
                        span: callee_typed.span,
                        message: "Cannot specialize generic on function without generics!"
                            .to_string(),
                    });
                }
                let map = generics_to_map(&func.type_params, &generic_args, None);
                let new_ty = callee_typed.ty.generic_to_concrete(&map);
                callee_typed.ty = new_ty;
                callee_typed.span = expr.span();
                Ok(callee_typed)
            }
            Type::Metatype(type_name, generics) => {
                if !generics.is_empty() {
                    return Err(TypeCheckerError::InvalidGenericSpecification {
                        span: callee_typed.span,
                        message: "Cannot specialize generic more than once!".to_string(),
                    });
                }
                let actual_generic_count = self.sys.get_generic_count_by_name(type_name);
                if generic_args.len() != actual_generic_count {
                    Err(TypeCheckerError::InvalidGenericSpecification {
                        span: callee_typed.span,
                        message: format!(
                            "Expected {} but got {} generic arguments!",
                            actual_generic_count,
                            generic_args.len(),
                        ),
                    })
                } else {
                    callee_typed.ty = Type::Metatype(type_name.clone(), Rc::from(generic_args));
                    callee_typed.span = expr.span();
                    Ok(callee_typed)
                }
            }
            _ => Err(TypeCheckerError::InvalidGenericSpecification {
                span: callee_typed.span,
                message: "Cannot specialize non-generic type!".to_string(),
            }),
        }
    }

    pub(crate) fn check_is(
        &mut self,
        expr: &Expr<'src>,
        expression: &Expr<'src>,
        type_name: &crate::scanner::Token,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let target = self.check_expression(expression, &Type::Unknown)?;
        let Type::Enum(enum_name, _) = &target.ty else {
            return Err(TypeCheckerError::InvalidIsUsage {
                span: type_name.span,
                message: "Is can only be used on enum types.",
            });
        };
        let enum_def = self
            .sys
            .get_enum(enum_name.as_ref())
            .expect("Invalid enum Type return!");

        if !enum_def.variants.contains_key(type_name.lexeme) {
            return Err(TypeCheckerError::InvalidIsUsage {
                span: type_name.span,
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
            span: expr.span(),
        })
    }

    pub(crate) fn check_tuple(
        &mut self,
        expr: &Expr<'src>,
        elements: &[Expr<'src>],
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mut typed_elements = Vec::with_capacity(elements.len());
        let mut type_vec = vec![];

        let expected_types = match *expected {
            Type::Tuple(ref t) => Some(&t.types),
            _ => None,
        };

        for (idx, element) in elements.iter().enumerate() {
            let target_inner = expected_types.and_then(|v| v.get(idx));
            let el = self.coerce_expression(element, target_inner.unwrap_or(&Type::Unknown))?;
            type_vec.push(el.ty.clone());
            typed_elements.push(el);
        }
        let ty = Type::new_tuple(type_vec);
        Ok(TypedExpr {
            ty,
            kind: ExprKind::Tuple {
                elements: typed_elements,
            },
            span: expr.span(),
        })
    }

    pub(crate) fn check_try(
        &mut self,
        expr: &Expr<'src>,
        operator: &crate::scanner::Token,
        expression: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        // TODO add suggested type
        let mut typed_expr = self.check_expression(expression, &Type::Unknown)?;
        if let Type::Enum(name, instance) = &mut typed_expr.ty
            && name.as_ref() == "Result"
        {
            let enum_def = self.sys.get_enum(name.as_ref()).unwrap();

            let (_, ok_type) = enum_def
                .get_variant_from_instance("Ok", instance)
                .expect("Result type missing");
            let (_, err_type) = enum_def
                .get_variant_from_instance("Err", instance)
                .expect("Result type missing");

            if let Some((func_return_type, _)) = self.scopes.return_type() {
                let provided_err = Type::Enum(name.clone(), vec![Type::Never, err_type].into());
                let ok = self.infer_ctx.unify_types(&func_return_type, &provided_err);
                if ok.is_err() {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: func_return_type,
                        found: provided_err,
                        span: typed_expr.span,
                        message: "The Result type propagate by try must be compatible with the function return type.",
                    });
                }
            } else {
                return Err(TypeCheckerError::InvalidReturnOutsideFunction {
                    span: operator.span,
                });
            }

            Ok(TypedExpr {
                ty: ok_type,
                kind: ExprKind::Try {
                    operand: Box::new(typed_expr),
                },
                span: expr.span(),
            })
        } else {
            Err(TypeCheckerError::TypeMismatch {
                expected: Type::Enum("Result".into(), vec![Type::Any, Type::Any].into()),
                found: typed_expr.ty,
                span: operator.span,
                message: "Try works only on the Result enum.",
            })
        }
    }

    pub(crate) fn check_coalesce(
        &mut self,
        expr: &Expr<'src>,
        operator: &crate::scanner::Token,
        left: &Expr<'src>,
        right: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let wrapped_expected = if *expected == Type::Unknown {
            Type::Unknown
        } else {
            expected.clone().wrap_in_optional()
        };

        let left_typed = self.check_expression(left, &wrapped_expected)?;
        let right_typed = self.check_expression(right, expected)?;
        let left_inner = match &left_typed.ty {
            Type::Optional(inner) => inner.as_ref(),
            _ => {
                return Err(TypeCheckerError::TypeMismatch {
                    expected: Type::Optional(Box::new(Type::Any)),
                    found: left_typed.ty,
                    span: left_typed.span,
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
                span: expr.span(),
            })
        } else {
            Err(TypeCheckerError::TypeMismatch {
                expected: left_inner.clone(),
                found: right_typed.ty,
                span: operator.span,
                message: "Cannot coalesce different types.",
            })
        }
    }

    pub(crate) fn check_force_unwrap(
        &mut self,
        expr: &Expr<'src>,
        expression: &Expr<'src>,
        operator: &crate::scanner::Token,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let expected_opt = if *expected == Type::Unknown {
            Type::Unknown
        } else {
            Type::Optional(Box::new(expected.clone()))
        };

        let expr_typed = self.check_expression(expression, &expected_opt)?;
        match expr_typed.ty.clone() {
            Type::Optional(inner) => Ok(TypedExpr {
                ty: *inner,
                kind: ExprKind::Unary {
                    operator: UnaryOp::Unwrap,
                    operand: Box::new(expr_typed),
                },
                span: expr.span(),
            }),
            _ => {
                // Emit warning instead of error
                self.warnings.push(
                    crate::typechecker::core::error::TypeCheckerWarning::RedundantForceUnwrap {
                        span: operator.span,
                    },
                );
                // Return the value unchanged
                Ok(expr_typed)
            }
        }
    }

    pub(crate) fn check_list(
        &mut self,
        expr: &Expr<'src>,
        elements: &[Expr<'src>],
        bracket_token: &crate::scanner::Token,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let expected_inner = match expected {
            Type::Optional(inner) => inner.as_ref().list_element().cloned(),
            other => other.list_element().cloned(),
        };

        if elements.is_empty() {
            let inner = expected_inner.ok_or(TypeCheckerError::CannotInferType {
                span: bracket_token.span,
                uninferred_generics: vec!["Val".to_string()],
            })?;
            return Ok(TypedExpr {
                ty: Type::new_list(inner),
                kind: ExprKind::List { elements: vec![] },
                span: expr.span(),
            });
        }

        let mut typed_elements = Vec::with_capacity(elements.len());
        let inner_ty = expected_inner.unwrap_or_else(|| self.infer_ctx.new_type_var());

        for element in elements.iter() {
            let typed = self
                .coerce_expression(element, &inner_ty)
                .recover(&mut self.errors, TypedExpr::new_blank(element.span()));
            typed_elements.push(typed);
        }

        let inferred_inner = self.infer_ctx.substitute(&inner_ty);
        if !inferred_inner.is_concrete() {
            let uninferred_generics = self.infer_ctx.uninferred_names(&inferred_inner);
            return Err(TypeCheckerError::CannotInferType {
                span: bracket_token.span,
                uninferred_generics,
            });
        }

        Ok(TypedExpr {
            ty: Type::new_list(inferred_inner),
            kind: ExprKind::List {
                elements: typed_elements,
            },
            span: expr.span(),
        })
    }
}
