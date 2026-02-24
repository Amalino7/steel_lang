mod access;
mod calls;
mod misc;
mod operators;
pub(crate) mod static_access;

use crate::compiler::analysis::ResolvedVar;
use crate::compiler::analysis::ResolvedVar::Global;
use crate::parser::ast::{Expr, Literal};
use crate::scanner::TokenType;
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::error::TypeCheckerError::AssignmentToCapturedVariable;
use crate::typechecker::core::types::Type;
use crate::typechecker::similarity::find_similar;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_expression(
        &mut self,
        expr: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        match expr {
            Expr::TypeSpecialization {
                callee,
                generics: generics_provided,
            } => self.check_type_specialization(expr, callee, generics_provided),

            Expr::Is {
                expression,
                type_name,
            } => self.check_is(expr, expression, type_name),

            Expr::Tuple { elements } => self.check_tuple(expr, elements, expected),

            Expr::Unary {
                operator,
                expression,
            } if operator.token_type == TokenType::Try => {
                self.check_try(expr, operator, expression)
            }

            Expr::Unary {
                operator,
                expression,
            } => self.check_unary_expression(operator, expression),

            Expr::Binary {
                operator,
                left,
                right,
            } => self.check_binary_expression(operator, left, right, expected),

            Expr::Variable { name } => {
                let var = self.scopes.lookup(name.lexeme);
                if let Some((ctx, resolved)) = var {
                    Ok(TypedExpr {
                        ty: ctx.type_info.clone(),
                        kind: ExprKind::GetVar(resolved, ctx.name.clone()),
                        span: name.span,
                    })
                } else if let Some(type_name) = self.res().get_owned_name(name.lexeme) {
                    Ok(TypedExpr {
                        ty: Type::Metatype(type_name.clone(), vec![].into()),
                        kind: ExprKind::GetVar(Global(0), type_name),
                        span: name.span,
                    })
                } else {
                    let visible_vars = self.scopes.visible_variable_names();
                    let suggestions = find_similar(name.lexeme, visible_vars, 3);
                    if name.lexeme == "self" {
                        return Err(TypeCheckerError::SelfOutsideOfImpl { span: name.span });
                    }

                    Err(TypeCheckerError::UndefinedVariable {
                        name: name.lexeme.to_string(),
                        span: name.span,
                        suggestions,
                    })
                }
            }

            Expr::Grouping { expression } => self.check_expression(expression, expected),

            Expr::Literal { literal, span } => {
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
                    span: *span,
                })
            }

            Expr::Assignment { identifier, value } => {
                let var_lookup = self.scopes.lookup_for_write(identifier.lexeme);
                if let Some((ctx, resolved)) = var_lookup {
                    if let ResolvedVar::Closure(_) = &resolved {
                        return Err(AssignmentToCapturedVariable {
                            name: ctx.name.to_string(),
                            span: identifier.span,
                            capture_origin: ctx.span,
                        });
                    }

                    if !ctx.is_reassignable() {
                        return Err(TypeCheckerError::AssignmentToImmutableBinding {
                            kind: ctx.kind,
                            name: ctx.name.to_string(),
                            span: identifier.span,
                            definition_span: ctx.span,
                        });
                    }

                    let (resolved, coerced_value) =
                        if let Some((old_resolved, old_ty)) = ctx.original_type.clone() {
                            let coerced_value = self.coerce_expression(value, &old_ty)?;
                            if let Type::Optional(inner) = &old_ty
                                && inner.as_ref() == &coerced_value.ty
                            {
                                self.scopes.widen_type(
                                    identifier.lexeme,
                                    coerced_value.ty.clone(),
                                    old_resolved.clone(),
                                )
                            } else {
                                self.scopes.widen_type(
                                    identifier.lexeme,
                                    old_ty.clone(),
                                    old_resolved.clone(),
                                );
                            }
                            (old_resolved, coerced_value)
                        } else {
                            // This bug here took way too long.
                            let expected = ctx.type_info.clone();
                            let coerced_value = self.coerce_expression(value, &expected)?;
                            (resolved, coerced_value)
                        };

                    Ok(TypedExpr {
                        ty: coerced_value.ty.clone(),
                        kind: ExprKind::Assign {
                            target: resolved,
                            value: Box::new(coerced_value),
                        },
                        span: expr.span(),
                    })
                } else {
                    if identifier.lexeme == "self" {
                        return Err(TypeCheckerError::SelfOutsideOfImpl {
                            span: identifier.span,
                        });
                    }
                    let visible_vars = self.scopes.visible_variable_names();
                    let suggestions = find_similar(identifier.lexeme, visible_vars, 3);
                    Err(TypeCheckerError::UndefinedVariable {
                        name: identifier.lexeme.to_string(),
                        span: identifier.span,
                        suggestions,
                    })
                }
            }

            Expr::Logical {
                operator,
                left,
                right,
            } if operator.token_type == TokenType::QuestionQuestion => {
                self.check_coalesce(expr, operator, left, right, expected)
            }

            Expr::Logical {
                left,
                operator,
                right,
            } => self.check_logical_expression(operator, left, right),

            Expr::Call {
                callee,
                arguments,
                safe,
            } => self.check_call(expr, callee, arguments, safe, expected),

            Expr::Get {
                object,
                field,
                safe,
            } => self.check_get(expr, object, field, safe),

            Expr::Set {
                object,
                field,
                value,
                safe,
            } => self.check_set(object, field, value, safe),

            Expr::ForceUnwrap {
                expression,
                operator,
            } => self.check_force_unwrap(expr, expression, operator, expected),

            Expr::List {
                elements,
                bracket_token,
            } => self.check_list(expr, elements, bracket_token, expected),

            Expr::Map { .. } => {
                todo!()
            }

            Expr::GetIndex {
                safe,
                object,
                index,
            } => self.check_get_index(expr, safe, object, index),

            Expr::SetIndex {
                safe,
                object,
                index,
                value,
            } => self.check_set_index(expr, safe, object, index, value),

            Expr::Error => {
                // Already reported by parser
                Ok(TypedExpr::new_blank(expr.span()))
            }
        }
    }

    pub(crate) fn coerce_expression(
        &mut self,
        expr: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mut expr = self.check_expression(expr, expected)?;
        let res = self.infer_ctx.unify_types(expected, &expr.ty);
        if res.is_ok() {
            let expr_ty = self.infer_ctx.substitute(&expr.ty);
            return if expr_ty.is_concrete() {
                expr.ty = expr_ty;
                Ok(expr)
            } else {
                let uninferred_generics = self.infer_ctx.uninferred_names(&expr_ty);
                Err(TypeCheckerError::CannotInferType {
                    span: expr.span,
                    uninferred_generics,
                })
            };
        }

        let (expected_type, was_optional) = if let Type::Optional(inner) = expected {
            (inner.as_ref(), true)
        } else {
            (expected, false)
        };

        if let (Type::Interface(iface_name), Some(name)) = (expected_type, expr.ty.get_name())
            && let Some(idx) = self.sys.get_vtable_idx(name, iface_name.clone())
        {
            let result_ty = if was_optional {
                Type::Optional(Box::new(Type::Interface(iface_name.clone())))
            } else {
                Type::Interface(iface_name.clone())
            };
            return Ok(TypedExpr {
                ty: result_ty,
                span: expr.span,
                kind: ExprKind::InterfaceUpcast {
                    expr: Box::new(expr),
                    vtable_idx: idx,
                },
            });
        }

        res.map_err(|msg| TypeCheckerError::ComplexTypeMismatch {
            expected: self.infer_ctx.substitute(expected),
            span: expr.span,
            message: msg.into(),
            found: expr.ty.clone(),
        })
        .map(|_| expr)
    }
}
