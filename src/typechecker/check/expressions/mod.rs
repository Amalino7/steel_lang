mod access;
mod calls;
mod flow;
mod misc;
mod operators;
pub(crate) mod static_access;

use crate::compiler::analysis::ResolvedVar;
use crate::compiler::analysis::ResolvedVar::Global;
use crate::parser::ast::{Expr, ExprKind as AstKind, Literal, StringPart};
use crate::scanner::Span;
use crate::typechecker::TypeChecker;
use crate::typechecker::core::ast::{ExprKind, TypedExpr, TypedStringPart};
use crate::typechecker::core::error::{
    BindingError, GenericError, Mismatch, MismatchContext, Recoverable, TypeCheckerError,
};
use crate::typechecker::core::types::Type;
use crate::typechecker::inference::{UnificationError, UnificationErrorKind};
use crate::typechecker::similarity::find_similar;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_expression(&mut self, expr: &Expr<'src>, expected: &Type) -> TypedExpr {
        let span = expr.span;
        let fallback = || TypedExpr::new_blank(span);
        match &expr.kind {
            AstKind::TypeSpecialization { callee, type_args } => {
                self.check_type_specialization(expr, callee, &type_args.items)
            }

            AstKind::Is { operand, variant } => self.check_is(expr, operand, variant),

            AstKind::Tuple(elements) => self.check_tuple(expr, elements, expected),

            AstKind::Try { keyword, operand } => self.check_try(expr, *keyword, operand),

            AstKind::Unary { op, operand } => self.check_unary_expression(*op, operand, span),

            AstKind::Binary { left, op, right } => self
                .check_binary_expression(*op, left, right, expected)
                .recover(&mut self.errors, fallback()),

            AstKind::Variable(name) => {
                let var = self.scopes.lookup(name.name);
                if let Some((ctx, resolved)) = var {
                    TypedExpr {
                        ty: ctx.type_info.clone(),
                        kind: ExprKind::GetVar(resolved, ctx.name.clone()),
                        span: name.span,
                    }
                } else if let Some(type_id) = self.res().get_type_id(name.name) {
                    TypedExpr {
                        ty: Type::Metatype(type_id, vec![].into()),
                        kind: ExprKind::GetVar(Global(0), name.name.into()),
                        span: name.span,
                    }
                } else {
                    if name.name == "self" {
                        self.report(TypeCheckerError::SelfOutsideOfImpl { span: name.span });
                        return fallback();
                    }

                    let visible_vars = self.scopes.visible_variable_names();
                    let suggestions = find_similar(name.name, visible_vars, 3);
                    self.report(TypeCheckerError::UndefinedVariable {
                        name: name.name.to_string(),
                        span: name.span,
                        suggestions,
                    });
                    fallback()
                }
            }

            AstKind::Grouping(inner) => self.check_expression(inner, expected),

            AstKind::StringInterp(parts) => {
                let typed_parts = parts
                    .iter()
                    .map(|part| match part {
                        StringPart::Literal { value, .. } => {
                            TypedStringPart::Literal(value.clone())
                        }
                        StringPart::Expr(e) => TypedStringPart::Expr(Box::new(
                            self.check_expression(e, &Type::Unknown),
                        )),
                    })
                    .collect();
                TypedExpr {
                    ty: Type::String,
                    kind: ExprKind::StringInterpolation { parts: typed_parts },
                    span,
                }
            }

            AstKind::Literal(literal) => {
                let ty = match literal {
                    Literal::Number(_) => Type::Number,
                    Literal::String(_) => Type::String,
                    Literal::Boolean(_) => Type::Boolean,
                    Literal::Void => Type::Void,
                    Literal::Nil => Type::Nil,
                };

                TypedExpr {
                    ty,
                    kind: ExprKind::Literal(literal.clone()),
                    span,
                }
            }

            AstKind::Assign {
                target: identifier,
                value,
            } => {
                let var_lookup = self.scopes.lookup_for_write(identifier.name);
                if let Some((ctx, resolved)) = var_lookup {
                    let is_closure = matches!(&resolved, ResolvedVar::Closure(_));
                    let is_reassignable = ctx.is_reassignable();
                    let ctx_name = ctx.name.to_string();
                    let ctx_span = ctx.def_span;
                    let ctx_kind = ctx.kind;
                    let original_type = ctx.original_type.clone();
                    let type_info = ctx.type_info.clone();

                    if is_closure {
                        self.report(TypeCheckerError::Binding(BindingError::Captured {
                            name: ctx_name.clone(),
                            span: identifier.span,
                            capture_origin: ctx_span,
                        }));
                    }

                    if !is_reassignable {
                        self.report(TypeCheckerError::Binding(BindingError::Immutable {
                            kind: ctx_kind,
                            name: ctx_name,
                            span: identifier.span,
                            definition_span: ctx_span,
                        }));
                    }

                    let (resolved, coerced_value) = if let Some((old_resolved, old_ty)) =
                        original_type
                    {
                        let coerced_value =
                            self.coerce_expression(value, &old_ty, MismatchContext::Generic, None);
                        if let Type::Optional(inner) = &old_ty
                            && inner.as_ref() == &coerced_value.ty
                        {
                            self.scopes.widen_type(
                                identifier.name,
                                coerced_value.ty.clone(),
                                old_resolved.clone(),
                            )
                        } else {
                            self.scopes.widen_type(
                                identifier.name,
                                old_ty.clone(),
                                old_resolved.clone(),
                            );
                        }
                        (old_resolved, coerced_value)
                    } else {
                        // This bug here took way too long.
                        let coerced_value = self.coerce_expression(
                            value,
                            &type_info,
                            MismatchContext::Generic,
                            None,
                        );
                        (resolved, coerced_value)
                    };

                    TypedExpr {
                        ty: coerced_value.ty.clone(),
                        kind: ExprKind::Assign {
                            target: resolved,
                            value: Box::new(coerced_value),
                        },
                        span,
                    }
                } else {
                    if identifier.name == "self" {
                        self.report(TypeCheckerError::SelfOutsideOfImpl {
                            span: identifier.span,
                        });
                        return fallback();
                    }
                    let visible_vars = self.scopes.visible_variable_names();
                    let suggestions = find_similar(identifier.name, visible_vars, 3);
                    self.report(TypeCheckerError::UndefinedVariable {
                        name: identifier.name.to_string(),
                        span: identifier.span,
                        suggestions,
                    });
                    fallback()
                }
            }

            AstKind::Coalesce { left, right, .. } => {
                self.check_coalesce(expr, left, right, expected)
            }

            AstKind::Logical { left, op, right } => {
                self.check_logical_expression(*op, left, right, span)
            }

            AstKind::Call { callee, safe, args } => self
                .check_call(expr, callee, args, *safe, expected)
                .recover(&mut self.errors, fallback()),

            AstKind::Field {
                object,
                safe,
                member,
            } => self
                .check_get(object, member, *safe)
                .recover(&mut self.errors, fallback()),

            AstKind::SetField {
                object,
                safe,
                member,
                value,
            } => self
                .check_set(object, member, value, *safe)
                .recover(&mut self.errors, fallback()),

            AstKind::ForceUnwrap { operand, bang } => {
                self.check_force_unwrap(expr, operand, *bang, expected)
            }
            AstKind::List(elements) => self.check_list(expr, elements, expected),
            AstKind::Map(entries) => self.check_map(expr, entries, expected),

            AstKind::Index {
                object,
                safe,
                index,
            } => self
                .check_get_index(expr, *safe, object, index)
                .recover(&mut self.errors, fallback()),

            AstKind::SetIndex {
                object,
                safe,
                index,
                value,
            } => self
                .check_set_index(expr, *safe, object, index, value)
                .recover(&mut self.errors, fallback()),

            // Already reported by the parser.
            AstKind::Error => fallback(),

            AstKind::Block { stmts, tail } => {
                self.check_block_expr(stmts, tail.as_deref(), expected, span)
            }

            AstKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.check_if_expr(
                condition,
                then_branch,
                else_branch.as_deref(),
                expected,
                span,
            ),

            AstKind::Match { scrutinee, arms } => self
                .check_match_expr(scrutinee, arms, span)
                .recover(&mut self.errors, fallback()),

            AstKind::Lambda { params, ret, body } => {
                self.check_lambda(params, ret.as_deref(), body, expected, span)
            }

            AstKind::Return(value) => {
                let Some((func_return_type, func_span)) = self.scopes.return_type() else {
                    self.report(TypeCheckerError::InvalidReturnOutsideFunction { span });
                    return fallback();
                };
                let typed = match value {
                    Some(value) => self.check_expression(value, &func_return_type),
                    // A bare `return` returns `void`, from the keyword.
                    None => TypedExpr {
                        ty: Type::Void,
                        kind: ExprKind::Literal(Literal::Void),
                        span,
                    },
                };
                let coerced = self.coerce_typed(
                    typed,
                    &func_return_type,
                    MismatchContext::Return,
                    Some(func_span),
                );
                TypedExpr {
                    ty: Type::Never,
                    kind: ExprKind::Return(Box::new(coerced)),
                    span,
                }
            }
        }
    }

    fn coerce(
        &mut self,
        mut provided: TypedExpr,
        expected: &Type,
    ) -> Result<TypedExpr, UnificationError> {
        let res = self.infer_ctx.unify_types(expected, &provided.ty);
        if res.is_ok() {
            provided.ty = self.infer_ctx.substitute(&provided.ty);
            return Ok(provided);
        }

        let (expected_type, was_optional) = if let Type::Optional(inner) = expected {
            (inner.as_ref(), true)
        } else {
            (expected, false)
        };

        if let (&Type::Interface(iface_id), Some(name)) = (expected_type, provided.ty.get_ty_id()) {
            return if let Some(idx) = self.sys.get_vtable_idx(name, iface_id) {
                let result_ty = if was_optional {
                    Type::Optional(Box::new(Type::Interface(iface_id)))
                } else {
                    Type::Interface(iface_id)
                };
                Ok(TypedExpr {
                    ty: result_ty,
                    span: provided.span,
                    kind: ExprKind::InterfaceUpcast {
                        expr: Box::new(provided),
                        vtable_idx: idx,
                    },
                })
            } else {
                Err(UnificationError {
                    kind: UnificationErrorKind::InterfaceNotImplemented {
                        interface: self.sys.get_interface(iface_id).name.clone(),
                    },
                    expected: expected_type.clone(),
                    found: provided.ty,
                })
            };
        }

        res.map(|_| unreachable!())
    }
    pub fn coerce_typed(
        &mut self,
        typed: TypedExpr,
        expected: &Type,
        context: MismatchContext,
        defined_at: Option<Span>,
    ) -> TypedExpr {
        let span = typed.span;
        let provided_ty = typed.ty.clone();
        match self.coerce(typed, expected) {
            Ok(coerced) => {
                if !coerced.ty.is_concrete() {
                    let uninferred = self.infer_ctx.uninferred_names(&coerced.ty, self.sys);
                    self.report(TypeCheckerError::Generic(GenericError::CannotInfer {
                        span,
                        uninferred_generics: uninferred,
                    }));
                    TypedExpr::new_blank(span)
                } else {
                    coerced
                }
            }
            Err(unif_err) => {
                self.report(TypeCheckerError::TypeMismatch {
                    mismatch: Mismatch::enriched(expected, &provided_ty, unif_err, &self.infer_ctx),
                    context,
                    primary_span: span,
                    defined_at,
                });
                TypedExpr::new_blank(span)
            }
        }
    }

    pub(crate) fn coerce_expression(
        &mut self,
        expr: &Expr<'src>,
        expected: &Type,
        context: MismatchContext,
        defined_at: Option<Span>,
    ) -> TypedExpr {
        let typed = self.check_expression(expr, expected);
        self.coerce_typed(typed, expected, context, defined_at)
    }
}
