mod access;
mod calls;
mod misc;
mod operators;
pub(crate) mod static_access;

use crate::compiler::analysis::ResolvedVar;
use crate::compiler::analysis::ResolvedVar::Global;
use crate::parser::ast::{Expr, Literal, StringPart};
use crate::scanner::Span;
use crate::scanner::TokenType;
use crate::typechecker::core::ast::{
    ExprKind, StmtKind, TypedExpr, TypedRefinements, TypedStmt, TypedStringPart,
};
use crate::typechecker::core::error::{
    BindingError, GenericError, Mismatch, MismatchContext, Recoverable, TypeCheckerError,
};
use crate::typechecker::core::types::Type;
use crate::typechecker::inference::{UnificationError, UnificationErrorKind};
use crate::typechecker::scope::guards::ScopeGuard;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::similarity::find_similar;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_expression(&mut self, expr: &Expr<'src>, expected: &Type) -> TypedExpr {
        let span = expr.span();
        let fallback = || TypedExpr::new_blank(span);
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
            } => self
                .check_binary_expression(operator, left, right, expected)
                .recover(&mut self.errors, fallback()),

            Expr::Variable { name } => {
                let var = self.scopes.lookup(name.lexeme);
                if let Some((ctx, resolved)) = var {
                    TypedExpr {
                        ty: ctx.type_info.clone(),
                        kind: ExprKind::GetVar(resolved, ctx.name.clone()),
                        span: name.span,
                    }
                } else if let Some(type_name) = self.res().get_owned_name(name.lexeme) {
                    TypedExpr {
                        ty: Type::Metatype(type_name.clone(), vec![].into()),
                        kind: ExprKind::GetVar(Global(0), type_name),
                        span: name.span,
                    }
                } else {
                    if name.lexeme == "self" {
                        self.report(TypeCheckerError::SelfOutsideOfImpl { span: name.span });
                        return fallback();
                    }

                    let visible_vars = self.scopes.visible_variable_names();
                    let suggestions = find_similar(name.lexeme, visible_vars, 3);
                    self.report(TypeCheckerError::UndefinedVariable {
                        name: name.lexeme.to_string(),
                        span: name.span,
                        suggestions,
                    });
                    fallback()
                }
            }

            Expr::Grouping { expression } => self.check_expression(expression, expected),

            Expr::StringInterp { parts, span } => {
                let typed_parts = parts
                    .iter()
                    .map(|part| match part {
                        StringPart::Literal(s) => TypedStringPart::Literal(s.clone()),
                        StringPart::Expr(e) => TypedStringPart::Expr(Box::new(
                            self.check_expression(e, &Type::Unknown),
                        )),
                    })
                    .collect();
                TypedExpr {
                    ty: Type::String,
                    kind: ExprKind::StringInterpolation { parts: typed_parts },
                    span: *span,
                }
            }

            Expr::Literal { literal, span } => {
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
                    span: *span,
                }
            }

            Expr::Assignment { identifier, value } => {
                let var_lookup = self.scopes.lookup_for_write(identifier.lexeme);
                if let Some((ctx, resolved)) = var_lookup {
                    let is_closure = matches!(&resolved, ResolvedVar::Closure(_));
                    let is_reassignable = ctx.is_reassignable();
                    let ctx_name = ctx.name.to_string();
                    let ctx_span = ctx.span;
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
                        span: expr.span(),
                    }
                } else {
                    if identifier.lexeme == "self" {
                        self.report(TypeCheckerError::SelfOutsideOfImpl {
                            span: identifier.span,
                        });
                        return fallback();
                    }
                    let visible_vars = self.scopes.visible_variable_names();
                    let suggestions = find_similar(identifier.lexeme, visible_vars, 3);
                    self.report(TypeCheckerError::UndefinedVariable {
                        name: identifier.lexeme.to_string(),
                        span: identifier.span,
                        suggestions,
                    });
                    fallback()
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
            } => self
                .check_call(expr, callee, arguments, safe, expected)
                .recover(&mut self.errors, fallback()),

            Expr::Get {
                object,
                field,
                safe,
            } => self
                .check_get(expr, object, field, safe)
                .recover(&mut self.errors, fallback()),

            Expr::Set {
                object,
                field,
                value,
                safe,
            } => self
                .check_set(object, field, value, safe)
                .recover(&mut self.errors, fallback()),

            Expr::ForceUnwrap {
                expression,
                operator,
            } => self.check_force_unwrap(expr, expression, operator, expected),
            Expr::List {
                elements,
                bracket_token,
            } => self.check_list(expr, elements, bracket_token, expected),
            Expr::Map { kv_pairs, .. } => self.check_map(expr, kv_pairs, expected),

            Expr::GetIndex {
                safe,
                object,
                index,
            } => self
                .check_get_index(expr, safe, object, index)
                .recover(&mut self.errors, fallback()),

            Expr::SetIndex {
                safe,
                object,
                index,
                value,
            } => self
                .check_set_index(expr, safe, object, index, value)
                .recover(&mut self.errors, fallback()),

            Expr::Error => {
                // Already reported by parser
                TypedExpr::new_blank(expr.span())
            }

            Expr::Block { body, tail, .. } => {
                self.check_block_expr(body, tail.as_deref(), expected, span)
            }

            Expr::IfExpr {
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

            Expr::MatchExpr { value, arms } => self
                .check_match_expr(value, arms)
                .recover(&mut self.errors, fallback()),

            Expr::Lambda {
                params,
                body,
                pipe_token,
            } => self.check_lambda(params, body, expected, pipe_token.span),
        }
    }

    // ── Block expression ──────────────────────────────────────────────────────

    pub(crate) fn check_block_expr(
        &mut self,
        body: &[crate::parser::ast::Stmt<'src>],
        tail: Option<&Expr<'src>>,
        expected: &Type,
        span: Span,
    ) -> TypedExpr {
        let mut scope = ScopeGuard::new(self, ScopeKind::Block);
        let typed_stmts = body.iter().map(|s| scope.check_stmt(s)).collect::<Vec<_>>();

        let typed_tail = match tail {
            Some(tail_expr) => scope.check_expression(tail_expr, expected),
            None => TypedExpr {
                ty: Type::Void,
                kind: ExprKind::Literal(Literal::Void),
                span,
            },
        };
        let ty = typed_tail.ty.clone();
        drop(scope);

        TypedExpr {
            ty,
            kind: ExprKind::Block {
                body: typed_stmts,
                tail: Box::new(typed_tail),
            },
            span,
        }
    }

    // ── If expression ─────────────────────────────────────────────────────────

    fn check_if_expr(
        &mut self,
        condition: &Expr<'src>,
        then_branch: &Expr<'src>,
        else_branch: Option<&Expr<'src>>,
        expected: &Type,
        span: Span,
    ) -> TypedExpr {
        let cond_typed =
            self.coerce_expression(condition, &Type::Boolean, MismatchContext::Condition, None);

        let refinements = self.analyze_condition(&cond_typed);
        let mut typed_refinements = TypedRefinements {
            true_path: vec![],
            else_path: vec![],
            after_path: vec![],
        };

        let then_typed = {
            let mut scope = ScopeGuard::new(self, ScopeKind::Block);
            for (name, ty) in refinements.true_path.iter() {
                if let Some(case) = scope.scopes.refine(name, ty.clone()) {
                    typed_refinements.true_path.push(case);
                }
            }
            scope.check_expression(then_branch, expected)
        };
        let then_ty = then_typed.ty.clone();

        let else_typed = else_branch.map(|eb| {
            let typed = {
                let mut scope = ScopeGuard::new(self, ScopeKind::Block);
                for (name, ty) in refinements.false_path.iter() {
                    if let Some(case) = scope.scopes.refine(name, ty.clone()) {
                        typed_refinements.else_path.push(case);
                    }
                }
                scope.check_expression(eb, &then_ty)
                // scope dropped here
            };
            // Coerce the else branch to the then-branch type for consistency.
            self.coerce_typed(typed, &then_ty, MismatchContext::Generic, None)
        });

        let ty = else_typed
            .as_ref()
            .map(|e| e.ty.clone())
            .unwrap_or(Type::Void);

        if else_typed.is_none() && then_typed.ty != ty {
            panic!(
                "Expected else branch to be of type {}, got {}",
                ty, then_typed.ty
            );
            // TODO add error reporting here
        }

        TypedExpr {
            ty,
            kind: ExprKind::IfExpr {
                condition: Box::new(cond_typed),
                then_branch: Box::new(then_typed),
                else_branch: else_typed.map(Box::new),
                typed_refinements: Box::new(typed_refinements),
            },
            span,
        }
    }

    // ── Lambda ────────────────────────────────────────────────────────────────

    pub(crate) fn check_lambda(
        &mut self,
        params: &[(
            crate::scanner::Token<'src>,
            crate::parser::ast::TypeAst<'src>,
        )],
        body: &Expr<'src>,
        expected: &Type,
        span: Span,
    ) -> TypedExpr {
        use crate::typechecker::core::types::{FunctionType, Symbol};
        use crate::typechecker::scope::variables::Declaration;
        use std::rc::Rc;

        // Extract expected parameter types / return type from context if available.
        let expected_fn = if let Type::Function(ft) = expected {
            Some(ft.clone())
        } else {
            None
        };

        // Resolve each parameter type: use annotation if present, else try hint, else infer.
        let mut resolved_params: Vec<(Symbol, Type)> = Vec::with_capacity(params.len());
        for (i, (name, type_ast)) in params.iter().enumerate() {
            use crate::parser::ast::TypeAst;
            let ty = if !matches!(type_ast, TypeAst::Infer) {
                self.res()
                    .resolve(type_ast)
                    .recover(&mut self.errors, Type::Error)
            } else if let Some(hint) = expected_fn.as_ref().and_then(|ft| ft.params.get(i)) {
                hint.1.clone()
            } else {
                self.infer_ctx.new_type_var()
            };
            resolved_params.push((name.lexeme.into(), ty));
        }

        let return_type = expected_fn
            .as_ref()
            .map(|ft| ft.return_type.clone())
            .unwrap_or_else(|| self.infer_ctx.new_type_var());

        // Enter function scope and declare parameters.
        let mut guard = ScopeGuard::new_function(self, return_type.clone(), span);
        let func_type = Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            params: resolved_params.clone(),
            return_type: return_type.clone(),
            type_params: vec![],
        }));
        let decl = Declaration::function(
            "__lambda__".into(),
            func_type,
            Span::new(span.start, span.start, span.line),
        );
        guard.scopes.declare(decl).expect("Lambda must have a name");
        for (name, ty) in &resolved_params {
            let decl = Declaration::parameter(name.clone(), ty.clone(), span);
            guard.scopes.declare(decl).ok_or_report(&mut guard.errors);
        }

        // Type-check the body expression as the return value.
        let typed_body =
            guard.coerce_expression(body, &return_type, MismatchContext::Return, Some(span));
        let actual_return = typed_body.ty.clone();
        let reserved = guard.scopes.max_index();
        let old_closures = guard.scopes.get_closures();
        drop(guard);

        // Resolve captures from the outer scope.
        let mut captures = vec![];
        for clos_var in old_closures {
            let (_, var_ctx) = self
                .scopes
                .lookup(clos_var.as_ref())
                .expect("Captured variable must exist in outer scope.");
            captures.push(var_ctx);
        }

        // Substitute any solved inference variables.
        let final_return = self.infer_ctx.substitute(&actual_return);
        let final_params: Vec<_> = resolved_params
            .iter()
            .map(|(n, t)| (n.clone(), self.infer_ctx.substitute(t)))
            .collect();
        let final_func_type = Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            params: final_params,
            return_type: final_return.clone(),
            type_params: vec![],
        }));

        // Wrap the expression body in a Return statement for the compiler.
        let body_stmt = TypedStmt {
            span: typed_body.span,
            type_info: final_return,
            kind: StmtKind::Return(typed_body),
        };

        TypedExpr {
            ty: final_func_type,
            kind: ExprKind::Function {
                reserved: reserved as u8,
                signature: span,
                body: Box::new(body_stmt),
                captures: Box::from(captures),
            },
            span,
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

        if let (Type::Interface(iface_name), Some(name)) = (expected_type, provided.ty.get_name()) {
            return if let Some(idx) = self.sys.get_vtable_idx(name, iface_name.clone()) {
                let result_ty = if was_optional {
                    Type::Optional(Box::new(Type::Interface(iface_name.clone())))
                } else {
                    Type::Interface(iface_name.clone())
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
                        interface: iface_name.clone(),
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
                    let uninferred = self.infer_ctx.uninferred_names(&coerced.ty);
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
