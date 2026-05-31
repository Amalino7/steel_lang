use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Expr, FunctionSig, Stmt, TypeAst};
use crate::scanner::{Span, Token};
use crate::typechecker::core::ast::{ExprKind, FunctionBody, FunctionDecl, TypedExpr};
use crate::typechecker::core::error::{MismatchContext, Recoverable, TypeCheckerError};
use crate::typechecker::core::types::{FunctionType, Type};
use crate::typechecker::scope::guards::{ScopeGuard, TypeScopeGuard};
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::{Symbol, TypeChecker};
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    fn resolve_captures(&mut self, closures: Vec<Symbol>) -> Box<[ResolvedVar]> {
        closures
            .iter()
            .map(|name| {
                self.scopes
                    .lookup(name.as_ref())
                    .expect("Captured variable must exist in outer scope.")
                    .1
            })
            .collect()
    }

    /// Shared helper that opens a function scope, declares slot 0 and params,
    /// invokes `make_body` to produce the body, then collects captures.
    fn build_function_decl<F>(
        &mut self,
        self_name: Symbol,
        self_type: Type,
        // Each entry is (name, type, span). Use Span::default() to skip drain_unused.
        params: &[(Symbol, Type, Span)],
        return_type: Type,
        span: Span,
        make_body: F,
    ) -> FunctionDecl
    where
        F: for<'g> FnOnce(&mut ScopeGuard<'g, 'src>) -> FunctionBody,
    {
        let mut guard = ScopeGuard::new_function(self, return_type, span);

        // Slot 0 is always the function itself (calling convention).
        let self_decl = Declaration::function(self_name, self_type, span);
        guard
            .scopes
            .declare(self_decl)
            .expect("slot 0 must be declarable in a fresh scope");

        for (name, ty, param_span) in params {
            let decl = Declaration::parameter(name.clone(), ty.clone(), *param_span);
            guard.scopes.declare(decl).ok_or_report(&mut guard.errors);
        }

        let body = make_body(&mut guard);
        let reserved = guard.scopes.max_index();
        let old_closures = guard.scopes.get_closures();
        drop(guard);

        let captures = self.resolve_captures(old_closures);

        FunctionDecl {
            reserved: reserved as u8,
            body,
            captures,
        }
    }

    pub(crate) fn check_function(
        &mut self,
        name: &Token<'src>,
        sig: &FunctionSig,
        body: &Stmt<'src>,
        generics: &[Token<'src>],
    ) -> (FunctionDecl, Type, Span) {
        let mut ty_guard = TypeScopeGuard::new_function(self, generics);
        let sig_result = ty_guard.res().resolve_generic_func(sig);
        let sig_ok = sig_result.is_ok();
        // TODO not like that
        let func = sig_result.recover(
            &mut ty_guard.errors,
            Rc::new(FunctionType {
                is_vararg: false,
                params: vec![],
                return_type: Type::Error,
                type_params: vec![],
            }),
        );
        let func_type = Type::Function(func.clone());

        // Use Span::default() when the signature failed so drain_unused skips these
        // params — they're declared only for body error-recovery, not real bindings.
        let params: Vec<(Symbol, Type, Span)> = sig
            .params
            .iter()
            .enumerate()
            .map(|(i, (param, _))| {
                let ty = func
                    .params
                    .get(i)
                    .map(|(_, t)| t.clone())
                    .unwrap_or(Type::Error);
                let param_span = if sig_ok { param.span } else { Span::default() };
                (param.lexeme.into(), ty, param_span)
            })
            .collect();

        let fn_decl = ty_guard.build_function_decl(
            name.lexeme.into(),
            func_type.clone(),
            &params,
            func.return_type.clone(),
            name.span,
            |guard| FunctionBody::Block(Box::new(guard.check_stmt(body))),
        );
        drop(ty_guard);

        let body_span = fn_decl.body.span();
        let function_span = name.span.merge(body_span);

        let return_type = match &func_type {
            Type::Function(f) => f.return_type.clone(),
            _ => unreachable!(),
        };
        if return_type != Type::Void
            && return_type != Type::Error
            && let FunctionBody::Block(block) = &fn_decl.body
            && !self.stmt_diverges(block).exits_function()
        {
            self.report(TypeCheckerError::MissingReturnStatement {
                fn_span: name.span.merge(Span {
                    end: body_span.start,
                    ..body_span
                }),
                fn_name: name.lexeme.to_string(),
                span: Span {
                    start: body_span.end,
                    ..body_span
                },
            });
        }

        (fn_decl, func_type, function_span)
    }

    pub(crate) fn check_lambda(
        &mut self,
        params: &[(Token<'src>, TypeAst<'src>)],
        body: &Expr<'src>,
        return_type_ann: Option<&TypeAst<'src>>,
        expected: &Type,
        span: Span,
    ) -> TypedExpr {
        let expected_fn = if let Type::Function(ft) = expected {
            Some(ft.clone())
        } else {
            None
        };

        let mut resolved_params: Vec<(Symbol, Type)> = Vec::with_capacity(params.len());
        for (i, (token, type_ast)) in params.iter().enumerate() {
            let ty = if !matches!(type_ast, TypeAst::Infer) {
                self.res()
                    .resolve(type_ast)
                    .recover(&mut self.errors, Type::Error)
            } else if let Some(hint) = expected_fn.as_ref().and_then(|ft| ft.params.get(i)) {
                hint.1.clone()
            } else {
                self.infer_ctx.new_type_var()
            };
            resolved_params.push((token.lexeme.into(), ty));
        }

        let return_type = if let Some(ann) = return_type_ann {
            self.res()
                .resolve(ann)
                .recover(&mut self.errors, Type::Error)
        } else {
            expected_fn
                .as_ref()
                .map(|ft| ft.return_type.clone())
                .unwrap_or_else(|| self.infer_ctx.new_type_var())
        };

        let self_type = Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            params: resolved_params.clone(),
            return_type: return_type.clone(),
            type_params: vec![],
        }));
        let params_with_spans: Vec<(Symbol, Type, Span)> = resolved_params
            .iter()
            .map(|(n, t)| (n.clone(), t.clone(), span))
            .collect();

        let return_type_for_body = return_type.clone();
        let fn_decl = self.build_function_decl(
            "__lambda__".into(),
            self_type,
            &params_with_spans,
            return_type.clone(),
            span,
            |guard| {
                let typed_body = guard.coerce_expression(
                    body,
                    &return_type_for_body,
                    MismatchContext::Return,
                    Some(span),
                );
                FunctionBody::Expr(Box::new(typed_body))
            },
        );

        let actual_return = if let FunctionBody::Expr(e) = &fn_decl.body {
            e.ty.clone()
        } else {
            unreachable!("lambda body is always FunctionBody::Expr")
        };

        // When the body diverges (Never), the return type was determined by `return`
        // statements inside the lambda and is stored in `return_type`.
        let final_return = {
            let sub_body = self.infer_ctx.substitute(&actual_return);
            if sub_body == Type::Never {
                self.infer_ctx.substitute(&return_type)
            } else {
                sub_body
            }
        };
        let final_params: Vec<_> = resolved_params
            .iter()
            .map(|(n, t)| (n.clone(), self.infer_ctx.substitute(t)))
            .collect();
        let final_func_type = Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            params: final_params,
            return_type: final_return,
            type_params: vec![],
        }));

        TypedExpr {
            ty: final_func_type,
            kind: ExprKind::Lambda(fn_decl),
            span,
        }
    }
}
