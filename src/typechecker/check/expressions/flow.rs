use crate::parser::ast::{Expr, Literal, Stmt};
use crate::scanner::Span;
use crate::typechecker::core::ast::{ExprKind, TypedExpr, TypedRefinements};
use crate::typechecker::core::error::MismatchContext;
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::guards::ScopeGuard;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_block_expr(
        &mut self,
        body: &[Stmt<'src>],
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
        let mut expr = TypedExpr {
            ty: typed_tail.ty.clone(),
            kind: ExprKind::Block {
                body: typed_stmts,
                tail: Box::new(typed_tail),
            },
            span,
        };
        if scope.expr_diverges(&expr).is_divergent() {
            expr.ty = Type::Never;
        }
        expr
    }
    pub(crate) fn check_if_expr(
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

        // When the then-branch diverges (Never), fall back to `expected` so the else branch
        // is not incorrectly required to produce `Never` as well.
        let else_target = if then_ty == Type::Never {
            expected
        } else {
            &then_ty
        };

        let else_typed = else_branch.map(|eb| {
            let typed = {
                let mut scope = ScopeGuard::new(self, ScopeKind::Block);
                for (name, ty) in refinements.false_path.iter() {
                    if let Some(case) = scope.scopes.refine(name, ty.clone()) {
                        typed_refinements.else_path.push(case);
                    }
                }
                scope.check_expression(eb, else_target)
            };
            self.coerce_typed(typed, else_target, MismatchContext::Generic, None)
        });

        // Compute the if-expression type, accounting for Never branches.
        // - If one branch is Never (always returns/diverges), the type is the other branch's type.
        // - Both Never → Never.
        // - No else → Void (condition may be false, yielding nothing).
        let ty = match (
            then_typed.ty.clone(),
            else_typed.as_ref().map(|e| e.ty.clone()),
        ) {
            (_, Some(Type::Never)) => then_typed.ty.clone(),
            (Type::Never, Some(else_ty)) => else_ty,
            (_, Some(else_ty)) => else_ty,
            (Type::Void | Type::Never, None) => Type::Void,
            (_, None) => {
                todo!("Error for missing else branch")
            }
        };

        // Guard logic
        if then_typed.ty == Type::Never {
            for (name, ty) in refinements.false_path.iter() {
                if let Some(case) = self.scopes.refine(name, ty.clone()) {
                    typed_refinements.after_path.push(case);
                }
            }
        }
        if let Some(else_typed) = else_typed.as_ref()
            && else_typed.ty == Type::Never
        {
            for (name, ty) in refinements.true_path.iter() {
                if let Some(case) = self.scopes.refine(name, ty.clone()) {
                    typed_refinements.after_path.push(case);
                }
            }
        }

        // TODO (Phase 4): report a proper error when the if expression is used as a value
        // and the then-branch type doesn't match (e.g. if without else in a non-void context).

        TypedExpr {
            ty,
            kind: ExprKind::If {
                condition: Box::new(cond_typed),
                then_branch: Box::new(then_typed),
                else_branch: else_typed.map(Box::new),
                typed_refinements: Box::new(typed_refinements),
            },
            span,
        }
    }
}
