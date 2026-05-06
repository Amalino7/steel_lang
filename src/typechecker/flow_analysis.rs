use crate::scanner::Span;
use crate::typechecker::core::ast::{
    ExprKind, ExprMatchCase, MatchCase, StmtKind, TypedExpr, TypedStmt, TypedStringPart,
};
use crate::typechecker::core::error::TypeCheckerWarning;
use crate::typechecker::core::types::Type;
use crate::typechecker::TypeChecker;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Diverges {
    /// Control continues normally past this point.
    No,
    /// Exits the immediately enclosing loop (`break` / `continue`).
    #[allow(dead_code)] // TODO: remove when break and continue are implemented.
    Loop,
    /// Exits the enclosing function (`return`) or diverges entirely
    /// like in `panic()`.
    Always,
}

impl Diverges {
    /// Used for checking control flow
    pub fn is_divergent(self) -> bool {
        self != Diverges::No
    }

    /// Checks if the flow is guaranteed to return.
    pub fn exits_function(self) -> bool {
        self == Diverges::Always
    }

    /// Combine two branches (if/else arms, match arms).
    pub fn meet(self, other: Diverges) -> Diverges {
        self.min(other)
    }
}

fn stmt_case_body(case: &MatchCase) -> &TypedStmt {
    match case {
        MatchCase::Variable { body, .. } | MatchCase::Named { body, .. } => body,
    }
}

fn expr_case_body(case: &ExprMatchCase) -> &TypedExpr {
    match case {
        ExprMatchCase::Variable { body, .. } | ExprMatchCase::Named { body, .. } => body,
    }
}

/// Check for diverging code
impl<'src> TypeChecker<'src> {
    pub(crate) fn stmt_diverges(&self, stmt: &TypedStmt) -> Diverges {
        match &stmt.kind {
            StmtKind::Expression(expr) => self.expr_diverges(expr),

            StmtKind::Let { value, .. } => self.expr_diverges(value),

            StmtKind::Block { body, .. } => {
                for s in body {
                    let d = self.stmt_diverges(s);
                    if d.is_divergent() {
                        return d;
                    }
                }
                Diverges::No
            }

            StmtKind::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cd = self.expr_diverges(condition);
                if cd.is_divergent() {
                    return cd;
                }
                match else_branch {
                    Some(eb) => self.stmt_diverges(then_branch).meet(self.stmt_diverges(eb)),
                    None => Diverges::No,
                }
            }

            StmtKind::Match { value, cases } => {
                let vd = self.expr_diverges(value);
                if vd.is_divergent() {
                    return vd;
                }
                if cases.is_empty() {
                    return Diverges::No;
                }
                cases
                    .iter()
                    .map(|c| self.stmt_diverges(stmt_case_body(c)))
                    .fold(Diverges::Always, Diverges::meet)
            }

            StmtKind::While { condition, .. } => self.expr_diverges(condition),

            StmtKind::Blank
            | StmtKind::Function { .. }
            | StmtKind::Global { .. }
            | StmtKind::StructDecl { .. }
            | StmtKind::Impl { .. }
            | StmtKind::EnumDecl { .. }
            | StmtKind::ExternFunction { .. } => Diverges::No,
        }
    }

    pub(crate) fn expr_diverges(&self, expr: &TypedExpr) -> Diverges {
        match &expr.kind {
            ExprKind::Return(_) => Diverges::Always,
            ExprKind::Call {
                callee, arguments, ..
            } => {
                // A never-typed call (panic(),  etc.) always diverges.
                if expr.ty == Type::Never {
                    return Diverges::Always;
                }
                let d = self.expr_diverges(callee);
                if d.is_divergent() {
                    return d;
                }
                for arg in arguments {
                    let d = self.expr_diverges(arg);
                    if d.is_divergent() {
                        return d;
                    }
                }
                Diverges::No
            }

            ExprKind::Block { body, tail, .. } => {
                for s in body {
                    let d = self.stmt_diverges(s);
                    if d.is_divergent() {
                        return d;
                    }
                }
                self.expr_diverges(tail)
            }

            ExprKind::IfExpr {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cd = self.expr_diverges(condition);
                if cd.is_divergent() {
                    return cd;
                }
                match else_branch {
                    Some(eb) => self.expr_diverges(then_branch).meet(self.expr_diverges(eb)),
                    // An if-without-else can always fall through.
                    None => Diverges::No,
                }
            }

            ExprKind::MatchExpr { value, cases } => {
                let vd = self.expr_diverges(value);
                if vd.is_divergent() {
                    return vd;
                }
                if cases.is_empty() {
                    return Diverges::No;
                }
                cases
                    .iter()
                    .map(|c| self.expr_diverges(expr_case_body(c)))
                    .fold(Diverges::Always, Diverges::meet)
            }

            ExprKind::Binary { left, right, .. } => {
                let d = self.expr_diverges(left);
                if d.is_divergent() {
                    return d;
                }
                self.expr_diverges(right)
            }
            ExprKind::Unary { operand, .. } | ExprKind::Try { operand } => {
                self.expr_diverges(operand)
            }

            // Only the left operand is always evaluated (short-circuit).
            ExprKind::Logical { left, .. } => self.expr_diverges(left),

            // Single subexpression (field names differ, so each gets its own arm).
            ExprKind::GetField { object, .. }
            | ExprKind::InterfaceMethodGet { object, .. }
            | ExprKind::MethodGet { object, .. } => self.expr_diverges(object),

            ExprKind::InterfaceUpcast { expr: sub, .. }
            | ExprKind::Is { target: sub, .. }
            | ExprKind::EnumInit { value: sub, .. } => self.expr_diverges(sub),

            ExprKind::Assign { value, .. } => self.expr_diverges(value),

            // Two subexpressions evaluated left-to-right.
            ExprKind::SetField { object, value, .. }
            | ExprKind::GetIndex {
                object,
                index: value,
                ..
            }
            | ExprKind::MapGet {
                object, key: value, ..
            } => {
                let d = self.expr_diverges(object);
                if d.is_divergent() {
                    return d;
                }
                self.expr_diverges(value)
            }

            // Three subexpressions evaluated left-to-right.
            ExprKind::SetIndex {
                object,
                index,
                value,
                ..
            }
            | ExprKind::MapSet {
                object,
                key: index,
                value,
                ..
            } => {
                let d = self.expr_diverges(object);
                if d.is_divergent() {
                    return d;
                }
                let d = self.expr_diverges(index);
                if d.is_divergent() {
                    return d;
                }
                self.expr_diverges(value)
            }

            // Variadic subexpressions.
            ExprKind::StructInit { args, .. } => {
                for arg in args {
                    let d = self.expr_diverges(arg);
                    if d.is_divergent() {
                        return d;
                    }
                }
                Diverges::No
            }

            ExprKind::Tuple { elements } | ExprKind::List { elements } => {
                for elem in elements {
                    let d = self.expr_diverges(elem);
                    if d.is_divergent() {
                        return d;
                    }
                }
                Diverges::No
            }

            ExprKind::Map { pairs } => {
                for (k, v) in pairs {
                    let d = self.expr_diverges(k);
                    if d.is_divergent() {
                        return d;
                    }
                    let d = self.expr_diverges(v);
                    if d.is_divergent() {
                        return d;
                    }
                }
                Diverges::No
            }

            ExprKind::StringInterpolation { parts } => {
                for part in parts {
                    if let TypedStringPart::Expr(e) = part {
                        let d = self.expr_diverges(e);
                        if d.is_divergent() {
                            return d;
                        }
                    }
                }
                Diverges::No
            }

            // Lambda body is its own function — defining it never diverges.
            ExprKind::Function { .. } => Diverges::No,

            // Leaf nodes.
            ExprKind::Literal(_) | ExprKind::GetVar(_, _) | ExprKind::NoOp => Diverges::No,
        }
    }
}

/// Check for unreachable code.
impl<'src> TypeChecker<'src> {
    pub(crate) fn check_unreachable(&mut self, stmts: &[TypedStmt]) {
        for s in stmts {
            self.walk_stmt(s);
        }
    }

    fn walk_stmt(&mut self, stmt: &TypedStmt) {
        match &stmt.kind {
            StmtKind::Block { body, .. } => {
                self.walk_block_stmts(body);
            }

            StmtKind::Global { stmts, .. } => {
                self.walk_block_stmts(stmts);
            }

            StmtKind::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_expr(condition);
                self.walk_stmt(then_branch);
                if let Some(eb) = else_branch {
                    self.walk_stmt(eb);
                }
            }

            StmtKind::Match { value, cases } => {
                self.walk_expr(value);
                for case in cases {
                    self.walk_stmt(stmt_case_body(case));
                }
            }

            StmtKind::While {
                condition, body, ..
            } => {
                self.walk_expr(condition);
                self.walk_stmt(body);
            }

            StmtKind::Function { function_decl, .. } => {
                let ExprKind::Function { body, .. } = &function_decl.kind else {
                    return;
                };
                self.walk_stmt(body);
            }

            StmtKind::Impl { methods, .. } => {
                for method in methods {
                    self.walk_stmt(method);
                }
            }

            StmtKind::Expression(expr) => {
                self.walk_expr(expr);
            }

            StmtKind::Let { value, .. } => {
                self.walk_expr(value);
            }

            StmtKind::Blank
            | StmtKind::StructDecl { .. }
            | StmtKind::EnumDecl { .. }
            | StmtKind::ExternFunction { .. } => {}
        }
    }

    fn walk_block_stmts(&mut self, stmts: &[TypedStmt]) {
        let mut diverged = false;
        let mut error_span: Option<Span> = None;
        for s in stmts {
            if diverged {
                error_span = Some(s.span);
                // Skip recursion to suppress cascading warnings inside unreachable code.
                break;
            }

            self.walk_stmt(s);
            diverged = self.stmt_diverges(s).is_divergent();
        }

        if let Some(initial_span) = error_span {
            let span = initial_span.merge(stmts[stmts.len() - 1].span);
            self.warnings
                .push(TypeCheckerWarning::UnreachableCode { span });
        }
    }

    fn walk_expr(&mut self, expr: &TypedExpr) {
        match &expr.kind {
            ExprKind::Block { body, tail, .. } => {
                let mut diverged = false;
                let mut error_span: Option<Span> = None;
                for s in body {
                    if diverged {
                        error_span = Some(s.span);
                        break;
                    }

                    self.walk_stmt(s);
                    diverged = self.stmt_diverges(s).is_divergent();
                }

                // Warn on an unreachable tail expression — but skip the
                // synthetic void tail the compiler inserts (ty == Void with
                // no user-written source).
                if diverged && error_span.is_none() && tail.ty != Type::Void {
                    error_span = error_span.or(Some(tail.span));
                } else if !diverged {
                    self.walk_expr(tail);
                }

                if let Some(initial_span) = error_span {
                    let span = initial_span.merge(tail.span);

                    self.warnings
                        .push(TypeCheckerWarning::UnreachableCode { span });
                }
            }

            ExprKind::IfExpr {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_expr(condition);
                self.walk_expr(then_branch);
                if let Some(eb) = else_branch {
                    self.walk_expr(eb);
                }
            }

            ExprKind::MatchExpr { value, cases } => {
                self.walk_expr(value);
                for case in cases {
                    self.walk_expr(expr_case_body(case));
                }
            }

            // Lambda: walk its body for unreachable code.
            ExprKind::Function { body, .. } => {
                self.walk_stmt(body);
            }

            ExprKind::Return(inner) => {
                self.walk_expr(inner);
            }

            ExprKind::Call {
                callee, arguments, ..
            } => {
                self.walk_expr(callee);
                for arg in arguments {
                    self.walk_expr(arg);
                }
            }

            ExprKind::Binary { left, right, .. } | ExprKind::Logical { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }

            ExprKind::Unary { operand, .. } | ExprKind::Try { operand } => {
                self.walk_expr(operand);
            }

            ExprKind::GetField { object, .. }
            | ExprKind::InterfaceMethodGet { object, .. }
            | ExprKind::MethodGet { object, .. } => {
                self.walk_expr(object);
            }

            ExprKind::InterfaceUpcast { expr: sub, .. }
            | ExprKind::Is { target: sub, .. }
            | ExprKind::EnumInit { value: sub, .. } => {
                self.walk_expr(sub);
            }

            ExprKind::Assign { value, .. } => {
                self.walk_expr(value);
            }

            ExprKind::SetField { object, value, .. }
            | ExprKind::GetIndex {
                object,
                index: value,
                ..
            }
            | ExprKind::MapGet {
                object, key: value, ..
            } => {
                self.walk_expr(object);
                self.walk_expr(value);
            }

            ExprKind::SetIndex {
                object,
                index,
                value,
                ..
            }
            | ExprKind::MapSet {
                object,
                key: index,
                value,
                ..
            } => {
                self.walk_expr(object);
                self.walk_expr(index);
                self.walk_expr(value);
            }

            ExprKind::StructInit { args, .. } => {
                for arg in args {
                    self.walk_expr(arg);
                }
            }

            ExprKind::Tuple { elements } | ExprKind::List { elements } => {
                for elem in elements {
                    self.walk_expr(elem);
                }
            }

            ExprKind::Map { pairs } => {
                for (k, v) in pairs {
                    self.walk_expr(k);
                    self.walk_expr(v);
                }
            }

            ExprKind::StringInterpolation { parts } => {
                for part in parts {
                    if let TypedStringPart::Expr(e) = part {
                        self.walk_expr(e);
                    }
                }
            }

            // Leaf nodes — nothing to recurse into.
            ExprKind::Literal(_) | ExprKind::GetVar(_, _) | ExprKind::NoOp => {}
        }
    }
}
