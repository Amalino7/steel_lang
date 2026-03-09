use crate::parser::ast::{Stmt, TypeAst};
use crate::scanner::Token;
use crate::typechecker::core::ast::{StmtKind, TypedExpr, TypedRefinements, TypedStmt};
use crate::typechecker::core::error::{MismatchContext, Recoverable, TypeCheckerError};
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::guards::ScopeGuard;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_stmt(&mut self, stmt: &Stmt<'src>) -> TypedStmt {
        match stmt {
            Stmt::Expression(expr) => {
                let typed_expr = self.check_expression(expr, &Type::Unknown);

                TypedStmt {
                    kind: StmtKind::Expression(typed_expr),
                    span: stmt.span(),
                    type_info: Type::Void,
                }
            }
            Stmt::Let {
                binding,
                value,
                type_info,
            } => {
                let declared_type = self
                    .res()
                    .resolve(type_info)
                    .recover(&mut self.errors, Type::Error);

                let type_annotation_span = match type_info {
                    TypeAst::Infer => None,
                    other => Some(other.span()),
                };
                let coerced_value = self.coerce_expression(
                    value,
                    &declared_type,
                    MismatchContext::Let,
                    type_annotation_span,
                );

                let final_type = if declared_type == Type::Error || declared_type == Type::Unknown {
                    coerced_value.ty.clone()
                } else {
                    declared_type
                };

                let typed_binding = self
                    .check_binding(binding, &final_type, false)
                    .ok_or_report(&mut self.errors);

                let kind = if let Some(tb) = typed_binding {
                    StmtKind::Let {
                        binding: tb,
                        value: coerced_value,
                    }
                } else {
                    StmtKind::Blank
                };

                TypedStmt {
                    kind,
                    type_info: Type::Void,
                    span: binding.span(),
                }
            }
            impl_block @ Stmt::Impl {
                interfaces,
                name,
                methods,
                generics,
            } => {
                if self.non_global("impl", &name.0) {
                    return TypedStmt::new_blank(stmt.span());
                }
                self.define_impl(impl_block, interfaces, name, methods, generics)
            }
            Stmt::Block { body, brace_token } => {
                let mut scope = ScopeGuard::new(self, ScopeKind::Block);
                let stmts = body
                    .iter()
                    .map(|stmt| scope.check_stmt(stmt))
                    .collect::<Vec<_>>();
                TypedStmt {
                    kind: StmtKind::Block {
                        body: stmts,
                        reserved: 0,
                    },
                    type_info: Type::Void,
                    span: stmt.span().merge(brace_token.span),
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_typed = self.check_expression(condition, &Type::Boolean);
                let cond_typed =
                    self.coerce_typed(cond_typed, &Type::Boolean, MismatchContext::Condition, None);

                let refinements = self.analyze_condition(&cond_typed);
                let mut typed_refinements = TypedRefinements {
                    true_path: vec![],
                    else_path: vec![],
                    after_path: vec![],
                };
                let mut scope = ScopeGuard::new(self, ScopeKind::Block);
                for (name, ty) in refinements.true_path.iter() {
                    if let Some(case) = scope.scopes.refine(name, ty.clone()) {
                        typed_refinements.true_path.push(case)
                    }
                }
                let then_branch_typed = scope.check_stmt(then_branch);
                drop(scope);

                let else_branch_typed = if let Some(else_branch) = else_branch {
                    let mut scope = ScopeGuard::new(self, ScopeKind::Block);
                    for (name, ty) in refinements.false_path.iter() {
                        if let Some(case) = scope.scopes.refine(name, ty.clone()) {
                            typed_refinements.else_path.push(case)
                        }
                    }
                    let stmt = scope.check_stmt(else_branch);
                    Some(Box::new(stmt))
                } else {
                    None
                };

                // Guard logic
                if self.stmt_returns(&then_branch_typed).unwrap_or(false) {
                    for (name, ty) in refinements.false_path {
                        if let Some(case) = self.scopes.refine(&name, ty.clone()) {
                            typed_refinements.after_path.push(case)
                        }
                    }
                }
                if let Some(else_branch_typed) = &else_branch_typed
                    && self.stmt_returns(else_branch_typed).unwrap_or(false)
                {
                    for (name, ty) in refinements.true_path {
                        if let Some(case) = self.scopes.refine(&name, ty.clone()) {
                            typed_refinements.after_path.push(case)
                        }
                    }
                }

                TypedStmt {
                    kind: StmtKind::If {
                        condition: cond_typed,
                        then_branch: Box::new(then_branch_typed),
                        else_branch: else_branch_typed,
                        typed_refinements: Box::new(typed_refinements),
                    },
                    type_info: Type::Void,
                    span: stmt.span(),
                }
            }
            Stmt::While { condition, body } => {
                let cond_typed = self.check_expression(condition, &Type::Boolean);
                let cond_typed =
                    self.coerce_typed(cond_typed, &Type::Boolean, MismatchContext::Condition, None);

                let refinements = self.analyze_condition(&cond_typed);
                let mut scope = ScopeGuard::new(self, ScopeKind::Block);
                let mut true_path = vec![];
                for (name, ty) in refinements.true_path.iter() {
                    if let Some(case) = scope.scopes.refine(name, ty.clone()) {
                        true_path.push(case)
                    }
                }
                let body = scope.check_stmt(body);
                drop(scope);

                TypedStmt {
                    kind: StmtKind::While {
                        condition: cond_typed,
                        body: Box::new(body),
                        true_path,
                    },
                    type_info: Type::Void,
                    span: stmt.span(),
                }
            }
            Stmt::Function {
                name,
                body,
                signature,
                generics,
            } => {
                let res = self
                    .check_function(name, signature, body, generics)
                    .recover(&mut self.errors, TypedExpr::new_blank(stmt.span()));
                if !self.scopes.is_global() {
                    let decl = Declaration::function(name.lexeme.into(), res.ty.clone(), name.span);
                    self.scopes.declare(decl).ok_or_report(&mut self.errors);
                }
                let func = self
                    .scopes
                    .lookup(name.lexeme)
                    .expect("Function should have been declared!");
                TypedStmt {
                    span: res.span,
                    type_info: Type::Void,
                    kind: StmtKind::Function {
                        name: name.lexeme.into(),
                        target: func.1,
                        function_decl: res,
                    },
                }
            }
            Stmt::Return { value, keyword } => {
                if let Some((func_return_type, func_span)) = self.scopes.return_type() {
                    let coerced_return = self.coerce_expression(
                        value,
                        &func_return_type,
                        MismatchContext::Return,
                        Some(func_span),
                    );

                    TypedStmt {
                        span: coerced_return.span.merge(keyword.span),
                        kind: StmtKind::Return(coerced_return),
                        type_info: Type::Void,
                    }
                } else {
                    let span = stmt.span();
                    self.report(TypeCheckerError::InvalidReturnOutsideFunction { span });

                    TypedStmt::new_blank(span)
                }
            }

            Stmt::Struct { name, .. } => {
                // structs already defined
                self.non_global("Struct", name);

                TypedStmt {
                    kind: StmtKind::StructDecl {},
                    span: stmt.span(),
                    type_info: Type::Void,
                }
            }
            Stmt::Interface { name, .. } => {
                self.non_global("Interface", name);
                TypedStmt {
                    kind: StmtKind::Blank {},
                    span: stmt.span(),
                    type_info: Type::Void,
                }
            }
            Stmt::Enum { name, .. } => {
                self.non_global("Enum", name);
                TypedStmt {
                    kind: StmtKind::EnumDecl {},
                    span: stmt.span(),
                    type_info: Type::Void,
                }
            }
            Stmt::Match { value, arms } => self
                .check_match_stmt(value, arms)
                .recover(&mut self.errors, TypedStmt::new_blank(stmt.span())),
        }
    }
    fn non_global(&mut self, kind: &'static str, name: &Token<'src>) -> bool {
        if !self.scopes.is_global() {
            self.report(TypeCheckerError::NonGlobalDeclaration {
                kind,
                name: name.lexeme.to_string(),
                span: name.span,
            });
            true
        } else {
            false
        }
    }
}
