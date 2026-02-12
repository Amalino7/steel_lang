use crate::parser::ast::Stmt;
use crate::typechecker::error::{Recoverable, TypeCheckerError};
use crate::typechecker::scope_manager::{ScopeGuard, ScopeType};
use crate::typechecker::type_ast::{StmtKind, TypedExpr, TypedRefinements, TypedStmt};
use crate::typechecker::types::Type;
use crate::typechecker::{FunctionContext, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_stmt(&mut self, stmt: &Stmt<'src>) -> TypedStmt {
        match stmt {
            Stmt::Expression(expr) => {
                let typed_expr = self
                    .check_expression(expr, &Type::Unknown)
                    .recover(&mut self.errors, TypedExpr::new_blank(expr.span()));

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
                    .ok_log(&mut self.errors)
                    .unwrap_or(Type::Error);

                let coerced_value = self
                    .coerce_expression(value, &declared_type)
                    .recover(&mut self.errors, TypedExpr::new_blank(value.span()));

                let final_type = if declared_type == Type::Error || declared_type == Type::Unknown {
                    coerced_value.ty.clone()
                } else {
                    declared_type
                };

                let typed_binding = self
                    .check_binding(binding, &final_type)
                    .ok_log(&mut self.errors);

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
            } => self.define_impl(impl_block, interfaces, name, methods, generics),
            Stmt::Block { body, brace_token } => {
                let mut scope = ScopeGuard::new(self, ScopeType::Block);
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
                let cond_typed = self
                    .check_expression(condition, &Type::Boolean)
                    .unwrap_or_else(|e| {
                        self.errors.push(e);
                        TypedExpr::new_blank(condition.span())
                    });

                if cond_typed.ty != Type::Boolean
                    && cond_typed.ty != Type::Unknown
                    && cond_typed.ty != Type::Error
                {
                    self.errors.push(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_typed.ty.clone(),
                        span: condition.span(),
                        message: "If value must be a boolean.",
                    });
                }

                let refinements = self.analyze_condition(&cond_typed);
                let mut typed_refinements = TypedRefinements {
                    true_path: vec![],
                    else_path: vec![],
                    after_path: vec![],
                };
                let mut scope = ScopeGuard::new(self, ScopeType::Block);
                for (name, ty) in refinements.true_path.iter() {
                    if let Some(case) = scope.scopes.refine(name, ty.clone()) {
                        typed_refinements.true_path.push(case)
                    }
                }
                let then_branch_typed = scope.check_stmt(then_branch);
                drop(scope);

                let else_branch_typed = if let Some(else_branch) = else_branch {
                    let mut scope = ScopeGuard::new(self, ScopeType::Block);
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
                let cond_typed = self
                    .check_expression(condition, &Type::Boolean)
                    .unwrap_or_else(|err| {
                        self.errors.push(err);
                        TypedExpr::new_blank(condition.span())
                    });
                if cond_typed.ty != Type::Boolean
                    && cond_typed.ty != Type::Unknown
                    && cond_typed.ty != Type::Error
                {
                    self.errors.push(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_typed.ty.clone(),
                        span: condition.span(),
                        message: "While value must be a boolean.",
                    });
                }

                let refinements = self.analyze_condition(&cond_typed);
                let mut scope = ScopeGuard::new(self, ScopeType::Block);
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
                let mut guard = ScopeGuard::new_type_block(self, generics);
                let func_type = guard.res().resolve_func(signature, generics).recover(
                    &mut guard.errors,
                    Type::new_function(vec![], Type::Void, vec![]),
                );

                let res = guard
                    .check_function(name, signature, body, func_type.clone(), name.lexeme.into())
                    .recover(&mut guard.errors, TypedStmt::new_blank(stmt.span()));
                drop(guard);

                if !self.scopes.is_global() {
                    self.scopes
                        .declare(name.lexeme.into(), func_type, name.span)
                        .ok_log(&mut self.errors);
                }
                res
            }
            Stmt::Return { value, keyword } => {
                if let FunctionContext::Function(func_return_type, func_span) =
                    self.current_function.clone()
                {
                    let coerced_return = match self.coerce_expression(value, &func_return_type) {
                        Ok(v) => v,
                        Err(err) => {
                            let wrapped = err.with_origin(func_span);
                            self.errors.push(wrapped);
                            TypedExpr::new_blank(value.span())
                        }
                    };

                    TypedStmt {
                        span: coerced_return.span.merge(keyword.span),
                        kind: StmtKind::Return(coerced_return),
                        type_info: Type::Void,
                    }
                } else {
                    let span = stmt.span();
                    Err(TypeCheckerError::InvalidReturnOutsideFunction { span })
                        .recover(&mut self.errors, TypedStmt::new_blank(span))
                }
            }

            Stmt::Struct { name, .. } | Stmt::Interface { name, .. } => {
                // structs already defined
                if !self.scopes.is_global() {
                    Err(TypeCheckerError::StructOutsideOfGlobalScope {
                        name: name.lexeme.to_string(),
                        span: name.span,
                    })
                    .recover(&mut self.errors, TypedStmt::new_blank(stmt.span()))
                } else {
                    TypedStmt {
                        kind: StmtKind::StructDecl {},
                        span: stmt.span(),
                        type_info: Type::Void,
                    }
                }
            }
            Stmt::Enum { name, .. } => {
                if !self.scopes.is_global() {
                    Err(TypeCheckerError::StructOutsideOfGlobalScope {
                        name: name.lexeme.to_string(),
                        span: name.span,
                    })
                    .recover(&mut self.errors, TypedStmt::new_blank(stmt.span()));
                }

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
}
