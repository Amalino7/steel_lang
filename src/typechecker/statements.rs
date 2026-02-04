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
                    .check_expression(expr, None)
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
                let declared_type = Type::from_ast(type_info, &self.sys)
                    .ok_log(&mut self.errors)
                    .unwrap_or(Type::Unknown);

                let coerced_value = self
                    .coerce_expression(value, &declared_type)
                    .recover(&mut self.errors, TypedExpr::new_blank(value.span()));

                let final_type = if declared_type == Type::Unknown {
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
                    .check_expression(condition, Some(&Type::Boolean))
                    .unwrap_or_else(|e| {
                        self.errors.push(e);
                        TypedExpr::new_blank(condition.span())
                    });

                if cond_typed.ty != Type::Boolean && cond_typed.ty != Type::Unknown {
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
                let cond_type = self
                    .check_expression(condition, Some(&Type::Boolean))
                    .unwrap_or_else(|err| {
                        self.errors.push(err);
                        TypedExpr::new_blank(condition.span())
                    });
                if cond_type.ty != Type::Boolean {
                    self.errors.push(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_type.ty.clone(),
                        span: condition.span(),
                        message: "While value must be a boolean.",
                    });
                }

                let body = self.check_stmt(body);

                TypedStmt {
                    kind: StmtKind::While {
                        condition: cond_type,
                        body: Box::new(body),
                    },
                    type_info: Type::Void,
                    span: stmt.span(),
                }
            }
            Stmt::Function {
                name,
                params,
                body,
                type_,
                generics,
            } => {
                self.sys.push_generics(generics);
                let raw_type =
                    Type::from_function_ast(type_, &self.sys, self.sys.get_active_generics())
                        .recover(
                            &mut self.errors,
                            Type::new_function(vec![], Type::Void, vec![]),
                        );

                let final_type = raw_type.patch_param_names(params);

                if !self.scopes.is_global() {
                    self.scopes
                        .declare(name.lexeme.into(), final_type.clone(), name.span)
                        .ok_log(&mut self.errors);
                }

                let res = self
                    .check_function(name, params, body, final_type, name.lexeme.into())
                    .recover(&mut self.errors, TypedStmt::new_blank(stmt.span()));
                self.sys.pop_n_generics(generics.len());
                res
            }
            Stmt::Return { value, keyword } => {
                if let FunctionContext::Function(func_return_type) = self.current_function.clone() {
                    let coerced_return = self
                        .coerce_expression(value, &func_return_type)
                        .recover(&mut self.errors, TypedExpr::new_blank(value.span()));
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
