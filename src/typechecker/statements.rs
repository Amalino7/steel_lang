use crate::parser::ast::Stmt;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{StmtKind, TypedRefinements, TypedStmt};
use crate::typechecker::types::Type;
use crate::typechecker::{FunctionContext, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_stmt(&mut self, stmt: &Stmt<'src>) -> Result<TypedStmt, TypeCheckerError> {
        match stmt {
            Stmt::Expression(expr) => Ok(TypedStmt {
                kind: StmtKind::Expression(self.check_expression(expr, None)?),
                span: stmt.span(),
                type_info: Type::Void,
            }),
            Stmt::Let {
                binding,
                value,
                type_info,
            } => {
                let declared_type = Type::from_ast(type_info, &self.sys)?;
                let coerced_value = self.coerce_expression(value, &declared_type)?;
                let final_type = if declared_type == Type::Unknown {
                    coerced_value.ty.clone()
                } else {
                    declared_type
                };
                let typed_binding = self.check_binding(binding, &final_type)?;

                Ok(TypedStmt {
                    kind: StmtKind::Let {
                        binding: typed_binding,
                        value: coerced_value,
                    },
                    type_info: Type::Void,
                    span: binding.span(),
                })
            }
            impl_block @ Stmt::Impl {
                interfaces,
                name,
                methods,
                generics,
            } => self.define_impl(impl_block, interfaces, name, methods, generics),
            Stmt::Block { body, brace_token } => {
                self.scopes.begin_scope(ScopeType::Block);
                let stmts = body
                    .iter()
                    .map(|stmt| self.check_stmt(stmt))
                    .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

                self.scopes.end_scope();

                Ok(TypedStmt {
                    kind: StmtKind::Block {
                        body: stmts,
                        reserved: 0, // Only function scopes have reserved
                    },
                    type_info: Type::Void,
                    span: stmt.span().merge(brace_token.span),
                })
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_typed = self.check_expression(condition, Some(&Type::Boolean))?;

                if cond_typed.ty != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_typed.ty,
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
                self.scopes.begin_scope(ScopeType::Block);
                for (name, ty) in refinements.true_path.iter() {
                    if let Some(case) = self.scopes.refine(name, ty.clone()) {
                        typed_refinements.true_path.push(case)
                    }
                }
                let then_branch_typed = self.check_stmt(then_branch)?;
                self.scopes.end_scope();

                let else_branch_typed = if let Some(else_branch) = else_branch {
                    self.scopes.begin_scope(ScopeType::Block);
                    for (name, ty) in refinements.false_path.iter() {
                        if let Some(case) = self.scopes.refine(name, ty.clone()) {
                            typed_refinements.else_path.push(case)
                        }
                    }
                    let stmt = self.check_stmt(else_branch)?;
                    self.scopes.end_scope();
                    Some(Box::new(stmt))
                } else {
                    None
                };

                // Guard logic
                if self.stmt_returns(&then_branch_typed)? {
                    for (name, ty) in refinements.false_path {
                        if let Some(case) = self.scopes.refine(&name, ty.clone()) {
                            typed_refinements.after_path.push(case)
                        }
                    }
                }
                if let Some(else_branch_typed) = &else_branch_typed
                    && self.stmt_returns(else_branch_typed)?
                {
                    for (name, ty) in refinements.true_path {
                        if let Some(case) = self.scopes.refine(&name, ty.clone()) {
                            typed_refinements.after_path.push(case)
                        }
                    }
                }

                Ok(TypedStmt {
                    kind: StmtKind::If {
                        condition: cond_typed,
                        then_branch: Box::new(then_branch_typed),
                        else_branch: else_branch_typed,
                        typed_refinements: Box::new(typed_refinements),
                    },
                    type_info: Type::Void,
                    span: stmt.span(),
                })
            }
            Stmt::While { condition, body } => {
                let cond_type = self.check_expression(condition, Some(&Type::Boolean))?;
                let body = self.check_stmt(body)?;
                if cond_type.ty != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_type.ty,
                        span: condition.span(),
                        message: "While value must be a boolean.",
                    });
                }

                Ok(TypedStmt {
                    kind: StmtKind::While {
                        condition: cond_type,
                        body: Box::new(body),
                    },
                    type_info: Type::Void,
                    span: stmt.span(),
                })
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
                    Type::from_function_ast(type_, &self.sys, self.sys.get_active_generics())?;

                let final_type = raw_type.patch_param_names(params);

                if !self.scopes.is_global() {
                    self.scopes
                        .declare(name.lexeme.into(), final_type.clone(), name.span)?;
                }

                let res = self.check_function(name, params, body, final_type, name.lexeme.into());
                self.sys.pop_n_generics(generics.len());
                res
            }
            Stmt::Return(expr) => {
                if let FunctionContext::Function(func_return_type) = self.current_function.clone() {
                    let coerced_return = self.coerce_expression(expr, &func_return_type)?;
                    Ok(TypedStmt {
                        kind: StmtKind::Return(coerced_return),
                        span: expr.span(),
                        type_info: Type::Void,
                    })
                } else {
                    Err(TypeCheckerError::InvalidReturnOutsideFunction { span: expr.span() })
                }
            }

            Stmt::Struct { name, .. } | Stmt::Interface { name, .. } => {
                // structs already defined
                if !self.scopes.is_global() {
                    Err(TypeCheckerError::StructOutsideOfGlobalScope {
                        name: name.lexeme.to_string(),
                        span: name.span,
                    })
                } else {
                    Ok(TypedStmt {
                        kind: StmtKind::StructDecl {},
                        span: stmt.span(),
                        type_info: Type::Void,
                    })
                }
            }
            Stmt::Enum { name, .. } => {
                if !self.scopes.is_global() {
                    return Err(TypeCheckerError::StructOutsideOfGlobalScope {
                        name: name.lexeme.to_string(),
                        span: name.span,
                    });
                }

                Ok(TypedStmt {
                    kind: StmtKind::EnumDecl {},
                    span: stmt.span(),
                    type_info: Type::Void,
                })
            }
            Stmt::Match { value, arms } => self.check_match_stmt(value, arms),
        }
    }
}
