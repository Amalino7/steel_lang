use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Stmt, TypeAst};
use crate::scanner::Token;
use crate::typechecker::TypeChecker;
use crate::typechecker::core::ast::{StmtKind, TypedStmt};
use crate::typechecker::core::error::{MismatchContext, Recoverable, TypeCheckerError};
use crate::typechecker::core::types::{FunctionType, Type};
use crate::typechecker::scope::guards::{ScopeGuard, TypeScopeGuard};
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::variables::Declaration;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_stmt(&mut self, stmt: &Stmt<'src>) -> TypedStmt {
        match stmt {
            Stmt::Import(import) => TypedStmt::new_blank(import.keyword.span),
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

                let is_global = self.scopes.is_global();
                let mut guard = if is_global {
                    ScopeGuard::new(self, ScopeKind::Function) // This is necessary to handle global vars that use lazy eval
                } else {
                    ScopeGuard::new(self, ScopeKind::Block)
                };

                let coerced_value = guard.coerce_expression(
                    value,
                    &declared_type,
                    MismatchContext::Let,
                    type_annotation_span,
                );

                let reserved = if is_global {
                    guard.scopes.max_index()
                } else {
                    0
                };

                drop(guard);

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
                        reserved: reserved as u16,
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
                interfaces, name, ..
            } => {
                if self.non_global("impl", &name.0) {
                    return TypedStmt::new_blank(stmt.span());
                }
                self.define_impl(impl_block, interfaces, &name.0)
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
                if !self.scopes.is_global() {
                    let mut guard = TypeScopeGuard::new_function(self, generics);

                    let func_ty = guard.res().resolve_generic_func(signature).recover(
                        &mut guard.errors,
                        Rc::new(FunctionType {
                            is_vararg: false,
                            params: vec![],
                            return_type: Type::Error,
                            type_params: vec![],
                        }),
                    );

                    let (fn_decl, fn_type, fn_span) =
                        guard.check_function(name, signature, body, func_ty);
                    let decl = Declaration::function(name.lexeme.into(), fn_type, name.span);

                    let target = guard
                        .scopes
                        .declare(decl)
                        .ok_or_report(&mut guard.errors)
                        .unwrap_or(ResolvedVar::Global(0));

                    TypedStmt {
                        span: fn_span,
                        type_info: Type::Void,
                        kind: StmtKind::Function {
                            name: name.lexeme.into(),
                            target,
                            decl: fn_decl,
                        },
                    }
                } else {
                    TypedStmt::new_blank(name.span)
                }
            }
            Stmt::ExternFunction { name, .. } => {
                if self.non_global("extern func", name) {
                    return TypedStmt::new_blank(stmt.span());
                }
                let (_, location) = self
                    .scopes
                    .lookup(name.lexeme)
                    .expect("extern func should have been declared");
                TypedStmt {
                    kind: StmtKind::ExternFunction {
                        name: name.lexeme.into(),
                        target: location,
                    },
                    span: name.span,
                    type_info: Type::Void,
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
