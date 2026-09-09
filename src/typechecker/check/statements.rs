use crate::parser::ast::{Stmt, TypeAst};
use crate::scanner::Token;
use crate::typechecker::core::ast::{StmtKind, TypedStmt};
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
                let (fn_decl, fn_type, fn_span) =
                    self.check_function(name, signature, body, generics);
                if !self.scopes.is_global() {
                    let decl = Declaration::function(name.lexeme.into(), fn_type, name.span);
                    self.scopes.declare(decl).ok_or_report(&mut self.errors);
                }
                let (_, target) = self
                    .scopes
                    .lookup(name.lexeme)
                    .expect("Function should have been declared!");
                TypedStmt {
                    span: fn_span,
                    type_info: Type::Void,
                    kind: StmtKind::Function {
                        name: name.lexeme.into(),
                        target,
                        decl: fn_decl,
                    },
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
