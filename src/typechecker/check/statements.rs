use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Block, FnDecl, Ident, LetDecl, Stmt, While};
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
            Stmt::Expr(expr) => {
                let typed_expr = self.check_expression(expr, &Type::Unknown);
                TypedStmt {
                    kind: StmtKind::Expression(typed_expr),
                    span: expr.span,
                    type_info: Type::Void,
                }
            }
            Stmt::Let(decl) => self.check_let(decl),
            Stmt::While(stmt) => self.check_while(stmt),
            Stmt::Fn(decl) if decl.is_extern() => {
                let name = decl.sig.name;
                if self.non_global("extern func", &name) {
                    return TypedStmt::new_blank(decl.span);
                }
                let (_, location) = self
                    .scopes
                    .lookup(name.name)
                    .expect("extern func should have been declared");
                TypedStmt {
                    kind: StmtKind::ExternFunction {
                        name: name.name.into(),
                        target: location,
                    },
                    span: decl.span,
                    type_info: Type::Void,
                }
            }
            Stmt::Fn(decl) => self.check_local_function(decl),
            Stmt::Impl(decl) => {
                if self.non_global("impl", &decl.target.name) {
                    return TypedStmt::new_blank(decl.span);
                }
                self.define_impl(decl)
            }
            Stmt::Struct(decl) => {
                // Already defined with the other globals.
                self.non_global("Struct", &decl.name);
                TypedStmt {
                    kind: StmtKind::StructDecl {},
                    span: decl.span,
                    type_info: Type::Void,
                }
            }
            Stmt::Interface(decl) => {
                self.non_global("Interface", &decl.name);
                TypedStmt {
                    kind: StmtKind::Blank {},
                    span: decl.span,
                    type_info: Type::Void,
                }
            }
            Stmt::Enum(decl) => {
                self.non_global("Enum", &decl.name);
                TypedStmt {
                    kind: StmtKind::EnumDecl {},
                    span: decl.span,
                    type_info: Type::Void,
                }
            }
        }
    }

    /// A function or `while` body, in its own block scope.
    pub(crate) fn check_body(&mut self, body: &Block<'src>) -> TypedStmt {
        let mut scope = ScopeGuard::new(self, ScopeKind::Block);
        let stmts = body
            .stmts
            .iter()
            .map(|stmt| scope.check_stmt(stmt))
            .collect::<Vec<_>>();
        TypedStmt {
            kind: StmtKind::Block {
                body: stmts,
                reserved: 0,
            },
            type_info: Type::Void,
            span: body.span,
        }
    }

    fn check_let(&mut self, decl: &LetDecl<'src>) -> TypedStmt {
        let declared_type = self
            .res()
            .resolve_annotation(decl.ty.as_ref())
            .recover(&mut self.errors, Type::Error);
        let type_annotation_span = decl.ty.as_ref().map(|ty| ty.span);

        let is_global = self.scopes.is_global();
        let mut guard = if is_global {
            ScopeGuard::new(self, ScopeKind::Function) // This is necessary to handle global vars that use lazy eval
        } else {
            ScopeGuard::new(self, ScopeKind::Block)
        };

        let coerced_value = guard.coerce_expression(
            &decl.value,
            &declared_type,
            MismatchContext::Let,
            type_annotation_span,
        );

        let max_index = if is_global {
            guard.scopes.max_index()
        } else {
            0
        };

        drop(guard);
        let reserved = self.limit_locals(max_index, decl.binding.span);
        // A binding without globals (`let _ = ...`) is initialized inline in the module script, whose
        // frame has to hold the initializer's locals too.
        self.scopes.reserve_at_least(reserved as usize);

        let final_type = if declared_type == Type::Error || declared_type == Type::Unknown {
            coerced_value.ty.clone()
        } else {
            declared_type
        };

        let typed_binding = self
            .check_binding(
                &decl.binding,
                &final_type,
                false,
                decl.vis.is_public() && is_global,
            )
            .ok_or_report(&mut self.errors);

        let kind = if let Some(tb) = typed_binding {
            StmtKind::Let {
                reserved,
                binding: tb,
                value: coerced_value,
            }
        } else {
            StmtKind::Blank
        };

        TypedStmt {
            kind,
            type_info: Type::Void,
            span: decl.binding.span,
        }
    }

    fn check_while(&mut self, stmt: &While<'src>) -> TypedStmt {
        let cond_typed = self.check_expression(&stmt.condition, &Type::Boolean);
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
        let body = scope.check_body(&stmt.body);
        drop(scope);

        TypedStmt {
            kind: StmtKind::While {
                condition: cond_typed,
                body: Box::new(body),
                true_path,
            },
            type_info: Type::Void,
            span: stmt.span,
        }
    }

    /// A function declared inside another one. Global functions are checked by `define_global_functions`.
    fn check_local_function(&mut self, decl: &FnDecl<'src>) -> TypedStmt {
        let name = decl.sig.name;
        let Some(body) = &decl.body else {
            unreachable!("externs are handled by check_stmt")
        };
        if self.scopes.is_global() {
            return TypedStmt::new_blank(name.span);
        }
        let mut guard = TypeScopeGuard::new_function(self, decl.sig.generics.params());

        let func_ty = guard.res().resolve_generic_func(&decl.sig).recover(
            &mut guard.errors,
            Rc::new(FunctionType {
                is_vararg: false,
                params: vec![],
                return_type: Type::Error,
                type_params: vec![],
            }),
        );

        let (fn_decl, fn_type) = guard.check_function(decl, body, func_ty);
        let declaration = Declaration::function(name.name.into(), fn_type, name.span);

        let target = guard
            .scopes
            .declare(declaration)
            .ok_or_report(&mut guard.errors)
            .unwrap_or(ResolvedVar::Global(0));

        TypedStmt {
            span: decl.span,
            type_info: Type::Void,
            kind: StmtKind::Function {
                name: name.name.into(),
                target,
                decl: fn_decl,
            },
        }
    }

    fn non_global(&mut self, kind: &'static str, name: &Ident<'src>) -> bool {
        if !self.scopes.is_global() {
            self.report(TypeCheckerError::NonGlobalDeclaration {
                kind,
                name: name.name.to_string(),
                span: name.span,
            });
            true
        } else {
            false
        }
    }
}
