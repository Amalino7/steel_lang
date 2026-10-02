use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{FunctionSig, Stmt};
use crate::scanner::{Span, Token};
use crate::typechecker::core::ast::{StmtKind, TypedStmt};
use crate::typechecker::core::error::{Recoverable, TypeCheckerError};
use crate::typechecker::core::types::{FunctionType, Type};
use crate::typechecker::scope::guards::TypeScopeGuard;
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::{Symbol, TypeChecker};
use std::rc::Rc;

/// A global function or impl method whose body is checked after all globals are declared.
pub(crate) type GlobalFn<'a, 'src> = (
    &'a Token<'src>,
    &'a FunctionSig<'src>,
    &'a Stmt<'src>,
    Rc<FunctionType>,
    ResolvedVar,
);

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_functions<'a>(
        &mut self,
        ast: &'a [Stmt<'src>],
        typed_ast: &mut Vec<TypedStmt>,
    ) -> Vec<GlobalFn<'a, 'src>> {
        let mut func_types = vec![];
        for stmt in ast.iter() {
            match stmt {
                Stmt::ExternFunction {
                    name,
                    generics,
                    signature,
                    is_public,
                    ..
                } => {
                    let mut guard = TypeScopeGuard::new_function(self, generics);
                    let func_ty = guard
                        .res()
                        .resolve_generic_func(signature)
                        .map(Type::Function);

                    guard.declare_function(name.lexeme.into(), name.span, func_ty, *is_public);
                }
                Stmt::Function {
                    name,
                    generics,
                    signature,
                    body,
                    is_public,
                    ..
                } => {
                    let mut guard = TypeScopeGuard::new_function(self, generics);
                    let func_ty = guard
                        .res()
                        .resolve_generic_func(signature)
                        .map(Type::Function);

                    let location = guard.declare_function(
                        name.lexeme.into(),
                        name.span,
                        func_ty.clone(),
                        *is_public,
                    );

                    if let Ok(Type::Function(ty)) = &func_ty {
                        func_types.push((name, signature, body.as_ref(), ty.clone(), location));
                    }
                }
                Stmt::Impl {
                    interfaces,
                    name,
                    methods,
                    generics,
                } => self.declare_impl_methods(
                    name,
                    interfaces,
                    methods,
                    generics,
                    typed_ast,
                    &mut func_types,
                ),
                _ => {}
            }
        }
        func_types
    }

    pub(crate) fn define_global_functions<'a>(
        &mut self,
        funcs: Vec<GlobalFn<'a, 'src>>,
        typed_ast: &mut Vec<TypedStmt>,
    ) {
        for (name, sig, body, ty, target) in funcs {
            let mut guard = TypeScopeGuard::old_function(self, &ty.type_params);
            let (decl, _, fn_span) = guard.check_function(name, sig, body, ty);
            typed_ast.push(TypedStmt {
                span: fn_span,
                type_info: Type::Void,
                kind: StmtKind::Function {
                    name: name.lexeme.into(),
                    target,
                    decl,
                },
            })
        }
    }

    fn declare_function(
        &mut self,
        name: Symbol,
        span: Span,
        func_type: Result<Type, TypeCheckerError>,
        is_public: bool,
    ) -> ResolvedVar {
        let func_type = func_type.recover(&mut self.errors, Type::Error);
        let decl = Declaration::global_function(name, func_type, span).public(is_public);
        self.scopes
            .declare(decl)
            .ok_or_report(&mut self.errors)
            .unwrap_or(ResolvedVar::Global(0)) // TODO rethink
    }
}
