use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Stmt, TypeAst};
use crate::scanner::{Span, Token};
use crate::typechecker::check::globals::GlobalFn;
use crate::typechecker::core::ast::{StmtKind, TypedStmt};
use crate::typechecker::core::error::{Mismatch, Recoverable, TypeCheckerError};
use crate::typechecker::core::types::{NameTypeId, Type};
use crate::typechecker::scope::guards::TypeScopeGuard;
use crate::typechecker::system::ImplMethod;
use crate::typechecker::{Symbol, TypeChecker};
use std::iter::repeat_n;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_impl_methods<'a>(
        &mut self,
        name: &'a (Token<'src>, Vec<TypeAst<'src>>),
        interfaces: &[Token<'src>],
        methods: &'a [Stmt<'src>],
        generics: &[Token<'src>],
        typed_ast: &mut Vec<TypedStmt>,
        func_types: &mut Vec<GlobalFn<'a, 'src>>,
    ) {
        let impl_gen_count = generics.len();
        let generic_ids = self.sys.declare_ids(generics);

        let mut partial_guard = TypeScopeGuard::new_type_params(self, generics, &generic_ids);

        let self_ty = partial_guard
            .res()
            .resolve_named(&name.0, &name.1)
            .recover(&mut partial_guard.errors, Type::Error);

        let mut guard = TypeScopeGuard::new_impl(&mut partial_guard, self_ty);

        for interface in interfaces {
            let interface_id = guard.type_scopes.lookup_type(interface.lexeme);
            if !matches!(interface_id, Some(NameTypeId::Interface(_))) {
                guard.errors.push(TypeCheckerError::UndefinedType {
                    name: interface.lexeme.to_string(),
                    span: interface.span,
                    message: "Interface does not exist.",
                });
            }
        }

        for method in methods {
            let (func_name, signature, generics, method_public) = match method {
                Stmt::Function {
                    name,
                    signature,
                    generics,
                    is_public,
                    ..
                }
                | Stmt::ExternFunction {
                    name,
                    signature,
                    generics,
                    is_public,
                    ..
                } => (name, signature, generics, *is_public),
                _ => unreachable!(),
            };
            let generic_ids = guard.sys.declare_ids(generics);

            let mut inner_guard =
                TypeScopeGuard::new_type_params(&mut guard, generics, &generic_ids);
            let func_ty = inner_guard
                .res()
                .resolve_generic_func(signature)
                .map(Type::Function);

            let location = ResolvedVar::Global(inner_guard.id_generator.next().get() as u16);

            if let Ok(Type::Function(ty)) = &func_ty
                && let Stmt::Function { body, .. } = method
            {
                func_types.push((func_name, signature, body, ty.clone(), location.clone()));
            } else if let Stmt::ExternFunction { .. } = method {
                let mangled_name: Symbol = format!("{}.{}", name.0.lexeme, func_name.lexeme).into();

                typed_ast.push(TypedStmt {
                    kind: StmtKind::ExternFunction {
                        name: mangled_name.to_string().into(),
                        target: location.clone(),
                    },
                    span: func_name.span,
                    type_info: Type::Nil,
                });
            }

            let func_type = func_ty.recover(&mut inner_guard.errors, Type::Error);
            inner_guard.register_impl_method(
                &name.0,
                func_name,
                func_type,
                method_public,
                location,
                impl_gen_count,
            );
        }
    }

    fn register_impl_method(
        &mut self,
        type_name: &Token,
        func_name: &Token,
        func_type: Type,
        is_public: bool,
        location: ResolvedVar,
        impl_generic_count: usize,
    ) {
        // Register impl metadata so method-access can freshen and unify correctly.
        let self_type = self
            .type_scopes
            .get_self_type()
            .cloned()
            .unwrap_or(Type::Error);

        let method_id = self.sys.register_method(ImplMethod {
            impl_generic_count,
            func_type,
            origin: func_name.span,
            is_public,
            self_type,
            location,
        });

        let Some(type_name_id) = self.type_scopes.lookup_type(type_name.lexeme) else {
            return;
        };
        if self.sys.defining_file(type_name_id) == self.file_id {
            let res =
                self.sys
                    .declare_inherent_method(type_name_id, func_name.lexeme.into(), method_id);
            self.redeclaration_method(res, func_name.lexeme, func_name.span);
            return;
        }
        if let Some(inherent) = self
            .sys
            .lookup_inherent_method(type_name_id, func_name.lexeme)
        {
            let inherent_origin = self.sys.get_method_info(inherent).origin;
            let type_name = self.sys.get_name(type_name_id);
            self.report(TypeCheckerError::ExtensionShadowsInherent {
                method_name: func_name.lexeme.into(),
                type_name,
                span: func_name.span,
                inherent_origin,
            });
        }
        let res = self
            .type_scopes
            .declare_method(type_name_id, func_name.lexeme.into(), method_id);
        self.extension_conflict(
            res,
            type_name_id,
            func_name.lexeme,
            func_name.span,
            func_name.span,
        );
    }

    pub(crate) fn define_impl(
        &mut self,
        impl_block: &Stmt<'src>,
        interfaces: &[Token],
        type_name: &Token,
    ) -> TypedStmt {
        let mut vtables = vec![];
        for interface in interfaces {
            if let Some(vtable) =
                self.check_interface_conformance(type_name.lexeme, interface, impl_block.span())
            {
                vtables.push(vtable);
            }
        }

        TypedStmt {
            kind: StmtKind::Impl {
                vtables: vtables.into(),
            },
            span: impl_block.span(),
            type_info: Type::Void,
        }
    }

    fn check_interface_conformance(
        &mut self,
        type_name: &str,
        interface: &Token,
        impl_span: Span,
    ) -> Option<Vec<ResolvedVar>> {
        let Some(NameTypeId::Interface(id)) = self.type_scopes.lookup_type(interface.lexeme) else {
            return None;
        };

        let type_id = self.type_scopes.lookup_type(type_name)?;

        let interface_type = self.sys.get_interface(id);

        let mut vtable =
            repeat_n(ResolvedVar::Local(0), interface_type.methods.len()).collect::<Vec<_>>();
        let mut missing_methods = vec![];

        for (method_name, (location, method_type)) in interface_type.methods.iter() {
            let Some(method_id) = self.lookup_method(type_id, method_name) else {
                missing_methods.push(method_name.clone());
                continue;
            };
            let method_info = self.sys.get_method_info(method_id);
            if let Err(err) = self
                .infer_ctx
                .unify_types(method_type, &method_info.func_type)
            {
                let full_err = TypeCheckerError::InterfaceMethodTypeMismatch {
                    method_name: method_name.clone(),
                    interface: interface.lexeme.to_string(),
                    type_mismatch: Mismatch::enriched(
                        method_type,
                        &method_info.func_type,
                        err,
                        &self.infer_ctx,
                    ),
                    span: method_info.origin,
                    interface_origin: interface_type.origin,
                };
                self.errors.push(full_err);
            } else {
                if interface_type.is_public && !method_info.is_public {
                    self.errors
                        .push(TypeCheckerError::InterfaceMethodNotPublic {
                            method_name: method_name.to_string(),
                            interface_name: interface.lexeme.to_string(),
                            span: method_info.origin,
                        });
                }

                vtable[*location] = method_info.location.clone();
            }
        }

        if !missing_methods.is_empty() {
            self.report(TypeCheckerError::MissingInterfaceMethods {
                missing_methods,
                interface: interface.lexeme.to_string(),
                span: impl_span,
                interface_origin: interface_type.origin,
            });
        }
        self.sys.define_impl(type_id, id);

        Some(vtable)
    }
}
