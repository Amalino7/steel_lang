use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Ident, ImplDecl, NamedType};
use crate::scanner::Span;
use crate::typechecker::check::globals::GlobalFn;
use crate::typechecker::core::ast::{StmtKind, TypedStmt};
use crate::typechecker::core::error::{GenericError, Mismatch, Recoverable, TypeCheckerError};
use crate::typechecker::core::types::{NameTypeId, Type};
use crate::typechecker::scope::binding::{Binding, Origin};
use crate::typechecker::scope::guards::TypeScopeGuard;
use crate::typechecker::system::ImplMethod;
use crate::typechecker::{Symbol, TypeChecker};
use std::iter::repeat_n;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_impl_methods<'a>(
        &mut self,
        decl: &'a ImplDecl<'src>,
        typed_ast: &mut Vec<TypedStmt>,
        func_types: &mut Vec<GlobalFn<'a, 'src>>,
    ) {
        let generics = decl.generics.params();
        let impl_gen_count = generics.len();
        let generic_ids = self.sys.declare_ids(generics);

        let mut partial_guard = TypeScopeGuard::new_type_params(self, generics, &generic_ids);

        let self_ty = partial_guard
            .res()
            .resolve_named(&decl.target)
            .recover(&mut partial_guard.errors, Type::Error);

        let mut guard = TypeScopeGuard::new_impl(&mut partial_guard, self_ty);

        for interface in &decl.interfaces {
            guard.check_impl_interface(interface);
        }

        let target = decl.target.name;
        for method in &decl.methods {
            let func_name = method.sig.name;
            let method_generics = method.sig.generics.params();
            let generic_ids = guard.sys.declare_ids(method_generics);

            let mut inner_guard =
                TypeScopeGuard::new_type_params(&mut guard, method_generics, &generic_ids);
            let func_ty = inner_guard
                .res()
                .resolve_generic_func(&method.sig)
                .map(Type::Function);

            let location = ResolvedVar::Global(inner_guard.id_generator.next().get() as u16);

            match (&func_ty, &method.body) {
                (Ok(Type::Function(ty)), Some(body)) => {
                    func_types.push((method, body, ty.clone(), location.clone()));
                }
                (_, Some(_)) => {}
                (_, None) => {
                    let mangled_name: Symbol = format!("{}.{}", target.name, func_name.name).into();
                    inner_guard.check_extern(&mangled_name, func_name.span);

                    typed_ast.push(TypedStmt {
                        kind: StmtKind::ExternFunction {
                            name: mangled_name.to_string().into(),
                            target: location.clone(),
                        },
                        span: method.span,
                        type_info: Type::Nil,
                    });
                }
            }

            let func_type = func_ty.recover(&mut inner_guard.errors, Type::Error);
            inner_guard.register_impl_method(
                &target,
                &func_name,
                func_type,
                method.vis.is_public(),
                location,
                impl_gen_count,
            );
        }
    }

    /// An interface in an `impl` header must exist and, having no generic parameters, take no type arguments.
    fn check_impl_interface(&mut self, interface: &NamedType<'src>) {
        let interface_id = self.type_scopes.lookup_type(interface.name.name);
        if !matches!(interface_id, Some(NameTypeId::Interface(_))) {
            self.errors.push(TypeCheckerError::UndefinedType {
                name: interface.name.name.to_string(),
                span: interface.name.span,
                message: "Interface does not exist.",
            });
        } else if !interface.args.is_empty() {
            self.errors
                .push(TypeCheckerError::Generic(GenericError::CountMismatch {
                    span: interface.span,
                    found: interface.args.len(),
                    expected: 0,
                    type_name: interface.name.name.to_string(),
                }));
        }
    }

    fn register_impl_method(
        &mut self,
        type_name: &Ident,
        func_name: &Ident,
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
            self_type,
            location,
        });
        let binding = Binding::new(
            method_id,
            Origin::Defined {
                span: func_name.span,
                public: is_public,
            },
        );

        let Some(type_name_id) = self.type_scopes.lookup_type(type_name.name) else {
            return;
        };
        if self.sys.defining_file(type_name_id) == self.file_id {
            let res =
                self.sys
                    .declare_inherent_method(type_name_id, func_name.name.into(), binding);
            self.redeclaration_method(res, func_name.name, func_name.span);
            return;
        }
        if let Some(inherent) = self
            .sys
            .lookup_inherent_method(type_name_id, func_name.name)
        {
            let inherent_origin = self.sys.get_method_info(inherent.target).origin;
            let type_name = self.sys.get_name(type_name_id);
            self.report(TypeCheckerError::ExtensionShadowsInherent {
                method_name: func_name.name.into(),
                type_name,
                span: func_name.span,
                inherent_origin,
            });
        }
        let res = self
            .type_scopes
            .declare_method(type_name_id, func_name.name.into(), binding);
        self.extension_conflict(
            res,
            type_name_id,
            func_name.name,
            func_name.span,
            func_name.span,
        );
    }

    /// Checks that the target implements each interface in the header. Missing methods are reported on the
    /// header.
    pub(crate) fn define_impl(&mut self, decl: &ImplDecl<'src>) -> TypedStmt {
        let mut vtables = vec![];
        for interface in &decl.interfaces {
            if let Some(vtable) = self.check_interface_conformance(
                decl.target.name.name,
                &interface.name,
                decl.header_span(),
            ) {
                vtables.push(vtable);
            }
        }

        TypedStmt {
            kind: StmtKind::Impl {
                vtables: vtables.into(),
            },
            span: decl.span,
            type_info: Type::Void,
        }
    }

    fn check_interface_conformance(
        &mut self,
        type_name: &str,
        interface: &Ident,
        impl_span: Span,
    ) -> Option<Vec<ResolvedVar>> {
        let Some(NameTypeId::Interface(id)) = self.type_scopes.lookup_type(interface.name) else {
            return None;
        };

        let type_id = self.type_scopes.lookup_type(type_name)?;

        let interface_type = self.sys.get_interface(id);

        let mut vtable =
            repeat_n(ResolvedVar::Local(0), interface_type.methods.len()).collect::<Vec<_>>();
        let mut missing_methods = vec![];

        for (method_name, (location, method_type)) in interface_type.methods.iter() {
            let Some(method) = self.lookup_method(type_id, method_name) else {
                missing_methods.push(method_name.clone());
                continue;
            };
            let method_info = self.sys.get_method_info(method.target);
            if let Err(err) = self
                .infer_ctx
                .unify_types(method_type, &method_info.func_type)
            {
                let full_err = TypeCheckerError::InterfaceMethodTypeMismatch {
                    method_name: method_name.clone(),
                    interface: interface.name.to_string(),
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
                // A public interface needs public methods. An imported extension was exported, so
                // it is public; only a method defined here without `public` fails.
                let is_private = matches!(method.origin, Origin::Defined { public: false, .. });
                if interface_type.is_public && is_private {
                    self.errors
                        .push(TypeCheckerError::InterfaceMethodNotPublic {
                            method_name: method_name.to_string(),
                            interface_name: interface.name.to_string(),
                            span: method_info.origin,
                        });
                }

                vtable[*location] = method_info.location.clone();
            }
        }

        if !missing_methods.is_empty() {
            self.report(TypeCheckerError::MissingInterfaceMethods {
                missing_methods,
                interface: interface.name.to_string(),
                span: impl_span,
                interface_origin: interface_type.origin,
            });
        }
        self.sys.define_impl(type_id, id);

        Some(vtable)
    }
}
