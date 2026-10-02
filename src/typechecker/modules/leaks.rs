//! Private-type leak pass (E078): a type private to this module must not appear in its public API.

use crate::resolver::Exports;
use crate::scanner::Span;
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::types::{NameTypeId, Type};
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashSet;

impl<'ctx> TypeChecker<'ctx> {
    pub(crate) fn check_leaks(&mut self, exports: &Exports) {
        let mut surface = self.public_surface(exports);
        // Guarantees stable diagnostics order.
        surface.sort_by_key(|(_, span, _)| (span.file_id, span.start, span.end));

        let mut reported: HashSet<(Symbol, NameTypeId)> = HashSet::new();
        for (item, span, ty) in surface {
            let mut leaked = Vec::new();
            self.collect_private_types(&ty, &mut leaked);
            for type_id in leaked {
                if !reported.insert((item.clone(), type_id)) {
                    continue;
                }
                let error = TypeCheckerError::PrivateTypeInPublicApi {
                    item: item.clone(),
                    private_type: self.sys.get_name(type_id),
                    span,
                    type_origin: self.sys.get_origin(type_id).unwrap_or_default(),
                };
                self.report(error);
            }
        }
    }

    /// Every (item name, item span, type) visible from outside this module.
    fn public_surface(&self, exports: &Exports) -> Vec<(Symbol, Span, Type)> {
        let mut surface = Vec::new();

        for (name, var) in &exports.vars {
            // Re-export guard: imported items are not part of this module's API.
            if var.is_public && var.span.file_id == self.file_id {
                surface.push((name.clone(), var.span, var.type_info.clone()));
            }
        }

        for &type_id in exports.types.values() {
            let Some(origin) = self.sys.get_origin(type_id) else {
                continue;
            };
            // Re-export guard: imported items are not part of this module's API.
            if origin.file_id != self.file_id || !self.sys.is_public(type_id) {
                continue;
            }
            let name = self.sys.get_name(type_id);
            match type_id {
                NameTypeId::Struct(id) => {
                    for (ty, is_public) in self.sys.get_struct(id).fields_with_visibility() {
                        if is_public {
                            surface.push((name.clone(), origin, ty.clone()));
                        }
                    }
                }
                NameTypeId::Enum(id) => {
                    for ty in self.sys.get_enum(id).variant_types() {
                        // Struct-style payloads are stored as their own (always public) structs.
                        if let Type::Struct(payload, _) = ty {
                            for (field_ty, _) in
                                self.sys.get_struct(*payload).fields_with_visibility()
                            {
                                surface.push((name.clone(), origin, field_ty.clone()));
                            }
                        } else {
                            surface.push((name.clone(), origin, ty.clone()));
                        }
                    }
                }
                NameTypeId::Interface(id) => {
                    for (_, ty) in self.sys.get_interface(id).methods.values() {
                        surface.push((name.clone(), origin, ty.clone()));
                    }
                }
                NameTypeId::Generic(_) | NameTypeId::Primitive(_) => {}
            }
            for method in self.sys.inherent_methods_of(type_id) {
                let info = self.sys.get_method_info(method);
                if info.is_public {
                    surface.push((name.clone(), info.origin, info.func_type.clone()));
                }
            }
        }

        for (_, method_name, method) in exports.extensions.iter() {
            let info = self.sys.get_method_info(method);
            if info.is_public {
                surface.push((method_name.clone(), info.origin, info.func_type.clone()));
            }
        }

        surface
    }

    /// Collects the named types in `ty` that are defined in this module and not `public`.
    fn collect_private_types(&self, ty: &Type, out: &mut Vec<NameTypeId>) {
        let named = match ty {
            Type::Struct(id, _) => Some(NameTypeId::Struct(*id)),
            Type::Enum(id, _) => Some(NameTypeId::Enum(*id)),
            Type::Interface(id) => Some(NameTypeId::Interface(*id)),
            Type::Metatype(id, _) if !matches!(id, NameTypeId::Generic(_)) => Some(*id),
            _ => None,
        };
        if let Some(id) = named
            && self.sys.defining_file(id) == self.file_id
            && !self.sys.is_public(id)
        {
            out.push(id);
        }
        match ty {
            Type::Optional(inner) => self.collect_private_types(inner, out),
            Type::Function(func) => {
                for (_, param) in &func.params {
                    self.collect_private_types(param, out);
                }
                self.collect_private_types(&func.return_type, out);
            }
            Type::Tuple(tuple) => {
                for elem in &tuple.types {
                    self.collect_private_types(elem, out);
                }
            }
            Type::Struct(_, args) | Type::Enum(_, args) | Type::Metatype(_, args) => {
                for arg in args.iter() {
                    self.collect_private_types(arg, out);
                }
            }
            _ => {}
        }
    }
}
