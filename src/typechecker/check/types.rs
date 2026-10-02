use crate::parser::ast::{Stmt, StructField, VariantType};
use crate::scanner::Span;
use crate::typechecker::core::error::{
    DuplicateDefinition, DuplicateKind, Recoverable, TypeCheckerError,
};
use crate::typechecker::core::types::type_defs::FieldDef;
use crate::typechecker::core::types::{EnumId, InterfaceId, NameTypeId, StructId, Type};
use crate::typechecker::scope::guards::TypeScopeGuard;
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_types<'a>(
        &mut self,
        ast: &'a [Stmt<'src>],
    ) -> Vec<(NameTypeId, &'a Stmt<'src>)> {
        // declare global types by name
        let mut tasks = vec![];

        for stmt in ast.iter() {
            match stmt {
                Stmt::Struct {
                    name,
                    generics,
                    is_public,
                    ..
                } => {
                    let id = self.sys.declare_struct(
                        stmt.span(),
                        name.lexeme.into(),
                        generics,
                        *is_public,
                    );

                    let res = self
                        .type_scopes
                        .declare_global(name.lexeme.into(), id.into());
                    self.redeclaration_type(res, name.span);

                    tasks.push((id.into(), stmt))
                }
                Stmt::Interface {
                    name, is_public, ..
                } => {
                    let id =
                        self.sys
                            .declare_interface(name.lexeme.into(), stmt.span(), *is_public);
                    let res = self
                        .type_scopes
                        .declare_global(name.lexeme.into(), id.into());

                    self.redeclaration_type(res, name.span);

                    tasks.push((id.into(), stmt))
                }
                Stmt::Enum {
                    name,
                    generics,
                    is_public,
                    ..
                } => {
                    let id = self.sys.declare_enum(
                        stmt.span(),
                        name.lexeme.into(),
                        generics,
                        *is_public,
                    );
                    let res = self
                        .type_scopes
                        .declare_global(name.lexeme.into(), id.into());

                    self.redeclaration_type(res, name.span);
                    tasks.push((id.into(), stmt))
                }
                _ => {}
            }
        }
        tasks
    }

    pub(crate) fn redeclaration_type(
        &mut self,
        old_id: Result<(), NameTypeId>,
        new_location: Span,
    ) {
        if let Err(old_id) = old_id {
            let origin = self.sys.get_origin(old_id);
            self.report(TypeCheckerError::Duplicate(DuplicateDefinition {
                kind: DuplicateKind::Type,
                name: self.sys.get_name(old_id).to_string(),
                span: new_location,
                original: origin,
            }))
        }
    }

    pub fn define_types(&mut self, tasks: Vec<(NameTypeId, &Stmt<'src>)>) {
        for (ty_id, ast) in tasks {
            match ty_id {
                NameTypeId::Struct(id) => {
                    self.define_struct(id, ast);
                }
                NameTypeId::Enum(id) => {
                    self.define_enum(id, ast);
                }
                NameTypeId::Interface(id) => {
                    self.define_interface(id, ast);
                }
                _ => {}
            }
        }
    }

    fn define_interface(&mut self, id: InterfaceId, stmt: &Stmt<'src>) {
        if let Stmt::Interface {
            methods, generics, ..
        } = stmt
        {
            let generic_ids = self.sys.get_generic_param_names(id.into());

            let mut guard = TypeScopeGuard::new_type_params(self, generics, &generic_ids);
            let mut guard = TypeScopeGuard::new_impl(&mut guard, Type::Never);
            let mut method_map: HashMap<String, (usize, Type)> = HashMap::new();
            for (i, sig) in methods.iter().enumerate() {
                let ty = guard
                    .res()
                    .resolve_generic_func(&sig.signature)
                    .map(Type::Function);
                let ty = ty.recover(&mut guard.errors, Type::Error);
                method_map.insert(sig.name.lexeme.to_string(), (i, ty));
            }

            guard.sys.define_interface(&id, method_map);
        }
    }

    fn define_struct(&mut self, id: StructId, stmt: &Stmt<'src>) {
        if let Stmt::Struct {
            fields, generics, ..
        } = stmt
        {
            let generic_ids = self.sys.get_generic_param_names(id.into());

            let mut guard = TypeScopeGuard::new_type_params(self, generics, &generic_ids);
            let field_types = guard.define_struct_fields(fields, false);
            guard.sys.define_struct(id, field_types);
        }
    }

    fn define_enum(&mut self, id: EnumId, stmt: &Stmt<'src>) {
        if let Stmt::Enum {
            name,
            variants,
            generics,
            ..
        } = stmt
        {
            let generic_ids = self.sys.get_generic_param_names(id.into());

            let mut guard = TypeScopeGuard::new_type_params(self, generics, &generic_ids);
            let mut typed_variants: HashMap<Symbol, (usize, Type)> = HashMap::new();
            let mut seen_variants: HashMap<Symbol, Span> = HashMap::new();
            let mut valid_idx = 0usize;
            for (v_name, fields) in variants.iter() {
                let sym: Symbol = v_name.lexeme.into();
                if let Some(&original) = seen_variants.get(&sym) {
                    guard.report(TypeCheckerError::Duplicate(DuplicateDefinition {
                        kind: DuplicateKind::Variant,
                        name: v_name.lexeme.to_string(),
                        span: v_name.span,
                        original: Some(original),
                    }));
                    continue;
                }
                seen_variants.insert(sym.clone(), v_name.span);

                let ty = match fields {
                    VariantType::Tuple(tuple_def) => {
                        let res = guard.res().resolve_tuple(tuple_def);
                        res.recover(&mut guard.errors, Type::Error)
                    }
                    VariantType::Struct(struct_def) => {
                        let field_types = guard.define_struct_fields(struct_def, true);
                        let full_name: Symbol = format!("{}.{}", name.lexeme, v_name.lexeme).into();

                        let id =
                            guard
                                .sys
                                .declare_struct(v_name.span, full_name.clone(), &[], true);
                        guard.sys.define_struct(id, field_types);
                        Type::Struct(id, vec![].into())
                    }
                    VariantType::Unit => Type::Void,
                };

                typed_variants.insert(sym, (valid_idx, ty));
                valid_idx += 1;
            }
            guard.sys.define_enum(&id, typed_variants);
        }
    }

    fn define_struct_fields(
        &mut self,
        fields: &[StructField<'src>],
        is_enum: bool,
    ) -> Vec<FieldDef> {
        let mut field_defs = vec![];
        let mut seen: HashMap<Symbol, Span> = HashMap::new();

        for StructField {
            name,
            is_public,
            type_info,
        } in fields
        {
            let sym_name: Symbol = name.lexeme.into();
            if let Some(&original) = seen.get(&sym_name) {
                self.report(TypeCheckerError::Duplicate(DuplicateDefinition {
                    kind: DuplicateKind::Field,
                    name: name.lexeme.to_string(),
                    span: name.span,
                    original: Some(original),
                }));
            } else {
                seen.insert(sym_name.clone(), name.span);
                let field_type = self
                    .res()
                    .resolve(type_info)
                    .recover(&mut self.errors, Type::Error);
                field_defs.push(FieldDef {
                    name: sym_name,
                    index: field_defs.len(),
                    ty: field_type,
                    is_public: *is_public || is_enum,
                    name_span: name.span,
                });
            }
        }
        field_defs
    }
}
