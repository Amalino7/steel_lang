use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{FunctionSig, ImportSegment, ImportType, Stmt, TypeAst, VariantType};
use crate::resolver::Exports;
use crate::scanner::{Span, Token};
use crate::typechecker::core::ast::{StmtKind, TypedStmt};
use crate::typechecker::core::error::{
    BindingError, DuplicateDefinition, DuplicateKind, Mismatch, Recoverable, TypeCheckerError,
};
use crate::typechecker::core::types::{
    EnumId, FunctionType, InterfaceId, NameTypeId, StructId, Type,
};
use crate::typechecker::scope::guards::TypeScopeGuard;
use crate::typechecker::scope::variables::{Declaration, DeclarationKind, VariableContext};
use crate::typechecker::system::{ImplMethod, MethodId};
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;
use std::iter::repeat_n;
use std::rc::Rc;

// TODO find out why when impl block fails Type is not findable.
impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_functions<'a>(
        &mut self,
        ast: &'a [Stmt<'src>],
        typed_ast: &mut Vec<TypedStmt>,
    ) -> Vec<(
        &'a Token<'src>,
        &'a FunctionSig<'src>,
        &'a Stmt<'src>,
        Rc<FunctionType>,
        ResolvedVar,
    )> {
        let mut func_types = vec![];
        for stmt in ast.iter() {
            match stmt {
                Stmt::ExternFunction {
                    name,
                    generics,
                    signature,
                } => {
                    let mut guard = TypeScopeGuard::new_function(self, generics);
                    let func_ty = guard
                        .res()
                        .resolve_generic_func(signature)
                        .map(Type::Function);

                    guard.declare_function(name.lexeme.into(), name.span, func_ty, false);
                }
                Stmt::Function {
                    name,
                    generics,
                    signature,
                    body,
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
                        false,
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
                } => {
                    let impl_gen_count = generics.len();
                    let generic_ids = self.sys.declare_ids(generics);

                    let mut partial_guard =
                        TypeScopeGuard::new_type_params(self, generics, &generic_ids);

                    let self_ty = partial_guard
                        .res()
                        .resolve_named(&name.0, &name.1)
                        .recover(&mut partial_guard.errors, Type::Error);

                    let mut guard = TypeScopeGuard::new_impl(&mut partial_guard, self_ty);

                    for interface in interfaces {
                        let interface_id = guard.type_scopes.lookup_type(interface.lexeme);
                        if let Some(NameTypeId::Interface(id)) = interface_id {
                        } else {
                            guard.errors.push(TypeCheckerError::UndefinedType {
                                name: interface.lexeme.to_string(),
                                span: interface.span,
                                message: "Interface does not exist.",
                            });
                        }
                    }

                    for method in methods {
                        let (func_name, signature, generics) = match method {
                            Stmt::Function {
                                name,
                                signature,
                                generics,
                                ..
                            }
                            | Stmt::ExternFunction {
                                name,
                                signature,
                                generics,
                            } => (name, signature, generics),
                            _ => unreachable!(),
                        };
                        let generic_ids = guard.sys.declare_ids(generics);

                        let mut inner_guard =
                            TypeScopeGuard::new_type_params(&mut guard, generics, &generic_ids);
                        let func_ty = inner_guard
                            .res()
                            .resolve_generic_func(signature)
                            .map(Type::Function);

                        let location =
                            ResolvedVar::Global(inner_guard.id_generator.next().get() as u16);

                        if let Ok(Type::Function(ty)) = &func_ty
                            && let Stmt::Function { body, .. } = method
                        {
                            func_types.push((
                                func_name,
                                signature,
                                body,
                                ty.clone(),
                                location.clone(),
                            ));
                        } else if let Stmt::ExternFunction { .. } = method {
                            let mangled_name: Symbol =
                                format!("{}.{}", name.0.lexeme, func_name.lexeme).into();

                            typed_ast.push(TypedStmt {
                                kind: StmtKind::ExternFunction {
                                    name: mangled_name.to_string().into(),
                                    target: location.clone(),
                                },
                                span: func_name.span,
                                type_info: Type::Nil,
                            });
                        }

                        // Register impl metadata so method-access can freshen and unify correctly.
                        let self_type = inner_guard
                            .type_scopes
                            .get_self_type()
                            .cloned()
                            .unwrap_or(Type::Error);

                        let func_type = func_ty.recover(&mut inner_guard.errors, Type::Error);

                        let method_id = inner_guard.sys.register_method(ImplMethod {
                            impl_generic_count: impl_gen_count,
                            func_type,
                            origin: func_name.span,
                            self_type,
                            location,
                        });

                        if let Some(type_name_id) =
                            inner_guard.type_scopes.lookup_type(name.0.lexeme)
                        {
                            let res = inner_guard.type_scopes.declare_method(
                                type_name_id,
                                func_name.lexeme.into(),
                                method_id,
                            );

                            inner_guard.redeclaration_method(res, func_name.lexeme, func_name.span);
                        }
                    }
                }
                _ => {}
            }
        }
        func_types
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
                self.check_interface_vtable(type_name.lexeme, interface, impl_block.span())
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

    pub(crate) fn define_global_functions<'a>(
        &mut self,
        funcs: Vec<(
            &'a Token<'src>,
            &'a FunctionSig<'src>,
            &'a Stmt<'src>,
            Rc<FunctionType>,
            ResolvedVar,
        )>,
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
        is_method: bool,
    ) -> ResolvedVar {
        let func_type = func_type.recover(&mut self.errors, Type::Error);
        let decl = match is_method {
            true => Declaration::method(name, func_type, span),
            false => Declaration::global_function(name, func_type, span),
        };
        self.scopes
            .declare(decl)
            .ok_or_report(&mut self.errors)
            .unwrap_or(ResolvedVar::Global(0)) // TODO rethink
    }

    fn check_interface_vtable(
        &mut self,
        type_name: &str,
        interface: &Token,
        impl_span: Span,
    ) -> Option<Vec<ResolvedVar>> {
        let Some(NameTypeId::Interface(id)) = self.type_scopes.lookup_type(interface.lexeme) else {
            return None;
        };

        let type_id = self.type_scopes.lookup_type(type_name)?;

        let interface_type = self.sys.get_interface(id).clone();

        let mut vtable =
            repeat_n(ResolvedVar::Local(0), interface_type.methods.len()).collect::<Vec<_>>();
        let mut missing_methods = vec![];

        for (method_name, (location, method_type)) in interface_type.methods.iter() {
            let Some(method_id) = self.type_scopes.lookup_method(type_id, method_name) else {
                missing_methods.push(method_name.clone());
                continue;
            };
            let method_info = self.sys.get_method_info(*method_id);
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
                self.report(full_err);
            } else {
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

    pub(crate) fn declare_global_types<'a>(
        &mut self,
        ast: &'a [Stmt<'src>],
    ) -> Vec<(NameTypeId, &'a Stmt<'src>)> {
        // declare global types by name
        let mut tasks = vec![];

        for stmt in ast.iter() {
            match stmt {
                Stmt::Struct { name, generics, .. } => {
                    let id = self
                        .sys
                        .declare_struct(stmt.span(), name.lexeme.into(), generics);

                    let res = self
                        .type_scopes
                        .declare_global(name.lexeme.into(), id.into());
                    self.redeclaration_type(res, name.span);

                    tasks.push((id.into(), stmt))
                }
                Stmt::Interface { name, .. } => {
                    let id = self.sys.declare_interface(name.lexeme.into(), stmt.span());
                    let res = self
                        .type_scopes
                        .declare_global(name.lexeme.into(), id.into());

                    self.redeclaration_type(res, name.span);

                    tasks.push((id.into(), stmt))
                }
                Stmt::Enum { name, generics, .. } => {
                    let id = self
                        .sys
                        .declare_enum(stmt.span(), name.lexeme.into(), generics);
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

    fn redeclaration_type(&mut self, old_id: Result<(), NameTypeId>, new_location: Span) {
        if let Err(old_id) = old_id {
            let origin = self.sys.get_origin(old_id);
            self.report(TypeCheckerError::Duplicate(DuplicateDefinition {
                kind: DuplicateKind::Type,
                name: self.sys.get_name(old_id).to_string(),
                span: new_location,
                original: origin.unwrap_or_default(),
            }))
        }
    }

    fn redeclaration_method(&mut self, res: Result<(), MethodId>, name: &str, new_location: Span) {
        if let Err(id) = res {
            let method_info = self.sys.get_method_info(id);
            self.report(TypeCheckerError::Binding(BindingError::Redeclaration {
                name: name.into(),
                span: new_location,
                original: method_info.origin,
                original_kind: DeclarationKind::Method,
            }));
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
            name,
            methods,
            generics,
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
            name,
            fields,
            generics,
        } = stmt
        {
            let generic_ids = self.sys.get_generic_param_names(id.into());

            let mut guard = TypeScopeGuard::new_type_params(self, generics, &generic_ids);
            let field_types = guard.define_struct_fields(fields);
            guard.sys.define_struct(id, field_types);
        }
    }

    fn define_enum(&mut self, id: EnumId, stmt: &Stmt<'src>) {
        if let Stmt::Enum {
            name,
            variants,
            generics,
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
                        original,
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
                        let field_types = guard.define_struct_fields(struct_def);
                        let full_name: Symbol = format!("{}.{}", name.lexeme, v_name.lexeme).into();

                        let id = guard
                            .sys
                            .declare_struct(v_name.span, full_name.clone(), &[]);
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
        fields: &[(Token, TypeAst)],
    ) -> HashMap<Symbol, (usize, Type)> {
        let mut field_types: HashMap<Symbol, (usize, Type)> = HashMap::new();
        let mut seen: HashMap<Symbol, Span> = HashMap::new();
        let mut valid_idx = 0usize;
        for (name, type_ast) in fields.iter() {
            let sym: Symbol = name.lexeme.into();
            if let Some(&original) = seen.get(&sym) {
                self.report(TypeCheckerError::Duplicate(DuplicateDefinition {
                    kind: DuplicateKind::Field,
                    name: name.lexeme.to_string(),
                    span: name.span,
                    original,
                }));
            } else {
                seen.insert(sym.clone(), name.span);
                let field_type = self
                    .res()
                    .resolve(type_ast)
                    .recover(&mut self.errors, Type::Error);
                field_types.insert(sym, (valid_idx, field_type));
                valid_idx += 1;
            }
        }
        field_types
    }

    pub fn imports(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast {
            if let Stmt::Import(import) = stmt {
                self.handle_import(None, &import.segment)
            }
        }
    }

    fn handle_import(&mut self, prefix: Option<&str>, import: &ImportSegment) {
        let mut path_segments = import.path.iter().map(|p| p.lexeme).collect::<Vec<_>>();

        if let Some(prefix) = prefix {
            path_segments.insert(0, prefix);
        }

        let path = path_segments.join(".");

        match &import.import_type {
            ImportType::Simple { terminator } => {
                self.import_named(&path, terminator, terminator);
            }
            ImportType::Alias { terminator, alias } => {
                self.import_named(&path, terminator, alias);
            }
            ImportType::Group { options } => {
                for option in options {
                    self.handle_import(Some(&path), option);
                }
            }
            ImportType::Wildcard => {
                let package_id = *self.module_graph.name_to_id_map.get(&path).unwrap();
                let module_info = &self.module_graph.modules[package_id.0 as usize];
                self.import_all(&module_info.exports);
            }
        }
    }

    fn import_named(&mut self, path: &str, terminator: &Token, target: &Token) {
        let package_id = *self.module_graph.name_to_id_map.get(path).unwrap();

        let module_info = &self.module_graph.modules[package_id.0 as usize];

        if let Some((_, &id)) = module_info.exports.types.get_key_value(terminator.lexeme) {
            let res = self.type_scopes.declare_global(target.lexeme.into(), id);
            self.redeclaration_type(res, target.span);

            for ((new_id, name), method_id) in module_info.exports.methods.iter() {
                if *new_id == id {
                    let res = self
                        .type_scopes
                        .declare_method(id, name.clone(), *method_id);
                    self.redeclaration_method(res, target.lexeme, target.span);
                }
            }
        }

        if let Some(ctx) = module_info.exports.vars.get(terminator.lexeme) {
            self.scopes
                .declare_existing(&VariableContext {
                    name: target.lexeme.into(),
                    type_info: ctx.type_info.clone(),
                    original_type: ctx.original_type.clone(),
                    ..*ctx
                })
                .ok_or_report(&mut self.errors);
        }
    }

    pub fn import_all(&mut self, exports: &Exports) {
        for (name, id) in exports.types.iter() {
            let res = self.type_scopes.declare_global(name.clone(), *id);
            self.redeclaration_type(res, self.sys.get_origin(*id).unwrap_or_default())
        }

        for ((id, method_name), method_id) in exports.methods.iter() {
            let res = self
                .type_scopes
                .declare_method(*id, method_name.clone(), *method_id);

            self.redeclaration_method(
                res,
                method_name,
                self.sys.get_method_info(*method_id).origin,
            );
        }

        for var in exports.vars.values() {
            self.scopes
                .declare_existing(var)
                .expect("TODO error handling");
        }
    }
}
