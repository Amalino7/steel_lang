use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{FunctionSig, Stmt, TypeAst, VariantType};
use crate::scanner::{Span, Token};
use crate::typechecker::core::ast::{ExprKind, StmtKind, TypedExpr, TypedStmt};
use crate::typechecker::core::error::{
    DuplicateDefinition, DuplicateKind, Mismatch, Recoverable, TypeCheckerError,
};
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::guards::{ScopeGuard, TypeScopeGuard};
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::system::ImplMethod;
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;
use std::iter::repeat_n;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_functions(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast.iter() {
            match stmt {
                Stmt::Function {
                    name,
                    generics,
                    signature,
                    ..
                } => {
                    let mut guard = TypeScopeGuard::new_function(self, generics);
                    let func_ty = guard
                        .res()
                        .resolve_generic_func(signature)
                        .map(Type::Function);
                    guard.declare_function(name.lexeme.into(), name.span, func_ty, false);
                }
                Stmt::Impl {
                    interfaces,
                    name,
                    methods,
                    generics,
                } => {
                    let impl_gen_count = generics.len();
                    let mut partial_guard = TypeScopeGuard::new_type_params(self, generics);
                    let self_ty = partial_guard
                        .res()
                        .resolve_named(&name.0, &name.1)
                        .recover(&mut partial_guard.errors, Type::Error);

                    let mut guard = TypeScopeGuard::new_impl(&mut partial_guard, self_ty);

                    for interface in interfaces {
                        let interface_type = guard.sys.get_interface(interface.lexeme);
                        if interface_type.is_none() {
                            guard.errors.push(TypeCheckerError::UndefinedType {
                                name: interface.lexeme.to_string(),
                                span: interface.span,
                                message: "Interface does not exist.",
                            });
                        }
                    }

                    for method in methods {
                        let Stmt::Function {
                            name: func_name,
                            signature,
                            generics,
                            ..
                        } = method
                        else {
                            unreachable!();
                        };
                        let mut inner_guard = TypeScopeGuard::new_type_params(&mut guard, generics);
                        let func_ty = inner_guard
                            .res()
                            .resolve_generic_func(signature)
                            .map(Type::Function);
                        let mangled_name: Symbol =
                            format!("{}.{}", name.0.lexeme, func_name.lexeme).into();

                        inner_guard.declare_function(
                            mangled_name.clone(),
                            func_name.span,
                            func_ty,
                            true,
                        );

                        // Register impl metadata so method-access can freshen and unify correctly.
                        let self_type = inner_guard
                            .type_scopes
                            .get_self_type()
                            .cloned()
                            .unwrap_or(Type::Error);
                        inner_guard.sys.register_method(
                            mangled_name,
                            ImplMethod {
                                impl_generic_count: impl_gen_count,
                                self_type,
                            },
                        );
                    }
                }
                Stmt::Interface {
                    name,
                    methods,
                    generics,
                } => {
                    let mut guard = TypeScopeGuard::new_type_params(self, generics);
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
                    guard.sys.define_interface(name.lexeme, method_map);
                }
                _ => {}
            }
        }
    }

    pub(crate) fn define_impl(
        &mut self,
        impl_block: &Stmt<'src>,
        interfaces: &[Token],
        (name, target_generics): &(Token, Vec<TypeAst>),
        methods: &[Stmt<'src>],
        generics: &[Token<'src>],
    ) -> TypedStmt {
        let mut typed_methods = vec![];
        let mut outer_guard = TypeScopeGuard::new_type_params(self, generics);
        let self_ty = outer_guard
            .res()
            .resolve_named(name, target_generics)
            .recover(&mut outer_guard.errors, Type::Error);
        let mut guard = TypeScopeGuard::new_impl(&mut outer_guard, self_ty);
        // define methods
        for method in methods {
            let Stmt::Function {
                name: func_name,
                body,
                signature,
                generics,
            } = method
            else {
                unreachable!();
            };
            let primary_mangled = format!("{}.{}", name.lexeme, func_name.lexeme);

            if let Some(expr) = guard
                .check_function(func_name, signature, body, generics)
                .ok_or_report(&mut guard.errors)
            {
                let (_, location) = guard
                    .scopes
                    .lookup(&primary_mangled)
                    .expect("Function was added");

                let stmt = TypedStmt {
                    kind: StmtKind::Function {
                        name: primary_mangled.into_boxed_str(),
                        target: location,
                        function_decl: expr,
                    },
                    span: Default::default(),
                    type_info: Type::Nil,
                };
                typed_methods.push(stmt)
            }
        }
        drop(guard);
        drop(outer_guard);
        // generate vtables
        let mut vtables = vec![];
        for interface in interfaces {
            if let Some(vtable) =
                self.check_interface_vtable(name.lexeme, interface, impl_block.span())
            {
                vtables.push(vtable);
            }
        }

        TypedStmt {
            kind: StmtKind::Impl {
                methods: typed_methods.into(),
                vtables: vtables.into(),
            },
            span: impl_block.span(),
            type_info: Type::Void,
        }
    }

    pub(crate) fn check_function(
        &mut self,
        name: &Token<'src>,
        sig: &FunctionSig,
        body: &Stmt<'src>,
        generics: &[Token<'src>],
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mut ty_guard = TypeScopeGuard::new_function(self, generics);
        let func = ty_guard.res().resolve_generic_func(sig)?;
        let func_type = Type::Function(func.clone());
        let mut guard =
            ScopeGuard::new_function(&mut ty_guard, func.return_type.clone(), name.span);

        let decl = Declaration::function(name.lexeme.into(), func_type.clone(), name.span);
        guard.scopes.declare(decl)?;

        // Declare parameters. Use ok_or_report so duplicate-param errors don't prevent body checking.
        for (i, (param, _)) in sig.params.iter().enumerate() {
            let param_decl =
                Declaration::parameter(param.lexeme.into(), func.params[i].1.clone(), param.span);
            guard
                .scopes
                .declare(param_decl)
                .ok_or_report(&mut guard.errors);
        }

        let func_body = guard.check_stmt(body);

        let reserved = guard.scopes.max_index();

        let old_closures = guard.scopes.get_closures();
        drop(guard);
        drop(ty_guard);

        let mut captures = vec![];
        for clos_var in old_closures {
            let (_, var_ctx) = self
                .scopes
                .lookup(clos_var.as_ref())
                .expect("Variable should exist in upper scope.");
            captures.push(var_ctx);
        }
        let function_span = name.span.merge(func_body.span);

        Ok(TypedExpr {
            kind: ExprKind::Function {
                signature: name.span.merge(Span {
                    end: func_body.span.start,
                    ..func_body.span
                }),
                body: Box::new(func_body),
                captures: Box::from(captures),
                reserved: reserved as u8,
            },
            ty: func_type,
            span: function_span,
        })
    }

    fn declare_function(
        &mut self,
        name: Symbol,
        span: Span,
        func_type: Result<Type, TypeCheckerError>,
        is_method: bool,
    ) {
        let func_type = func_type.recover(&mut self.errors, Type::Error);
        let decl = match is_method {
            true => Declaration::method(name, func_type, span),
            false => Declaration::global_function(name, func_type, span),
        };
        self.scopes.declare(decl).ok_or_report(&mut self.errors);
    }

    fn check_interface_vtable(
        &mut self,
        type_name: &str,
        interface: &Token,
        impl_span: Span,
    ) -> Option<Vec<ResolvedVar>> {
        let interface_type = self.sys.get_interface(interface.lexeme)?.clone();

        let mut vtable =
            repeat_n(ResolvedVar::Local(0), interface_type.methods.len()).collect::<Vec<_>>();
        let mut missing_methods = vec![];

        for (method_name, (location, method_type)) in interface_type.methods.iter() {
            let impl_method_name = format!("{}.{}", type_name, method_name);

            if let Some((resolved_type, method_location)) = self.scopes.lookup(&impl_method_name) {
                if let Err(err) = self
                    .infer_ctx
                    .unify_types(method_type, &resolved_type.type_info)
                {
                    let full_err = TypeCheckerError::InterfaceMethodTypeMismatch {
                        method_name: method_name.clone(),
                        interface: interface.lexeme.to_string(),
                        type_mismatch: Mismatch::enriched(
                            method_type,
                            &resolved_type.type_info,
                            err,
                            &self.infer_ctx,
                        ),
                        span: resolved_type.span,
                        interface_origin: interface_type.origin,
                    };
                    self.report(full_err);
                } else {
                    vtable[*location] = method_location;
                }
            } else {
                missing_methods.push(method_name.clone());
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
        self.sys.define_impl(type_name, interface_type.name.clone());

        Some(vtable)
    }

    pub(crate) fn declare_global_types(&mut self, ast: &[Stmt<'src>]) {
        // declare global types by name
        for stmt in ast.iter() {
            match stmt {
                Stmt::Struct { name, generics, .. } => {
                    if self.redeclaration_check(name).is_ok() {
                        self.sys
                            .declare_struct(stmt.span(), name.lexeme.into(), generics);
                    }
                }
                Stmt::Interface { name, .. } => {
                    if self.redeclaration_check(name).is_ok() {
                        self.sys.declare_interface(name.lexeme.into(), stmt.span())
                    }
                }
                Stmt::Enum { name, generics, .. } => {
                    if self.redeclaration_check(name).is_ok() {
                        self.sys
                            .declare_enum(stmt.span(), name.lexeme.into(), generics);
                    }
                }
                _ => {}
            }
        }
    }

    fn redeclaration_check(&mut self, name: &Token<'src>) -> Result<(), ()> {
        let primitive_error = TypeCheckerError::PrimitiveTypeShadowing {
            name: name.lexeme.to_string(),
            span: name.span,
        };
        if self.sys.get_primitive_name(name.lexeme).is_some() {
            self.report(primitive_error);
            Err(())
        } else if let Some(original) = self.sys.get_origin(name.lexeme) {
            if original == Span::default() {
                self.report(primitive_error);
            } else {
                self.report(TypeCheckerError::Duplicate(DuplicateDefinition {
                    kind: DuplicateKind::Type,
                    name: name.lexeme.to_string(),
                    span: name.span,
                    original,
                }));
            }
            Err(())
        } else {
            Ok(())
        }
    }

    pub(crate) fn define_global_structs(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast {
            if let Stmt::Struct {
                name,
                fields,
                generics,
            } = stmt
            {
                // Only process structs that were successfully declared (no duplicates)
                if self.sys.get_struct(name.lexeme).is_some() {
                    let mut guard = TypeScopeGuard::new_type_params(self, generics);
                    let field_types = guard.define_struct_fields(fields);
                    guard.sys.define_struct(name.lexeme, field_types);
                }
            }
        }
    }

    pub(crate) fn define_enum_variants(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast {
            if let Stmt::Enum {
                name,
                variants,
                generics,
            } = stmt
            {
                // Only process enums that were successfully declared (no duplicates)
                if self.sys.get_enum(name.lexeme).is_none() {
                    continue;
                }
                let mut guard = TypeScopeGuard::new_type_params(self, generics);
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
                            let full_name: Symbol =
                                format!("{}.{}", name.lexeme, v_name.lexeme).into();

                            guard
                                .sys
                                .declare_struct(v_name.span, full_name.clone(), &[]);
                            guard.sys.define_struct(&full_name, field_types);
                            Type::Struct(full_name, vec![].into())
                        }
                        VariantType::Unit => Type::Void,
                    };

                    typed_variants.insert(sym, (valid_idx, ty));
                    valid_idx += 1;
                }
                guard.sys.define_enum(name.lexeme, typed_variants);
            }
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
}
