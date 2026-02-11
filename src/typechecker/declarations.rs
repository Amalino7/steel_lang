use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Stmt, TypeAst, VariantType};
use crate::scanner::{Span, Token};
use crate::typechecker::error::{Recoverable, TypeCheckerError};
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{StmtKind, TypedStmt};
use crate::typechecker::type_resolver::TypeScopeGuard;
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::Type;
use crate::typechecker::{FunctionContext, Symbol, TypeChecker};
use std::collections::HashMap;
use std::iter::repeat_n;

impl<'src> TypeChecker<'src> {
    fn reg(&mut self) -> &mut TypeSystem {
        &mut self.resolver.sys
    }
    pub(crate) fn declare_global_functions(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast.iter() {
            match stmt {
                Stmt::Function {
                    name,
                    generics,
                    signature,
                    ..
                } => {
                    let mut guard = TypeScopeGuard::new(self, generics).expect("Invalid generics.");
                    let func_ty = guard.resolver.resolve_func(signature, generics);
                    guard.declare_function(name.lexeme.into(), name.span, func_ty);
                    drop(guard);
                }
                Stmt::Impl {
                    interfaces,
                    name,
                    methods,
                    generics,
                } => {
                    let guard = TypeScopeGuard::new(self, generics).expect("Invalid generics.");
                    let self_ty = guard
                        .resolver
                        .resolve_named(&name.0, &name.1)
                        .expect("Invalid type name.");
                    drop(guard);
                    let mut guard = TypeScopeGuard::new_impl_scope(self, generics, self_ty)
                        .expect("Invalid generics.");

                    for interface in interfaces {
                        let interface_type = guard.reg().get_interface(interface.lexeme);
                        if interface_type.is_none() {
                            guard.errors.push(TypeCheckerError::UndefinedType {
                                name: interface.lexeme.to_string(),
                                span: interface.span,
                                message: "Interface does not exist.",
                            });
                        }
                    }

                    for method in methods {
                        match method {
                            Stmt::Function {
                                name: func_name,
                                signature,
                                body: _,
                                generics,
                            } => {
                                let mut inner_guard = TypeScopeGuard::new(&mut guard, generics)
                                    .expect("Invalid generics.");
                                let func_ty =
                                    inner_guard.resolver.resolve_func(signature, generics);
                                let mangled_name: Symbol =
                                    format!("{}.{}", name.0.lexeme, func_name.lexeme).into();
                                inner_guard.declare_function(mangled_name, func_name.span, func_ty);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Stmt::Interface {
                    name,
                    methods,
                    generics,
                } => {
                    let mut guard = TypeScopeGuard::new_impl_scope(self, generics, Type::Any)
                        .expect("Invalid generics.");
                    let mut method_map: HashMap<String, (usize, Type)> = HashMap::new();
                    for (i, sig) in methods.iter().enumerate() {
                        let ty = guard.resolver.resolve_func(&sig.signature, &*sig.generics);
                        let ty = match ty {
                            Ok(t) => t,
                            Err(e) => {
                                guard.errors.push(e);
                                Type::Error
                            }
                        };
                        method_map.insert(sig.name.lexeme.to_string(), (i, ty));
                    }
                    guard.reg().define_interface(name.lexeme, method_map);
                }
                _ => {}
            }
        }
    }

    fn declare_function(
        &mut self,
        name: Symbol,
        span: Span,
        func_type: Result<Type, TypeCheckerError>,
    ) {
        if let Err(e) = func_type {
            self.errors.push(e);
            return;
        }
        let func_type = func_type.unwrap();

        let res = self.scopes.declare(name, func_type, span);

        if let Err(e) = res {
            self.errors.push(e);
        }
    }

    pub(crate) fn declare_global_types(&mut self, ast: &[Stmt<'src>]) {
        // declare global types by name
        for stmt in ast.iter() {
            if let Stmt::Struct { name, generics, .. } = stmt {
                self.reg()
                    .declare_struct(stmt.span(), name.lexeme.into(), generics);
            } else if let Stmt::Interface { name, .. } = stmt {
                self.reg()
                    .declare_interface(name.lexeme.into(), stmt.span());
            } else if let Stmt::Enum { name, generics, .. } = stmt {
                self.reg()
                    .declare_enum(stmt.span(), name.lexeme.into(), generics);
            }
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
                let mut guard = TypeScopeGuard::new(self, generics).expect("Invalid generics.");
                let field_types = guard.define_struct_fields(fields);
                guard.reg().define_struct(name.lexeme, field_types);
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
                let mut guard = TypeScopeGuard::new(self, generics).expect("Invalid generics.");
                let mut typed_variants = HashMap::new();
                for (idx, (v_name, fields)) in variants.iter().enumerate() {
                    let ty = match fields {
                        VariantType::Tuple(tuple_def) => {
                            let res = guard.resolver.resolve_tuple(tuple_def);
                            match res {
                                Ok(ty) => ty,
                                Err(err) => {
                                    guard.errors.push(err);
                                    Type::Error
                                }
                            }
                        }
                        VariantType::Struct(struct_def) => {
                            let field_types = guard.define_struct_fields(struct_def);
                            let full_name: Symbol =
                                format!("{}.{}", name.lexeme, v_name.lexeme).into();

                            guard
                                .reg()
                                .declare_struct(v_name.span, full_name.clone(), &[]);
                            guard.reg().define_struct(&full_name, field_types);
                            Type::Struct(full_name, vec![].into())
                        }
                        VariantType::Unit => Type::Void,
                    };

                    typed_variants.insert(v_name.lexeme.into(), (idx, ty));
                }
                guard.reg().define_enum(name.lexeme, typed_variants);
            }
        }
    }
    fn define_struct_fields(
        &mut self,
        fields: &[(Token, TypeAst)],
    ) -> HashMap<String, (usize, Type)> {
        let mut field_types = HashMap::new();
        for (i, (name, type_ast)) in fields.iter().enumerate() {
            let field_type = self.resolver.resolve(type_ast);
            match field_type {
                Ok(field_type) => {
                    field_types.insert(name.lexeme.to_string(), (i, field_type));
                }
                Err(err) => {
                    // Parsing error - mark field with Error to avoid treating as undecided.
                    field_types.insert(name.lexeme.to_string(), (i, Type::Error));
                    self.errors.push(err);
                }
            }
        }
        field_types
    }

    pub(crate) fn define_impl(
        &mut self,
        impl_block: &Stmt<'src>,
        interfaces: &[Token],
        (name, target_generics): &(Token, Vec<TypeAst>),
        methods: &[Stmt<'src>],
        generics: &[Token],
    ) -> TypedStmt {
        let mut typed_methods = vec![];
        let guard = TypeScopeGuard::new(self, generics).expect("Invalid generics.");
        let self_ty = guard
            .resolver
            .resolve_named(name, target_generics)
            .expect("Invalid type name.");
        let mut guard =
            TypeScopeGuard::new_impl_scope(self, generics, self_ty).expect("Invalid generics.");
        // define methods
        for method in methods {
            match method {
                Stmt::Function {
                    name: func_name,
                    body,
                    signature,
                    generics,
                } => {
                    let mut guard =
                        TypeScopeGuard::new(&mut guard, generics).expect("Invalid generics.");
                    let type_info = match guard.resolver.resolve_func(signature, generics) {
                        Ok(t) => t,
                        Err(err) => {
                            guard.errors.push(err);
                            continue;
                        }
                    };

                    let primary_mangled = format!("{}.{}", name.lexeme, func_name.lexeme);
                    if let Some(stmt) = guard
                        .check_function(func_name, params, body, type_info, primary_mangled.into())
                        .ok_log(&mut self.errors)
                    {
                        typed_methods.push(stmt)
                    }
                }
                _ => unreachable!(),
            }
        }
        drop(guard);
        // generate vtables
        let mut vtables = vec![];
        for interface in interfaces {
            // check if interface exists
            let Some(interface_type) = self.reg().get_interface(interface.lexeme) else {
                continue;
            };

            let mut vtable =
                repeat_n(ResolvedVar::Local(0), interface_type.methods.len()).collect::<Vec<_>>();
            let mut missing_methods = vec![];

            for (method_name, (location, method_type)) in interface_type.methods.iter() {
                let impl_method_name = format!("{}.{}", name.lexeme, method_name);

                if let Some((resolved_type, method_location)) =
                    self.scopes.lookup(&impl_method_name)
                {
                    // TODO better error report
                    if let Ok(_) = self
                        .infer_ctx
                        .unify_types(&resolved_type.type_info, method_type)
                    {
                        vtable[*location] = method_location;
                    } else {
                        missing_methods.push(format!("{method_name} (type mismatch)"));
                    }
                } else {
                    missing_methods.push(method_name.clone());
                }
            }

            if !missing_methods.is_empty() {
                self.errors
                    .push(TypeCheckerError::DoesNotImplementInterface {
                        missing_methods,
                        interface: interface.lexeme.to_string(),
                        span: impl_block.span(),
                        interface_origin: interface_type.origin,
                    });
            }
            self.reg()
                .define_impl(name.lexeme, interface_type.name.clone());

            vtables.push(vtable);
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
        params: &Vec<Token<'src>>,
        body: &Stmt<'src>,
        type_: Type,
        full_name: Symbol,
    ) -> Result<TypedStmt, TypeCheckerError> {
        let enclosing_function_context = self.current_function.clone();
        if let Type::Function(func) = &type_ {
            self.current_function = FunctionContext::Function(func.return_type.clone(), name.span);

            let prev_closures = self.scopes.clear_closures();
            self.scopes.begin_scope(ScopeType::Function);

            self.scopes
                .declare(name.lexeme.into(), type_.clone(), name.span)?;
            // Declare parameters.
            for (i, param) in params.iter().enumerate() {
                self.scopes
                    .declare(param.lexeme.into(), func.params[i].1.clone(), param.span)?;
            }

            let func_body = self.check_stmt(body);

            let reserved = self.scopes.end_scope();
            self.current_function = enclosing_function_context;

            let (_, func_location) = self
                .scopes
                .lookup(full_name.as_ref())
                .expect("Variable was just added to the scope.");

            let old_closures = self.scopes.return_closures(prev_closures);

            let mut captures = vec![];
            for clos_var in old_closures {
                let (_, var_ctx) = self
                    .scopes
                    .lookup(clos_var.as_ref())
                    .expect("Variable should exist in upper scope.");
                captures.push(var_ctx);
            }
            let function_span = name.span.merge(func_body.span);

            Ok(TypedStmt {
                kind: StmtKind::Function {
                    signature: name.span.merge(Span {
                        end: func_body.span.start,
                        ..func_body.span
                    }),
                    target: func_location,
                    name: Box::from(String::from(name.lexeme)),
                    body: Box::new(func_body),
                    captures: Box::from(captures),
                    reserved: reserved as u8,
                },
                type_info: type_,
                span: function_span,
            })
        } else {
            unreachable!()
        }
    }
}
