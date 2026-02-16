use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{FunctionSig, Stmt, TypeAst, VariantType};
use crate::scanner::{Span, Token};
use crate::typechecker::core::ast::{ExprKind, StmtKind, TypedExpr, TypedStmt};
use crate::typechecker::core::error::{Recoverable, TypeCheckerError};
use crate::typechecker::core::types::Type;
use crate::typechecker::resolver::convert_generics;
use crate::typechecker::scope::guards::{ScopeGuard, TypeScopeGuard};
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::scope::FunctionContext;
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
                    let mut guard = TypeScopeGuard::new_function(self, &convert_generics(generics));
                    let func_ty = guard
                        .res()
                        .resolve_func(signature, guard.type_scopes.all_generics())
                        .map(Type::Function);
                    guard.declare_function(name.lexeme.into(), name.span, func_ty);
                }
                Stmt::Impl {
                    interfaces,
                    name,
                    methods,
                    generics,
                } => {
                    let mut partial_guard =
                        TypeScopeGuard::new_type_params(self, &convert_generics(generics));
                    let self_ty = partial_guard
                        .res()
                        .resolve_named(&name.0, &name.1)
                        .expect("Invalid type name.");

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
                        match method {
                            Stmt::Function {
                                name: func_name,
                                signature,
                                body: _,
                                generics,
                            } => {
                                let mut inner_guard = TypeScopeGuard::new_type_params(
                                    &mut guard,
                                    &convert_generics(generics),
                                );
                                let func_ty = inner_guard
                                    .res()
                                    .resolve_func(signature, inner_guard.type_scopes.all_generics())
                                    .map(Type::Function);
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
                    let mut guard =
                        TypeScopeGuard::new_type_params(self, &convert_generics(generics));
                    let mut guard = TypeScopeGuard::new_impl(&mut guard, Type::Any);
                    let mut method_map: HashMap<String, (usize, Type)> = HashMap::new();
                    for (i, sig) in methods.iter().enumerate() {
                        let ty = guard
                            .res()
                            .resolve_func(&sig.signature, guard.type_scopes.all_generics())
                            .map(Type::Function);
                        let ty = match ty {
                            Ok(t) => t,
                            Err(e) => {
                                guard.errors.push(e);
                                Type::Error
                            }
                        };
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
        let mut outer_guard = TypeScopeGuard::new_type_params(self, &convert_generics(generics));
        let self_ty = outer_guard
            .res()
            .resolve_named(name, target_generics)
            .expect("Invalid type name.");
        let mut guard = TypeScopeGuard::new_impl(&mut outer_guard, self_ty);
        // define methods
        for method in methods {
            match method {
                Stmt::Function {
                    name: func_name,
                    body,
                    signature,
                    generics,
                } => {
                    let primary_mangled = format!("{}.{}", name.lexeme, func_name.lexeme);

                    if let Some(expr) = guard
                        .check_function(func_name, signature, body, generics)
                        .ok_log(&mut guard.errors)
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
                _ => unreachable!(),
            }
        }
        drop(guard);
        drop(outer_guard);
        // generate vtables
        let mut vtables = vec![];
        for interface in interfaces {
            // check if interface exists
            let Some(interface_type) = self.sys.get_interface(interface.lexeme) else {
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
                    if self
                        .infer_ctx
                        .unify_types(&resolved_type.type_info, method_type)
                        .is_ok()
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
            self.sys
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
        sig: &FunctionSig,
        body: &Stmt<'src>,
        generics: &[Token<'src>],
    ) -> Result<TypedExpr, TypeCheckerError> {
        let enclosing_function_context = self.current_function.clone();
        let mut ty_guard = TypeScopeGuard::new_function(self, &convert_generics(generics));

        let func = ty_guard
            .res()
            .resolve_func(sig, ty_guard.type_scopes.all_generics())?;
        let func_type = Type::Function(func.clone());
        let mut guard = ScopeGuard::new(&mut ty_guard, ScopeKind::Function);

        guard.current_function = FunctionContext::Function(func.return_type.clone(), name.span);

        let prev_closures = guard.scopes.clear_closures();

        let decl = Declaration::function(name.lexeme.into(), func_type.clone(), name.span);
        guard.scopes.declare(decl)?;

        // Declare parameters.
        for (i, (param, _)) in sig.params.iter().enumerate() {
            let param_decl =
                Declaration::parameter(param.lexeme.into(), func.params[i].1.clone(), param.span);
            guard.scopes.declare(param_decl)?;
        }

        let func_body = guard.check_stmt(body);

        let reserved = guard.scopes.max_index();
        drop(guard);
        drop(ty_guard);
        self.current_function = enclosing_function_context;

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
    ) {
        if let Err(e) = func_type {
            self.errors.push(e);
            return;
        }
        let func_type = func_type.unwrap();
        let decl = Declaration::global_function(name, func_type, span);
        let res = self.scopes.declare(decl);

        if let Err(e) = res {
            self.errors.push(e);
        }
    }

    pub(crate) fn declare_global_types(&mut self, ast: &[Stmt<'src>]) {
        // declare global types by name
        for stmt in ast.iter() {
            if let Stmt::Struct { name, generics, .. } = stmt {
                self.sys
                    .declare_struct(stmt.span(), name.lexeme.into(), generics);
            } else if let Stmt::Interface { name, .. } = stmt {
                self.sys.declare_interface(name.lexeme.into(), stmt.span());
            } else if let Stmt::Enum { name, generics, .. } = stmt {
                self.sys
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
                let mut guard = TypeScopeGuard::new_type_params(self, &convert_generics(generics));
                let field_types = guard.define_struct_fields(fields);
                guard.sys.define_struct(name.lexeme, field_types);
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
                let mut guard = TypeScopeGuard::new_type_params(self, &convert_generics(generics));
                let mut typed_variants = HashMap::new();
                for (idx, (v_name, fields)) in variants.iter().enumerate() {
                    let ty = match fields {
                        VariantType::Tuple(tuple_def) => {
                            let res = guard.res().resolve_tuple(tuple_def);
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
                                .sys
                                .declare_struct(v_name.span, full_name.clone(), &[]);
                            guard.sys.define_struct(&full_name, field_types);
                            Type::Struct(full_name, vec![].into())
                        }
                        VariantType::Unit => Type::Void,
                    };

                    typed_variants.insert(v_name.lexeme.into(), (idx, ty));
                }
                guard.sys.define_enum(name.lexeme, typed_variants);
            }
        }
    }

    fn define_struct_fields(
        &mut self,
        fields: &[(Token, TypeAst)],
    ) -> HashMap<Symbol, (usize, Type)> {
        let mut field_types = HashMap::new();
        for (i, (name, type_ast)) in fields.iter().enumerate() {
            let field_type = self.res().resolve(type_ast);
            match field_type {
                Ok(field_type) => {
                    field_types.insert(name.lexeme.into(), (i, field_type));
                }
                Err(err) => {
                    // Parsing error - mark field with Error to avoid treating as undecided.
                    field_types.insert(name.lexeme.into(), (i, Type::Error));
                    self.errors.push(err);
                }
            }
        }
        field_types
    }
}
