use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Stmt, TypeAst, VariantType};
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{StmtKind, TypedStmt};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::Type;
use crate::typechecker::{FunctionContext, Symbol, TypeChecker};
use std::collections::HashMap;
use std::iter::repeat_n;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_functions(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast.iter() {
            match stmt {
                Stmt::Function {
                    name,
                    params,
                    type_: type_info,
                    body: _,
                    generics,
                } => {
                    self.sys.push_generics(generics);

                    let type_info = Type::from_function_ast(
                        type_info,
                        &self.sys,
                        self.sys.get_active_generics(),
                    );
                    self.sys.pop_n_generics(generics.len());
                    self.declare_function(name.lexeme.into(), type_info, params);
                }
                Stmt::Impl {
                    interfaces,
                    name,
                    methods,
                    generics,
                } => {
                    self.sys.push_generics(generics);
                    for interface in interfaces {
                        let interface_type = self.sys.get_interface(interface.lexeme);
                        if interface_type.is_none() {
                            self.errors.push(TypeCheckerError::UndefinedType {
                                name: interface.lexeme.to_string(),
                                line: interface.line,
                                message: "Interface does not exist.",
                            });
                        }
                    }
                    for method in methods {
                        match method {
                            Stmt::Function {
                                name: func_name,
                                params,
                                type_: type_info,
                                body: _,
                                generics,
                            } => {
                                self.sys.push_generics(generics);
                                let func_type =
                                    Type::from_method_ast(type_info, &name.0, &name.1, &self.sys);
                                let mangled_name: Symbol =
                                    format!("{}.{}", name.0.lexeme, func_name.lexeme).into();
                                self.declare_function(mangled_name, func_type, params);
                                self.sys.pop_n_generics(generics.len());
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.sys.pop_n_generics(generics.len());
                }
                Stmt::Interface {
                    name,
                    methods,
                    generics,
                } => {
                    let mut method_map: HashMap<String, (usize, Type)> = HashMap::new();
                    for (i, sig) in methods.iter().enumerate() {
                        let ty = match Type::from_method_ast(&sig.type_, name, generics, &self.sys)
                        {
                            Ok(t) => t,
                            Err(e) => {
                                self.errors.push(e);
                                Type::Unknown
                            }
                        };
                        method_map.insert(sig.name.lexeme.to_string(), (i, ty));
                    }
                    self.sys.define_interface(name.lexeme, method_map);
                }
                _ => {}
            }
        }
    }

    fn declare_function(
        &mut self,
        name: Symbol,
        func_type: Result<Type, TypeCheckerError>,
        params: &[Token],
    ) {
        if let Err(e) = func_type {
            self.errors.push(e);
            return;
        }
        let func_type = func_type.unwrap().patch_param_names(params);

        let res = self.scopes.declare(name, func_type);

        if let Err(e) = res {
            self.errors.push(e);
        }
    }

    pub(crate) fn declare_global_types(&mut self, ast: &[Stmt<'src>]) {
        // declare global types by name
        for stmt in ast.iter() {
            if let Stmt::Struct { name, generics, .. } = stmt {
                self.sys.declare_struct(name.lexeme.into(), generics);
            } else if let Stmt::Interface { name, .. } = stmt {
                self.sys.declare_interface(name.lexeme.into());
            } else if let Stmt::Enum { name, generics, .. } = stmt {
                self.sys.declare_enum(name.lexeme.into(), generics);
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
                self.sys.push_generics(generics);
                let field_types = self.define_struct_fields(fields);
                self.sys.define_struct(name.lexeme, field_types);
                self.sys.pop_n_generics(generics.len());
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
                self.sys.push_generics(generics);
                let mut typed_variants = HashMap::new();
                for (idx, (v_name, fields)) in variants.iter().enumerate() {
                    let ty = match fields {
                        VariantType::Tuple(tuple_def) => {
                            let res = Type::new_tuple(tuple_def, &self.sys);
                            match res {
                                Ok(ty) => ty,
                                Err(err) => {
                                    self.errors.push(err);
                                    Type::Unknown
                                }
                            }
                        }
                        VariantType::Struct(struct_def) => {
                            let field_types = self.define_struct_fields(struct_def);
                            let full_name: Symbol =
                                format!("{}.{}", name.lexeme, v_name.lexeme).into();
                            // TODO generics here???
                            self.sys.declare_struct(full_name.clone(), &[]);
                            self.sys.define_struct(&full_name, field_types);
                            Type::Struct(full_name, vec![].into())
                        }
                        VariantType::Unit => Type::Void,
                    };

                    typed_variants.insert(v_name.lexeme.to_string(), (idx, ty));
                }
                self.sys.pop_n_generics(generics.len());
                self.sys.define_enum(name.lexeme, typed_variants);
            }
        }
    }
    fn define_struct_fields(
        &mut self,
        fields: &[(Token, TypeAst)],
    ) -> HashMap<String, (usize, Type)> {
        let mut field_types = HashMap::new();
        for (i, (name, type_ast)) in fields.iter().enumerate() {
            let field_type = Type::from_ast(type_ast, &self.sys);
            match field_type {
                Ok(field_type) => {
                    field_types.insert(name.lexeme.to_string(), (i, field_type));
                }
                Err(err) => {
                    // Using Unknown to minimize the number of errors.
                    field_types.insert(name.lexeme.to_string(), (i, Type::Unknown));
                    self.errors.push(err);
                }
            }
        }
        field_types
    }

    pub(crate) fn define_impl(
        &mut self,
        interfaces: &[Token],
        (name, target_generics): &(Token, Vec<Token>),
        methods: &[Stmt<'src>],
        generics: &[Token],
    ) -> Result<TypedStmt, TypeCheckerError> {
        let mut typed_methods = vec![];

        self.sys.push_generics(generics);
        // define methods
        for method in methods {
            match method {
                Stmt::Function {
                    name: func_name,
                    params,
                    body,
                    type_,
                    generics,
                } => {
                    self.sys.push_generics(generics);
                    let type_info =
                        match Type::from_method_ast(type_, name, target_generics, &self.sys) {
                            Ok(t) => t,
                            Err(err) => {
                                self.errors.push(err);
                                self.sys.pop_n_generics(generics.len());
                                continue;
                            }
                        };

                    let primary_mangled = format!("{}.{}", name.lexeme, func_name.lexeme);
                    let typed_method = self.check_function(
                        func_name,
                        params,
                        body,
                        type_info,
                        primary_mangled.into(),
                    );
                    self.sys.pop_n_generics(generics.len());
                    typed_methods.push(typed_method?);
                }
                _ => unreachable!(),
            }
        }
        self.sys.pop_n_generics(generics.len());
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
                    if TypeSystem::implement_method(&resolved_type.type_info, method_type) {
                        vtable[*location] = method_location;
                    } else {
                        missing_methods.push(format!("{method_name} (type mismatch)"));
                    }
                } else {
                    missing_methods.push(method_name.clone());
                }
            }

            if !missing_methods.is_empty() {
                return Err(TypeCheckerError::DoesNotImplementInterface {
                    missing_methods,
                    interface: interface.lexeme.to_string(),
                    line: interface.line,
                });
            }
            self.sys
                .define_impl(name.lexeme, interface_type.name.clone());

            vtables.push(vtable);
        }

        Ok(TypedStmt {
            kind: StmtKind::Impl {
                methods: typed_methods.into(),
                vtables: vtables.into(),
            },
            line: name.line,
            type_info: Type::Void,
        })
    }

    pub(crate) fn check_function(
        &mut self,
        name: &Token<'src>,
        params: &Vec<Token<'src>>,
        body: &Vec<Stmt<'src>>,
        type_: Type,
        full_name: Symbol,
    ) -> Result<TypedStmt, TypeCheckerError> {
        let enclosing_function_context = self.current_function.clone();
        if let Type::Function(func) = &type_ {
            self.current_function = FunctionContext::Function(func.return_type.clone());

            let prev_closures = self.scopes.clear_closures();
            self.scopes.begin_scope(ScopeType::Function);

            self.scopes.declare(name.lexeme.into(), type_.clone())?;
            // Declare parameters.
            for (i, param) in params.iter().enumerate() {
                self.scopes
                    .declare(param.lexeme.into(), func.params[i].1.clone())?;
            }

            let func_body = body
                .iter()
                .map(|stmt| self.check_stmt(stmt))
                .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

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
            Ok(TypedStmt {
                kind: StmtKind::Function {
                    target: func_location,
                    name: Box::from(String::from(name.lexeme)),
                    body: Box::from(TypedStmt {
                        kind: StmtKind::Block {
                            body: func_body,
                            reserved: reserved as u16,
                        },
                        line: name.line,
                        type_info: Type::Void,
                    }),
                    captures: Box::from(captures),
                },
                type_info: type_,
                line: name.line,
            })
        } else {
            unreachable!()
        }
    }
}
