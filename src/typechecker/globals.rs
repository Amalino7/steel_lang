use crate::parser::ast::{Stmt, TypeAst, VariantType};
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::types::Type;
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_functions(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast.iter() {
            match stmt {
                Stmt::Function {
                    name,
                    params,
                    type_: type_info,
                    body: _,
                } => {
                    let type_info = Type::from_ast(type_info, &self.sys);
                    self.declare_function(name.lexeme.into(), type_info, params);
                }
                Stmt::Impl {
                    interfaces,
                    name,
                    methods,
                } => {
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
                            } => {
                                let func_type = Type::from_method_ast(type_info, name, &self.sys);
                                let mangled_name: Symbol =
                                    format!("{}.{}", name.lexeme, func_name.lexeme).into();
                                self.declare_function(mangled_name, func_type, params);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Stmt::Interface { name, methods } => {
                    let mut method_map: HashMap<String, (usize, Type)> = HashMap::new();
                    for (i, sig) in methods.iter().enumerate() {
                        let ty = match Type::from_method_ast(&sig.type_, name, &self.sys) {
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
            if let Stmt::Struct { name, .. } = stmt {
                self.sys.declare_struct(name.lexeme.into());
            } else if let Stmt::Interface { name, .. } = stmt {
                self.sys.declare_interface(name.lexeme.into());
            } else if let Stmt::Enum { name, .. } = stmt {
                self.sys.declare_enum(name.lexeme.into());
            }
        }
    }

    pub(crate) fn define_global_structs(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast {
            if let Stmt::Struct { name, fields } = stmt {
                let field_types = self.define_struct_fields(fields);
                self.sys.define_struct(name.lexeme, field_types);
            }
        }
    }

    pub(crate) fn define_enum_variants(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast {
            if let Stmt::Enum { name, variants } = stmt {
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
                            self.sys.declare_struct(full_name.clone());
                            self.sys.define_struct(&full_name, field_types);
                            Type::Struct(full_name)
                        }
                        VariantType::Unit => Type::Void,
                    };

                    typed_variants.insert(v_name.lexeme.to_string(), (idx, ty));
                }

                self.sys.define_enum(name.lexeme.into(), typed_variants);
            }
        }
    }
    fn define_struct_fields(
        &mut self,
        fields: &[(Token, TypeAst)],
    ) -> HashMap<String, (usize, Type)> {
        let mut field_types = HashMap::new();
        for (i, (name, type_ast)) in fields.into_iter().enumerate() {
            let field_type = Type::from_ast(&type_ast, &self.sys);
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
}
