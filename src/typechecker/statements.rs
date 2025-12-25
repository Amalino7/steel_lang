use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Binding, Pattern, Stmt};
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{MatchCase, StmtKind, TypedBinding, TypedStmt};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::{TupleType, Type};
use crate::typechecker::{FunctionContext, Symbol, TypeChecker};
use std::collections::HashSet;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_stmt(&mut self, stmt: &Stmt<'src>) -> Result<TypedStmt, TypeCheckerError> {
        match stmt {
            Stmt::Expression(expr) => Ok(TypedStmt {
                kind: StmtKind::Expression(self.infer_expression(expr)?),
                line: stmt.get_line(),
                type_info: Type::Void,
            }),
            Stmt::Let {
                binding,
                value,
                type_info,
            } => {
                let value_node = self.infer_expression(value)?;
                let declared_type = Type::from_ast(type_info, &self.sys)?;

                if declared_type == Type::Unknown {
                    let typed_binding = self.check_binding(binding, &value_node.ty)?;
                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            binding: typed_binding,
                            value: value_node,
                        },
                        type_info: Type::Void,
                        line: binding.get_line(),
                    })
                } else {
                    let coerced_value = self.sys.verify_assignment(
                        &declared_type,
                        value_node,
                        binding.get_line(),
                    )?;

                    let typed_binding = self.check_binding(binding, &declared_type)?;

                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            binding: typed_binding,
                            value: coerced_value,
                        },
                        type_info: Type::Void,
                        line: binding.get_line(),
                    })
                }
            }
            Stmt::Impl {
                interfaces,
                name,
                methods,
            } => {
                let mut typed_methods = vec![];

                // define methods
                for method in methods {
                    match method {
                        Stmt::Function {
                            name: func_name,
                            params,
                            body,
                            type_,
                        } => {
                            let type_info = Type::from_method_ast(type_, name, &self.sys)?;

                            let primary_mangled = format!("{}.{}", name.lexeme, func_name.lexeme);
                            let typed_method = self.check_function(
                                func_name,
                                params,
                                body,
                                type_info,
                                primary_mangled.into(),
                            )?;
                            typed_methods.push(typed_method);
                        }
                        _ => unreachable!(),
                    }
                }
                // generate vtables
                let mut vtables = vec![];
                for interface in interfaces {
                    // check if interface exists
                    let Some(interface_type) = self.sys.get_interface(interface.lexeme) else {
                        continue;
                    };

                    let mut vtable = std::iter::repeat(ResolvedVar::Local(0))
                        .take(interface_type.methods.len())
                        .collect::<Vec<_>>();
                    let mut missing_methods = vec![];

                    for (method_name, (location, method_type)) in interface_type.methods.iter() {
                        let impl_method_name = format!("{}.{}", name.lexeme, method_name);

                        if let Some((resolved_type, method_location)) =
                            self.scopes.lookup(&impl_method_name)
                        {
                            if TypeSystem::implement_method(&resolved_type.type_info, method_type) {
                                vtable[*location] = method_location;
                            } else {
                                missing_methods.push(format!("{} (type mismatch)", method_name));
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
            Stmt::Block { body, brace_token } => {
                self.scopes.begin_scope(ScopeType::Block);
                let stmts = body
                    .iter()
                    .map(|stmt| self.check_stmt(stmt))
                    .collect::<Result<Vec<TypedStmt>, TypeCheckerError>>()?;

                self.scopes.end_scope();

                Ok(TypedStmt {
                    kind: StmtKind::Block {
                        body: stmts,
                        reserved: 0, // Only function scopes have reserved
                    },
                    type_info: Type::Void,
                    line: brace_token.line,
                })
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_typed = self.infer_expression(condition)?;

                if cond_typed.ty != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_typed.ty,
                        line: condition.get_line(),
                        message: "If value must be a boolean.",
                    });
                }

                let refinements = self.analyze_condition(&cond_typed);
                self.scopes.begin_scope(ScopeType::Block);
                for (name, ty) in refinements.true_path.iter() {
                    self.scopes.refine(name, ty.clone());
                }
                let then_branch_typed = self.check_stmt(then_branch)?;
                self.scopes.end_scope();

                let else_branch_typed = if let Some(else_branch) = else_branch {
                    self.scopes.begin_scope(ScopeType::Block);
                    for (name, ty) in refinements.false_path.iter() {
                        self.scopes.refine(name, ty.clone());
                    }
                    let stmt = self.check_stmt(else_branch)?;
                    self.scopes.end_scope();
                    Some(Box::new(stmt))
                } else {
                    None
                };

                // Guard logic
                if self.stmt_returns(&then_branch_typed)? {
                    for (name, ty) in refinements.false_path {
                        self.scopes.refine(&name, ty);
                    }
                }
                if let Some(else_branch_typed) = &else_branch_typed {
                    if self.stmt_returns(&else_branch_typed)? {
                        for (name, ty) in refinements.true_path {
                            self.scopes.refine(&name, ty);
                        }
                    }
                }

                Ok(TypedStmt {
                    kind: StmtKind::If {
                        condition: cond_typed,
                        then_branch: Box::new(then_branch_typed),
                        else_branch: else_branch_typed,
                    },
                    type_info: Type::Void,
                    line: condition.get_line(),
                })
            }
            Stmt::While { condition, body } => {
                let cond_type = self.infer_expression(condition)?;
                let body = self.check_stmt(body)?;
                if cond_type.ty != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_type.ty,
                        line: condition.get_line(),
                        message: "While value must be a boolean.",
                    });
                }

                Ok(TypedStmt {
                    kind: StmtKind::While {
                        condition: cond_type,
                        body: Box::new(body),
                    },
                    type_info: Type::Void,
                    line: condition.get_line(),
                })
            }
            Stmt::Function {
                name,
                params,
                body,
                type_,
            } => {
                let raw_type = Type::from_ast(type_, &self.sys)?;

                let final_type = raw_type.patch_param_names(params);

                if !self.scopes.is_global() {
                    self.scopes
                        .declare(name.lexeme.into(), final_type.clone())?;
                }

                self.check_function(name, params, body, final_type, name.lexeme.into())
            }
            Stmt::Return(expr) => {
                let return_expr = self.infer_expression(expr)?;
                if let FunctionContext::Function(func_return_type) = self.current_function.clone() {
                    let coerced_return = self.sys.verify_assignment(
                        &func_return_type,
                        return_expr,
                        expr.get_line(),
                    )?;

                    Ok(TypedStmt {
                        kind: StmtKind::Return(coerced_return),
                        line: expr.get_line(),
                        type_info: Type::Void,
                    })
                } else {
                    Err(TypeCheckerError::InvalidReturnOutsideFunction {
                        line: expr.get_line(),
                    })
                }
            }

            Stmt::Struct { name, .. } | Stmt::Interface { name, .. } => {
                // structs already defined
                if !self.scopes.is_global() {
                    Err(TypeCheckerError::StructOutsideOfGlobalScope {
                        name: name.lexeme.to_string(),
                        line: name.line,
                    })
                } else {
                    Ok(TypedStmt {
                        kind: StmtKind::StructDecl {},
                        line: name.line,
                        type_info: Type::Void,
                    })
                }
            }
            Stmt::Enum { name, .. } => {
                if !self.scopes.is_global() {
                    return Err(TypeCheckerError::StructOutsideOfGlobalScope {
                        name: name.lexeme.to_string(),
                        line: name.line,
                    });
                }

                Ok(TypedStmt {
                    kind: StmtKind::EnumDecl {},
                    line: name.line,
                    type_info: Type::Void,
                })
            }
            Stmt::Match { value, arms } => {
                let value_typed = self.infer_expression(value)?;
                let enum_name = match &value_typed.ty {
                    Type::Enum(name) => name,
                    _ => {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Enum("Any".into()),
                            found: value_typed.ty,
                            line: value_typed.line,
                            message: "Match value must be an enum type.",
                        });
                    }
                };

                let enum_def = self.sys.get_enum(enum_name).unwrap().clone();
                let mut matched_variants = HashSet::new();
                let mut typed_cases = vec![];

                for arm in arms {
                    let Pattern::Named {
                        enum_name,
                        variant_name,
                        bind,
                    } = &arm.pattern
                    else {
                        todo!()
                    };

                    let (variant_idx, field_types) = enum_def
                        .variants
                        .get(variant_name.lexeme)
                        .ok_or_else(|| TypeCheckerError::UndefinedField {
                            struct_name: enum_def.name.to_string(),
                            field_name: variant_name.lexeme.to_string(),
                            line: variant_name.line,
                        })?;

                    if matched_variants.contains(variant_name.lexeme) {
                        // TODO Optional: Error for unreachable pattern
                        // return Err(TypeCheckerError::Generic {
                        //     message: format!("Duplicate match arm for '{}'", variant_name),
                        //     line: variant_name_token.line
                        // });
                    }
                    matched_variants.insert(variant_name.lexeme);

                    let payload_type = field_types;

                    self.scopes.begin_scope(ScopeType::Block);
                    let typed_binding = if let Some(binding) = &bind {
                        self.check_binding(binding, &payload_type)?
                    } else {
                        if !matches!(payload_type, Type::Void) {}
                        TypedBinding::Ignored
                    };

                    // E. Check the Body
                    let typed_body = self.check_stmt(&arm.body)?;

                    self.scopes.end_scope();

                    typed_cases.push(MatchCase {
                        variant_name: variant_name.lexeme.to_string(),
                        variant_idx: *variant_idx,
                        binding: typed_binding,
                        body: typed_body,
                    });
                }

                // Exhaustiveness check
                for variant in enum_def.variants.keys() {
                    if !matched_variants.contains(variant.as_str()) {
                        return Err(TypeCheckerError::UncoveredPattern {
                            variant: variant.clone(),
                            line: value_typed.line,
                        });
                    }
                }

                Ok(TypedStmt {
                    kind: StmtKind::Match {
                        value: Box::new(value_typed),
                        cases: typed_cases,
                    },
                    type_info: Type::Void,
                    line: value.get_line(),
                })
            }
        }
    }
    fn check_binding(
        &mut self,
        binding: &Binding,
        type_to_match: &Type,
    ) -> Result<TypedBinding, TypeCheckerError> {
        match binding {
            Binding::Struct { name, fields } => {
                if let Type::Struct(struct_name) = type_to_match {
                    if !(name.lexeme == struct_name.as_ref()
                        || struct_name.contains(".") && struct_name.ends_with(name.lexeme))
                    // Allows match variants
                    {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Struct(name.lexeme.into()),
                            found: type_to_match.clone(),
                            line: binding.get_line(),
                            message: "Can only use structure destructure syntax on structs",
                        });
                    }
                    let Some(struct_def) = self.sys.get_struct(struct_name.as_ref()) else {
                        return Err(TypeCheckerError::UndefinedType {
                            name: name.lexeme.to_string(),
                            line: binding.get_line(),
                            message: "Struct with that name wasn't found.",
                        });
                    };
                    let mut bindings = vec![];
                    for (field_name, binding) in fields {
                        let Some(&field_idx) = struct_def.fields.get(field_name.lexeme) else {
                            return Err(TypeCheckerError::UndefinedField {
                                struct_name: struct_def.name.to_string(),
                                field_name: field_name.lexeme.to_string(),
                                line: field_name.line,
                            });
                        };
                        let (_, field_type) = struct_def.ordered_fields[field_idx].clone();

                        bindings.push((field_idx, field_type, binding));
                    }
                    let mut typed_fields = vec![];
                    for (idx, ty, binding) in bindings {
                        let typed_binding = self.check_binding(binding, &ty)?;
                        typed_fields.push((idx as u8, typed_binding))
                    }

                    Ok(TypedBinding::Struct(typed_fields))
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Struct("Any".into()),
                        found: type_to_match.clone(),
                        line: binding.get_line(),
                        message: "Can only use structure destructure syntax on structs",
                    })
                }
            }
            Binding::Tuple { fields } => {
                if let Type::Tuple(tuple) = type_to_match {
                    if tuple.types.len() != fields.len() {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Tuple(Rc::new(TupleType {
                                types: vec![Type::Any; fields.len()],
                            })),
                            found: type_to_match.clone(),
                            line: binding.get_line(),
                            message: "Wrong number of tuple destructure.",
                        });
                    }
                    let mut bindings = vec![];
                    for (i, field) in fields.iter().enumerate() {
                        bindings.push(self.check_binding(field, &tuple.types[i])?);
                    }
                    Ok(TypedBinding::Tuple(bindings))
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Tuple(Rc::new(TupleType {
                            types: vec![Type::Any; fields.len()],
                        })),
                        found: type_to_match.clone(),
                        line: binding.get_line(),
                        message: "Can only use tuple destructure on tuples.",
                    })
                }
            }
            Binding::Variable(token) => {
                if token.lexeme == "_" {
                    return Ok(TypedBinding::Ignored);
                }
                self.scopes
                    .declare(token.lexeme.into(), type_to_match.clone())?;
                let (_, resolved) = self.scopes.lookup(token.lexeme).unwrap();
                Ok(TypedBinding::Variable(resolved))
            }
        }
    }

    fn check_function(
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
