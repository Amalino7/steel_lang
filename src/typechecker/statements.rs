use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Stmt;
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{MatchCase, StmtKind, Type, TypedStmt};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::{FunctionContext, Symbol, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_stmt(&mut self, stmt: &Stmt<'src>) -> Result<TypedStmt, TypeCheckerError> {
        match stmt {
            Stmt::Expression(expr) => Ok(TypedStmt {
                kind: StmtKind::Expression(self.infer_expression(expr)?),
                line: stmt.get_line(),
                type_info: Type::Void,
            }),
            Stmt::Let {
                identifier,
                value,
                type_info,
            } => {
                let value_node = self.infer_expression(value)?;
                let declared_type = Type::from_ast(type_info, &self.sys)?;

                if declared_type == Type::Unknown {
                    self.scopes
                        .declare(identifier.lexeme.into(), value_node.ty.clone())?;
                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            target: self.scopes.lookup(identifier.lexeme).unwrap().1,
                            value: value_node,
                        },
                        type_info: Type::Void,
                        line: identifier.line,
                    })
                } else {
                    let coerced_value =
                        self.sys
                            .verify_assignment(&declared_type, value_node, identifier.line)?;

                    self.scopes
                        .declare(identifier.lexeme.into(), declared_type)?;

                    Ok(TypedStmt {
                        kind: StmtKind::Let {
                            target: self.scopes.lookup(identifier.lexeme).unwrap().1,
                            value: coerced_value,
                        },
                        type_info: Type::Void,
                        line: identifier.line,
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

                let variable_count = self.scopes.scope_size() as u8;
                self.scopes.end_scope();

                Ok(TypedStmt {
                    kind: StmtKind::Block {
                        body: stmts,
                        variable_count,
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
                let mut matched_variants = std::collections::HashSet::new();
                let mut typed_cases = vec![];

                for arm in arms {
                    let variant_name = arm.pattern.variant_name.lexeme;
                    let variant_def = enum_def
                        .variants
                        .get(variant_name)
                        .ok_or_else(|| todo!("Add specific error message for missing variant"))?;

                    if matched_variants.contains(variant_name) {
                        // TODO Optional: Error for unreachable pattern
                    }
                    matched_variants.insert(variant_name.to_string());

                    let mut field_vars = vec![];
                    self.scopes.begin_scope(ScopeType::Block);
                    for (i, field_type) in variant_def.1.iter().enumerate() {
                        let bound_name = arm.pattern.captures[i].lexeme;
                        self.scopes.declare(bound_name.into(), field_type.clone())?;
                        field_vars.push(self.scopes.lookup(bound_name).unwrap().1);
                    }

                    let typed_body = self.check_stmt(&arm.body)?;

                    self.scopes.end_scope();

                    let variant_idx = enum_def.variants.get(variant_name).unwrap().0;
                    typed_cases.push(MatchCase {
                        variant_name: variant_name.to_string(),
                        variant_idx,
                        fields: field_vars,
                        body: typed_body,
                    });
                }

                // Exhaustiveness check
                for variant in enum_def.variants.keys() {
                    if !matched_variants.contains(variant) {
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

            self.scopes.end_scope();
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
                            variable_count: 0,
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
