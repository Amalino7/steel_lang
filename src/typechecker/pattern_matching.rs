use crate::parser::ast::{Binding, Expr, MatchArm, Pattern};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeType;
use crate::typechecker::type_ast::{MatchCase, StmtKind, TypedBinding, TypedExpr, TypedStmt};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::{generics_to_map, EnumType, TupleType, Type};
use crate::typechecker::TypeChecker;
use std::collections::HashSet;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_match_stmt(
        &mut self,
        value: &Expr<'src>,
        arms: &[MatchArm<'src>],
    ) -> Result<TypedStmt, TypeCheckerError> {
        let value_typed = self.check_expression(value, None)?;
        let enum_name = match &value_typed.ty {
            Type::Enum(name, ..) => name,
            _ => {
                return Err(TypeCheckerError::TypeMismatch {
                    expected: Type::Enum("Any".into(), vec![].into()),
                    found: value_typed.ty,
                    line: value_typed.line,
                    message: "Match value must be an enum type.",
                });
            }
        };

        let enum_def = self.sys.get_enum(enum_name).unwrap().clone();
        let mut matched_variants = HashSet::new();
        let mut typed_cases = vec![];

        let mut has_fallthrough = false;
        for arm in arms {
            let res = self.handle_match_arm(
                &value_typed,
                &enum_def,
                &mut matched_variants,
                &mut typed_cases,
                &mut has_fallthrough,
                arm,
            );
            if let Err(err) = res {
                self.errors.push(err);
            }
        }

        // Exhaustiveness check
        if !has_fallthrough {
            for variant in enum_def.variants.keys() {
                if !matched_variants.contains(variant.as_str()) {
                    return Err(TypeCheckerError::UncoveredPattern {
                        variant: variant.clone(),
                        line: value_typed.line,
                    });
                }
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
    fn handle_match_arm(
        &mut self,
        value_typed: &TypedExpr,
        enum_def: &EnumType,
        matched_variants: &mut HashSet<&'src str>,
        typed_cases: &mut Vec<MatchCase>,
        has_fallthrough: &mut bool,
        arm: &MatchArm<'src>,
    ) -> Result<(), TypeCheckerError> {
        if *has_fallthrough {
            return Err(TypeCheckerError::UnreachablePattern {
                line: arm.body.get_line(),
                message: "Default case must be the last arm.".to_string(),
            });
        }

        match &arm.pattern {
            Pattern::Named {
                enum_name,
                variant_name,
                bind,
            } => {
                if let Some(explicit_name) = enum_name
                    && explicit_name.lexeme != enum_def.name.as_ref()
                {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Enum(enum_def.name.clone(), vec![].into()),
                        found: value_typed.ty.clone(),
                        line: value_typed.line,
                        message: "Match value must be an enum of the same type as the enum being matched against.",
                    });
                }

                let (variant_idx, field_types) = enum_def
                    .get_variant(variant_name.lexeme)
                    .ok_or_else(|| TypeCheckerError::UndefinedField {
                        struct_name: enum_def.name.to_string(),
                        field_name: variant_name.lexeme.to_string(),
                        line: variant_name.line,
                    })?;

                if matched_variants.contains(variant_name.lexeme) {
                    return Err(TypeCheckerError::UnreachablePattern {
                        line: arm.body.get_line(),
                        message: format!("Repeated pattern {}", variant_name.lexeme),
                    });
                }
                matched_variants.insert(variant_name.lexeme);

                let payload_type = field_types;

                self.scopes.begin_scope(ScopeType::Block);
                let typed_binding = if let Some(binding) = &bind {
                    self.check_binding(binding, &payload_type)?
                } else {
                    if !matches!(payload_type, Type::Void) {
                        // TODO potentially warn about unused bindings
                        // self.warnings.push(TypeCheckerWarning::UnusedBinding {
                        //     binding: binding.lexeme.to_string(),
                        //     line: binding.line,
                        // });
                    }
                    TypedBinding::Ignored
                };

                let typed_body = self.check_stmt(&arm.body)?;

                self.scopes.end_scope();

                typed_cases.push(MatchCase::Named {
                    variant_idx: variant_idx as u16,
                    binding: typed_binding,
                    body: typed_body,
                });
                Ok(())
            }
            Pattern::Variable(name) => {
                self.scopes.begin_scope(ScopeType::Block);
                let typed_binding =
                    self.check_binding(&Binding::Variable(name.clone()), &value_typed.ty)?;
                let typed_body = self.check_stmt(&arm.body)?;
                self.scopes.end_scope();
                *has_fallthrough = true;
                typed_cases.push(MatchCase::Variable {
                    binding: typed_binding,
                    body: typed_body,
                });
                Ok(())
            }
        }
    }

    pub(crate) fn check_binding(
        &mut self,
        binding: &Binding,
        type_to_match: &Type,
    ) -> Result<TypedBinding, TypeCheckerError> {
        match binding {
            Binding::Struct { name, fields } => {
                if let Type::Struct(struct_name, generics) = type_to_match {
                    if !(name.lexeme == struct_name.as_ref()
                        || struct_name.contains(".") && struct_name.ends_with(name.lexeme))
                    // Allows match variants
                    {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Struct(name.lexeme.into(), vec![].into()),
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
                        let map = generics_to_map(&struct_def.generic_params, generics);
                        let field_type = TypeSystem::generic_to_concrete(field_type.clone(), &map);

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
                        expected: Type::Struct("Any".into(), vec![].into()),
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
}
