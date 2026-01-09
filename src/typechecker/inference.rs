use crate::typechecker::types::{TupleType, Type};
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::rc::Rc;

pub struct InferenceContext {
    substitutions: HashMap<u32, Type>,
    next_id: u32, // TODO is this a good idea
}
impl Default for InferenceContext {
    fn default() -> Self {
        Self::new()
    }
}
impl InferenceContext {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn new_type_var(&mut self) -> Type {
        let id = self.next_id;
        self.next_id += 1;
        Type::Infer(id)
    }
    pub fn is_resolved(&self, id: u32) -> bool {
        self.substitutions.contains_key(&id)
    }

    pub fn substitute(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Infer(id) => {
                if let Some(resolved) = self.substitutions.get(id) {
                    let resolved = resolved.clone();
                    self.substitute(&resolved)
                } else {
                    ty.clone()
                }
            }
            Type::Optional(inner) => Type::Optional(Box::new(self.substitute(inner))),
            Type::Function(func_type) => {
                let params = func_type
                    .params
                    .iter()
                    .map(|(name, param_ty)| (name.clone(), self.substitute(param_ty)))
                    .collect();
                let return_type = self.substitute(&func_type.return_type);
                Type::new_function(params, return_type, func_type.type_params.clone())
            }
            Type::Tuple(tuple_type) => {
                let types = tuple_type
                    .types
                    .iter()
                    .map(|t| self.substitute(t))
                    .collect();
                Type::Tuple(Rc::new(TupleType { types }))
            }
            Type::Struct(name, args) => {
                let resolved_args = args.iter().map(|t| self.substitute(t)).collect();
                Type::Struct(name.clone(), Rc::new(resolved_args))
            }
            Type::Interface(name, args) => {
                let resolved_args = args.iter().map(|t| self.substitute(t)).collect();
                Type::Interface(name.clone(), Rc::new(resolved_args))
            }
            Type::Enum(name, args) => {
                let resolved_args = args.iter().map(|t| self.substitute(t)).collect();
                Type::Enum(name.clone(), Rc::new(resolved_args))
            }
            Type::Metatype(_, _)
            | Type::Nil
            | Type::Number
            | Type::String
            | Type::Boolean
            | Type::Void
            | Type::Unknown
            | Type::Never
            | Type::Any
            | Type::GenericParam(_) => ty.clone(),
        }
    }

    pub(crate) fn unify_types(
        &mut self,
        expected_ty: &Type,
        provided: &Type,
    ) -> Result<(), String> {
        fn mismatch(expected: &Type, found: &Type) -> Result<(), String> {
            Err(format!(
                "Type mismatch: expected {}, found {}",
                expected, found
            ))
        }

        fn unify_complex(
            expected_name: &Symbol,
            expected_generics: &[Type],
            provided_name: &Symbol,
            provided_generics: &[Type],
            ts: &mut InferenceContext,
        ) -> Result<(), String> {
            if expected_name != provided_name {
                return Err(format!(
                    "Type mismatch: expected {}, found {}",
                    expected_name, provided_name
                ));
            }
            expected_generics
                .iter()
                .zip(provided_generics.iter())
                .map(|(e, p)| ts.unify_types(e, p))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(())
        }
        if provided == &Type::Never {
            return Ok(());
        }

        if !matches!(expected_ty, Type::Infer(_))
            && let Type::Infer(id) = provided
        {
            return if !self.is_resolved(*id) {
                self.substitutions.insert(*id, expected_ty.clone());
                Ok(())
            } else {
                let prov = self.substitutions.get(id).unwrap().clone();
                self.unify_types(expected_ty, &prov)
            };
        }

        match expected_ty {
            Type::Infer(id) => {
                if !self.is_resolved(*id) {
                    self.substitutions.insert(*id, provided.clone());
                    Ok(())
                } else {
                    let expected = self.substitutions.get(id).unwrap().clone();
                    self.unify_types(&expected, provided)
                }
            }
            Type::Metatype(_, _) => Err("Cannot unify metatypes".into()),
            Type::Nil | Type::Number | Type::String | Type::Void | Type::Boolean | Type::Never => {
                if expected_ty == provided {
                    Ok(())
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Unknown | Type::Any => Ok(()),
            Type::Optional(expected) => {
                if provided == &Type::Nil {
                    Ok(())
                } else if let Type::Optional(provided) = provided {
                    self.unify_types(expected, provided)
                } else {
                    self.unify_types(expected, provided)
                }
            }
            Type::Function(expected_inner) => {
                if let Type::Function(provided_inner) = provided {
                    if provided_inner.is_vararg {
                        return Err("Cannot use vararg functions as arguments".into());
                    }
                    if !provided_inner.type_params.is_empty() {
                        return Err("Cannot assign a generic function.\n TIP: specify generics using .<Type> notation.".into());
                    }

                    for (expected_param, provided_param) in expected_inner
                        .params
                        .iter()
                        .zip(provided_inner.params.iter())
                    {
                        self.unify_types(&expected_param.1, &provided_param.1)?;
                    }
                    self.unify_types(&expected_inner.return_type, &provided_inner.return_type)
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Tuple(expected_inner) => {
                if let Type::Tuple(provided_inner) = provided {
                    for (expected_param, provided_param) in
                        expected_inner.types.iter().zip(provided_inner.types.iter())
                    {
                        self.unify_types(expected_param, provided_param)?;
                    }
                    Ok(())
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::GenericParam(name) => {
                if let Type::GenericParam(provided_name) = provided {
                    if name == provided_name {
                        Ok(())
                    } else {
                        mismatch(expected_ty, provided)
                    }
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Struct(name, args) => {
                if let Type::Struct(provided_name, provided_args) = provided {
                    unify_complex(name, args, provided_name, provided_args, self)
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Interface(name, args) => {
                if let Type::Interface(provided_name, provided_args) = provided {
                    unify_complex(name, args, provided_name, provided_args, self)
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Enum(name, args) => {
                if let Type::Enum(provided_name, provided_args) = provided {
                    unify_complex(name, args, provided_name, provided_args, self)
                } else {
                    mismatch(expected_ty, provided)
                }
            }
        }
    }
}
