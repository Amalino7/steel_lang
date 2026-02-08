use crate::typechecker::types::{FunctionType, TupleType, Type};
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct InferenceContext {
    substitutions: HashMap<u32, Type>,
    next_id: u32,
}
impl Default for InferenceContext {
    fn default() -> Self {
        Self::new()
    }
}
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}

impl Variance {
    pub fn compose(self, other: Variance) -> Variance {
        match (self, other) {
            (Variance::Covariant, v) => v,
            (Variance::Contravariant, Variance::Covariant) => Variance::Contravariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Covariant,
            (Variance::Contravariant, Variance::Invariant) => Variance::Invariant,
            (Variance::Invariant, _) => Variance::Invariant,
        }
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

    fn resolve(&self, id: u32) -> Option<&Type> {
        self.substitutions.get(&id)
    }

    fn bind(&mut self, id: u32, ty: Type) -> Result<(), UnificationError> {
        if self.occurs_in(id, &ty) {
            return Err(UnificationError {
                kind: UnificationErrorKind::OccursCheck,
                expected: Type::Infer(id),
                found: ty,
            });
        }
        self.substitutions.insert(id, ty);
        Ok(())
    }

    fn occurs_in(&self, id: u32, ty: &Type) -> bool {
        match ty {
            Type::Infer(other_id) => {
                if id == *other_id {
                    return true;
                }
                if let Some(resolved) = self.resolve(*other_id) {
                    return self.occurs_in(id, resolved);
                }
                false
            }
            Type::Optional(inner) | Type::List(inner) => self.occurs_in(id, inner),
            Type::Map(key, value) => self.occurs_in(id, key) || self.occurs_in(id, value),
            Type::Function(func_type) => {
                func_type
                    .params
                    .iter()
                    .any(|(_, param_ty)| self.occurs_in(id, param_ty))
                    || self.occurs_in(id, &func_type.return_type)
            }
            Type::Tuple(tuple_type) => tuple_type.types.iter().any(|t| self.occurs_in(id, t)),
            Type::Struct(_, args) | Type::Interface(_, args) | Type::Enum(_, args) => {
                args.iter().any(|arg| self.occurs_in(id, arg))
            }
            _ => false,
        }
    }

    pub fn substitute(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Error => ty.clone(),
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
            Type::List(inner) => Type::List(Box::new(self.substitute(inner))),
            Type::Map(key, value) => Type::Map(
                Box::new(self.substitute(key)),
                Box::new(self.substitute(value)),
            ),
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
    ) -> Result<(), UnificationError> {
        println!("Unifying {:?} with {:?}", expected_ty, provided);
        self.unify_with_variance(expected_ty, provided, Variance::Covariant)
    }

    fn unify_with_variance(
        &mut self,
        expected: &Type,
        provided: &Type,
        variance: Variance,
    ) -> Result<(), UnificationError> {
        let mismatch = |kind| UnificationError {
            kind,
            expected: expected.clone(),
            found: provided.clone(),
        };

        if variance == Variance::Contravariant && self.is_trivially_unifiable(provided, expected) {
            return Ok(());
        }

        if self.is_trivially_unifiable(expected, provided) {
            return Ok(());
        }

        match (expected, provided) {
            (Type::Infer(id), _) => self.unify_infer_left(*id, provided),
            (_, Type::Infer(id)) => self.unify_infer_right(expected, *id, variance),
            (Type::Optional(expected_inner), Type::Optional(provided_inner)) => {
                self.unify_with_variance(expected_inner, provided_inner, variance)
            }
            (Type::Optional(expected_inner), provided) if variance == Variance::Covariant => {
                self.unify_with_variance(expected_inner, provided, variance)
            }
            (expected, Type::Optional(provided_inner)) if variance == Variance::Contravariant => {
                self.unify_with_variance(expected, provided_inner, variance)
            }
            (Type::List(expected_inner), Type::List(provided_inner)) => {
                self.unify_with_variance(expected_inner, provided_inner, Variance::Invariant)
            }
            (Type::Map(expected_key, expected_value), Type::Map(provided_key, provided_value)) => {
                self.unify_with_variance(expected_key, provided_key, Variance::Invariant)?;
                self.unify_with_variance(expected_value, provided_value, Variance::Invariant)
            }
            (Type::Function(exp), Type::Function(prov)) => {
                self.unify_functions(exp, prov, variance, mismatch)
            }
            (Type::Tuple(exp), Type::Tuple(prov)) => self.unify_tuples(exp, prov, mismatch),
            (Type::Metatype(_, _), _) => Err(UnificationError {
                kind: UnificationErrorKind::MetatypeNotUnifiable,
                expected: expected.clone(),
                found: provided.clone(),
            }),
            (Type::Struct(exp_name, exp_args), Type::Struct(prov_name, prov_args)) => {
                self.unify_complex(exp_name, exp_args, prov_name, prov_args, variance, mismatch)
            }
            (Type::Interface(exp_name, exp_args), Type::Interface(prov_name, prov_args)) => {
                self.unify_complex(exp_name, exp_args, prov_name, prov_args, variance, mismatch)
            }
            (Type::Enum(exp_name, exp_args), Type::Enum(prov_name, prov_args)) => {
                self.unify_complex(exp_name, exp_args, prov_name, prov_args, variance, mismatch)
            }
            (Type::GenericParam(exp_name), Type::GenericParam(prov_name)) => {
                if exp_name == prov_name {
                    Ok(())
                } else {
                    Err(type_mismatch(&expected, &provided))
                }
            }
            _ if expected == provided => Ok(()),
            _ => Err(type_mismatch(expected, provided)),
        }
    }

    fn unify_complex(
        &mut self,
        expected_name: &Symbol,
        expected_args: &[Type],
        provided_name: &Symbol,
        provided_args: &[Type],
        _variance: Variance,
        mismatch: impl FnOnce(UnificationErrorKind) -> UnificationError,
    ) -> Result<(), UnificationError> {
        if expected_name != provided_name {
            return Err(mismatch(UnificationErrorKind::TypeMismatch));
        }

        if expected_args.len() != provided_args.len() {
            return Err(mismatch(UnificationErrorKind::ArityMismatch {
                expected_len: expected_args.len(),
                found_len: provided_args.len(),
            }));
        }

        for (exp_arg, prov_arg) in expected_args.iter().zip(provided_args.iter()) {
            self.unify_with_variance(exp_arg, prov_arg, Variance::Invariant)?;
        }

        Ok(())
    }

    fn is_trivially_unifiable(&self, expected: &Type, provided: &Type) -> bool {
        matches!(provided, Type::Never)
            || matches!(expected, Type::Error | Type::Unknown | Type::Any)
            || (matches!(expected, Type::Optional(_)) && matches!(provided, Type::Nil))
    }

    fn unify_infer_left(&mut self, id: u32, provided: &Type) -> Result<(), UnificationError> {
        if self.is_resolved(id) {
            let resolved = self.resolve(id).unwrap().clone();
            self.unify_with_variance(&resolved, provided, Variance::Covariant) // TODO why covariant? here
        } else {
            self.bind(id, provided.clone())
        }
    }

    fn unify_infer_right(
        &mut self,
        expected: &Type,
        id: u32,
        variance: Variance,
    ) -> Result<(), UnificationError> {
        if self.is_resolved(id) {
            let resolved = self.resolve(id).unwrap().clone();
            self.unify_with_variance(expected, &resolved, variance)
        } else {
            self.bind(id, expected.clone())
        }
    }

    fn unify_functions(
        &mut self,
        exp_inner: &FunctionType,
        prov_inner: &FunctionType,
        variance: Variance,
        mismatch: impl FnOnce(UnificationErrorKind) -> UnificationError,
    ) -> Result<(), UnificationError> {
        if prov_inner.is_vararg {
            return Err(mismatch(UnificationErrorKind::VarargNotAllowed));
        }

        if !prov_inner.type_params.is_empty() {
            return Err(mismatch(UnificationErrorKind::GenericFunctionNotAllowed));
        }

        if exp_inner.params.len() != prov_inner.params.len() {
            return Err(mismatch(UnificationErrorKind::ArityMismatch {
                expected_len: exp_inner.params.len(),
                found_len: prov_inner.params.len(),
            }));
        }
        let param_variance = variance.compose(Variance::Contravariant);
        for ((_, exp_param), (_, prov_param)) in
            exp_inner.params.iter().zip(prov_inner.params.iter())
        {
            self.unify_with_variance(exp_param, prov_param, param_variance)?;
        }

        self.unify_with_variance(&exp_inner.return_type, &prov_inner.return_type, variance)
    }

    fn unify_tuples(
        &mut self,
        expected: &TupleType,
        provided: &TupleType,
        mismatch: impl FnOnce(UnificationErrorKind) -> UnificationError,
    ) -> Result<(), UnificationError> {
        if expected.types.len() != provided.types.len() {
            return Err(mismatch(UnificationErrorKind::ArityMismatch {
                expected_len: expected.types.len(),
                found_len: provided.types.len(),
            }));
        }

        for (exp_elem, prov_elem) in expected.types.iter().zip(provided.types.iter()) {
            self.unify_with_variance(exp_elem, prov_elem, Variance::Invariant)?; // TODO only if mutable
        }

        Ok(())
    }
}

fn type_mismatch(expected: &Type, found: &Type) -> UnificationError {
    UnificationError {
        kind: UnificationErrorKind::TypeMismatch,
        expected: expected.clone(),
        found: found.clone(),
    }
}

#[derive(Debug, Clone)]
pub struct UnificationError {
    pub kind: UnificationErrorKind,
    pub expected: Type,
    pub found: Type,
}

#[derive(Debug, Clone)]
pub enum UnificationErrorKind {
    TypeMismatch,
    ArityMismatch {
        expected_len: usize,
        found_len: usize,
    },
    VarianceMismatch,
    VarargNotAllowed,
    GenericFunctionNotAllowed,
    MetatypeNotUnifiable,
    OccursCheck,
}

impl fmt::Display for UnificationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            UnificationErrorKind::TypeMismatch => {
                write!(
                    f,
                    "Type mismatch: expected {}, found {}",
                    self.expected, self.found
                )
            }
            UnificationErrorKind::ArityMismatch {
                expected_len,
                found_len,
            } => {
                write!(
                    f,
                    "Arity mismatch: expected {} elements, found {} elements\n  Expected type: {}\n  Found type: {}",
                    expected_len, found_len, self.expected, self.found
                )
            }
            UnificationErrorKind::VarianceMismatch => {
                write!(
                    f,
                    "Variance mismatch: types are not compatible\n  Expected: {}\n  Found: {}",
                    self.expected, self.found
                )
            }
            UnificationErrorKind::VarargNotAllowed => {
                write!(f, "Cannot use vararg functions as arguments")
            }
            UnificationErrorKind::GenericFunctionNotAllowed => {
                write!(
                    f,
                    "Cannot assign a generic function.\n  TIP: specify generics using .<Type> notation"
                )
            }
            UnificationErrorKind::MetatypeNotUnifiable => {
                write!(f, "Cannot unify metatypes")
            }
            UnificationErrorKind::OccursCheck => {
                write!(
                    f,
                    "Infinite type detected: type variable occurs in its own definition\n  Type: {}",
                    self.expected
                )
            }
        }
    }
}

impl From<UnificationError> for String {
    fn from(err: UnificationError) -> String {
        err.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_occurs_check() {
        let mut ctx = InferenceContext::new();
        let var = ctx.new_type_var();
        let Type::Infer(id) = var else { panic!() };

        // Try to unify T = List<T> (should fail)
        let list_of_var = Type::List(Box::new(Type::Infer(id)));
        let result = ctx.unify_types(&var, &list_of_var);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().kind,
            UnificationErrorKind::OccursCheck
        ));
    }
}
