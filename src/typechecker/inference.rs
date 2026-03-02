use crate::typechecker::core::types::{FunctionType, GenericArgs, TupleType, Type};
use crate::typechecker::system::make_substitution_map;
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::fmt;

pub struct InferenceContext {
    substitutions: HashMap<u32, Type>,
    debug_names: HashMap<u32, Symbol>,
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
            debug_names: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn new_type_var(&mut self) -> Type {
        let id = self.next_id;
        self.next_id += 1;
        Type::Infer(id)
    }

    /// Like [`new_type_var`] but records `name` as the human-readable origin of this
    /// inference variable (e.g. the generic parameter name `"T"`).
    /// The name is used when building [`CannotInferType`] error messages.
    pub fn new_named_type_var(&mut self, name: Symbol) -> Type {
        let id = self.next_id;
        self.next_id += 1;
        self.debug_names.insert(id, name);
        Type::Infer(id)
    }

    /// Collect the human-readable names of every unresolved inference variable
    /// reachable inside `ty`.  Variables that have no recorded name are rendered
    /// as `"?<id>"` so they are never silently dropped.
    pub fn uninferred_names(&self, ty: &Type) -> Vec<String> {
        let mut names: Vec<String> = Vec::new();
        self.collect_uninferred(ty, &mut names);
        names.sort();
        names.dedup();
        names
    }

    fn collect_uninferred(&self, ty: &Type, names: &mut Vec<String>) {
        match ty {
            Type::Infer(id) => {
                if self.is_resolved(*id) {
                    let resolved = self.resolve(*id).unwrap().clone();
                    self.collect_uninferred(&resolved, names);
                } else {
                    let name = self
                        .debug_names
                        .get(id)
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| format!("?{}", id));
                    names.push(name);
                }
            }
            Type::Optional(inner) => self.collect_uninferred(inner, names),
            Type::Tuple(tt) => {
                for t in tt.types.iter() {
                    self.collect_uninferred(t, names);
                }
            }
            Type::Function(ft) => {
                for (_, t) in ft.params.iter() {
                    self.collect_uninferred(t, names);
                }
                self.collect_uninferred(&ft.return_type, names);
            }
            Type::Struct(_, args) | Type::Enum(_, args) => {
                for t in args.iter() {
                    self.collect_uninferred(t, names);
                }
            }
            _ => {}
        }
    }

    /// Should be used when new type arguments can be partially provided and the rest has to use new infer holes.
    pub fn fresh_args(&mut self, params: &[Symbol], provided: &[Type]) -> GenericArgs {
        let is_correct = params.len() == provided.len() || provided.is_empty();
        debug_assert!(
            is_correct,
            "Should be used when new type arguments can be partially provided and the rest has to use new infer holes."
        );
        params
            .iter()
            .enumerate()
            .map(|(i, name)| {
                provided
                    .get(i)
                    .cloned()
                    .unwrap_or_else(|| self.new_named_type_var(name.clone()))
            })
            .collect()
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
        ty.any(&|t| match t {
            Type::Infer(other_id) => {
                if id == *other_id {
                    return true;
                }
                if let Some(res) = self.resolve(*other_id) {
                    return self.occurs_in(id, res);
                }
                false
            }
            _ => false,
        })
    }

    pub fn substitute(&self, ty: &Type) -> Type {
        ty.clone().transform(
            &|ty| match ty {
                Type::Infer(id) => {
                    if let Some(resolved) = self.substitutions.get(&id).cloned() {
                        self.substitute(&resolved)
                    } else {
                        ty
                    }
                }
                other => other,
            },
            &|type_params| type_params.to_vec(),
        )
    }

    pub(crate) fn unify_types(
        &mut self,
        expected_ty: &Type,
        provided: &Type,
    ) -> Result<(), UnificationError> {
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
        match (expected, provided) {
            (_, Type::Infer(id)) => self.unify_infer_right(expected, *id, variance),
            (Type::Infer(id), _) => self.unify_infer_left(*id, provided, variance),

            _ if Self::is_trivially_unifiable(expected, provided, variance) => Ok(()),

            (Type::Optional(expected_inner), Type::Optional(provided_inner)) => {
                self.unify_with_variance(expected_inner, provided_inner, variance)
            }
            (Type::Optional(expected_inner), provided) if variance == Variance::Covariant => {
                self.unify_with_variance(expected_inner, provided, variance)
            }
            (expected, Type::Optional(provided_inner)) if variance == Variance::Contravariant => {
                self.unify_with_variance(expected, provided_inner, variance)
            }
            (Type::Function(exp), Type::Function(prov)) => {
                if !prov.type_params.is_empty() {
                    let new_generics = self.fresh_args(&prov.type_params, &[]);
                    let map = make_substitution_map(&prov.type_params, &new_generics);
                    let new_prov = Type::Function(prov.clone()).generic_to_concrete(&map);
                    let Type::Function(prov) = new_prov else {
                        return Err(mismatch(UnificationErrorKind::GenericFunctionNotAllowed));
                    };
                    self.unify_functions(exp, &prov, variance, mismatch)
                } else {
                    self.unify_functions(exp, prov, variance, mismatch)
                }
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
            (Type::Interface(exp_name), Type::Interface(prov_name)) => {
                if exp_name == prov_name {
                    Ok(())
                } else {
                    Err(mismatch(UnificationErrorKind::TypeMismatch))
                }
            }
            (Type::Enum(exp_name, exp_args), Type::Enum(prov_name, prov_args)) => {
                self.unify_complex(exp_name, exp_args, prov_name, prov_args, variance, mismatch)
            }
            (Type::GenericParam(exp_name), Type::GenericParam(prov_name)) => {
                if exp_name == prov_name {
                    Ok(())
                } else {
                    Err(type_mismatch(expected, provided))
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

    fn is_trivially_unifiable(expected: &Type, provided: &Type, variance: Variance) -> bool {
        match variance {
            Variance::Covariant => {
                matches!(provided, Type::Never)
                    || matches!(expected, Type::Error | Type::Unknown | Type::Any)
                    || (matches!(expected, Type::Optional(_)) && matches!(provided, Type::Nil))
            }
            Variance::Contravariant => {
                Self::is_trivially_unifiable(provided, expected, Variance::Covariant)
            }
            Variance::Invariant => {
                matches!(expected, Type::Error | Type::Unknown)
                    || matches!(provided, Type::Error | Type::Unknown)
            }
        }
    }

    fn unify_infer_left(
        &mut self,
        id: u32,
        provided: &Type,
        variance: Variance,
    ) -> Result<(), UnificationError> {
        if self.is_resolved(id) {
            let resolved = self.resolve(id).unwrap().clone();
            self.unify_with_variance(&resolved, provided, variance)
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
        // if prov_inner.is_vararg {
        //     return Err(mismatch(UnificationErrorKind::VarargNotAllowed));
        // }

        // if !prov_inner.type_params.is_empty() {
        //     return Err(mismatch(UnificationErrorKind::GenericFunctionNotAllowed));
        // }

        if exp_inner.arity() != prov_inner.arity() {
            return Err(mismatch(UnificationErrorKind::ArityMismatch {
                expected_len: exp_inner.arity(),
                found_len: prov_inner.arity(),
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
        let list_of_var = Type::new_list(Type::Infer(id));
        let result = ctx.unify_types(&var, &list_of_var);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().kind,
            UnificationErrorKind::OccursCheck
        ));
    }
}
