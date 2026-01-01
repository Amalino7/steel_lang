use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{ExprKind, TypedExpr};
use crate::typechecker::types::{EnumType, InterfaceType, StructType, TupleType, Type};
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeSystem {
    structs: HashMap<Symbol, StructType>,
    interfaces: HashMap<Symbol, InterfaceType>,
    impls: HashMap<(Symbol, Symbol), u32>,
    enums: HashMap<Symbol, EnumType>,
    active_generics: Vec<Symbol>,
}

impl TypeSystem {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashMap::new(),
            enums: HashMap::new(),
            active_generics: Vec::new(),
        }
    }

    pub fn push_generics(&mut self, generics: &[Token]) {
        generics
            .iter()
            .for_each(|e| self.active_generics.push(e.lexeme.into()));
    }
    pub fn get_active_generics(&self) -> Vec<Symbol> {
        self.active_generics.clone()
    }

    pub fn pop_n_generics(&mut self, count: usize) {
        self.active_generics
            .truncate(self.active_generics.len() - count);
    }
    pub fn does_generic_exist(&self, name: &Symbol) -> bool {
        self.active_generics.contains(name)
    }

    // Only the name exists
    pub fn declare_struct(&mut self, name: Symbol, generic_params: &[Token]) {
        self.structs.insert(
            name.clone(),
            StructType {
                name,
                fields: HashMap::new(),
                ordered_fields: vec![],
                generic_params: generic_params.iter().map(|t| t.lexeme.into()).collect(),
            },
        );
    }

    pub fn declare_enum(&mut self, name: Symbol, generic_params: &[Token]) {
        self.enums.insert(
            name.clone(),
            EnumType {
                name,
                variants: HashMap::new(),
                ordered_variants: vec![],
                generic_params: generic_params.iter().map(|t| t.lexeme.into()).collect(),
            },
        );
    }

    pub fn declare_interface(&mut self, name: Symbol) {
        self.interfaces.insert(
            name.clone(),
            InterfaceType {
                name,
                methods: HashMap::new(),
            },
        );
    }

    pub fn define_struct(&mut self, name: &str, fields_map: HashMap<String, (usize, Type)>) {
        if let Some(s) = self.structs.get_mut(name) {
            s.fields = fields_map
                .iter()
                .map(|(k, (idx, _))| (k.clone(), *idx))
                .collect();

            let mut vec_fields = vec![None; fields_map.len()];
            for (k, (idx, t)) in fields_map {
                if idx < vec_fields.len() {
                    vec_fields[idx] = Some((k, t));
                }
            }
            s.ordered_fields = vec_fields.into_iter().map(|opt| opt.unwrap()).collect();
        }
    }
    pub fn define_enum(&mut self, name: &str, variants: HashMap<String, (usize, Type)>) {
        if let Some(e) = self.enums.get_mut(name) {
            e.variants = variants
                .iter()
                .map(|(k, (idx, _))| (k.clone(), *idx))
                .collect();
            let mut vec_variants = vec![None; variants.len()];
            for (k, (idx, t)) in variants {
                if idx < vec_variants.len() {
                    vec_variants[idx] = Some((k, t));
                }
            }
            e.ordered_variants = vec_variants.into_iter().map(|opt| opt.unwrap()).collect();
        }
    }

    pub fn define_interface(&mut self, name: &str, methods: HashMap<String, (usize, Type)>) {
        self.interfaces.get_mut(name).map(|e| e.methods = methods);
    }

    pub fn define_impl(&mut self, struct_name: &str, iface_name: Symbol) {
        self.impls
            .insert((struct_name.into(), iface_name), self.impls.len() as u32);
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructType> {
        self.structs.get(name)
    }

    pub fn get_interface(&self, name: &str) -> Option<&InterfaceType> {
        self.interfaces.get(name)
    }

    pub fn get_enum(&self, name: &str) -> Option<&EnumType> {
        self.enums.get(name)
    }

    pub fn does_enum_exist(&self, name: &str) -> bool {
        self.enums.contains_key(name)
    }
    pub fn does_type_exist(&self, name: &str) -> bool {
        if self.structs.contains_key(name)
            || self.interfaces.contains_key(name)
            || self.enums.contains_key(name)
            || name == "string"
            || name == "number"
            || name == "boolean"
            || name == "void"
        {
            true
        } else {
            false
        }
    }
    pub fn get_owned_type_name(&self, name: &str) -> Option<Symbol> {
        if let Some(s) = self.structs.get(name) {
            Some(s.name.clone())
        } else if let Some(i) = self.interfaces.get(name) {
            Some(i.name.clone())
        } else if let Some(e) = self.enums.get(name) {
            Some(e.name.clone())
        } else if name == "string" || name == "number" || name == "boolean" || name == "void" {
            Some(name.into())
        } else {
            None
        }
    }

    pub fn get_generics_count(&self, name: &str) -> usize {
        if let Some(s) = self.structs.get(name) {
            s.generic_params.len()
        } else if let Some(i) = self.interfaces.get(name) {
            0
        } else if let Some(e) = self.enums.get(name) {
            e.generic_params.len()
        } else {
            0
        }
    }

    pub fn can_compare(&self, left: &Type, right: &Type) -> bool {
        if let Type::Optional(inner) = left {
            if *right == Type::Nil {
                true
            } else {
                self.can_compare(inner, right)
            }
        } else if let Type::Optional(inner) = right {
            if *left == Type::Nil {
                true
            } else {
                self.can_compare(left, inner)
            }
        } else if left == right {
            true
        } else {
            false
        }
    }

    fn unify_types(
        expected_ty: &Type,
        generics_map: &mut HashMap<Symbol, Type>,
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
            generics_map: &mut HashMap<Symbol, Type>,
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
                .map(|(e, p)| TypeSystem::unify_types(e, generics_map, p))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(())
        }

        match expected_ty {
            Type::Metatype(_, _) => Err("Cannot unify metatypes".into()),
            Type::Nil | Type::Number | Type::String | Type::Void | Type::Boolean => {
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
                    Self::unify_types(expected, generics_map, provided)
                } else {
                    Self::unify_types(expected, generics_map, provided)
                }
            }
            Type::Function(expected_inner) => {
                if let Type::Function(provided_inner) = provided {
                    if provided_inner.is_vararg {
                        return Err("Cannot use vararg functions as arguments".into());
                    }
                    if provided_inner.type_params.len() != 0 {
                        return Err("Cannot assign a generic function.\n TIP: specify generics using .<Type> notation.".into());
                    }

                    for (expected_param, provided_param) in expected_inner
                        .params
                        .iter()
                        .zip(provided_inner.params.iter())
                    {
                        Self::unify_types(&expected_param.1, generics_map, &provided_param.1)?;
                    }
                    Self::unify_types(
                        &expected_inner.return_type,
                        generics_map,
                        &provided_inner.return_type,
                    )
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Tuple(expected_inner) => {
                if let Type::Tuple(provided_inner) = provided {
                    for (expected_param, provided_param) in
                        expected_inner.types.iter().zip(provided_inner.types.iter())
                    {
                        Self::unify_types(expected_param, generics_map, &provided_param)?;
                    }
                    Ok(())
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::GenericParam(name) => {
                // If it exists in map it must be inferred
                if let Some(new_ty) = generics_map.get(name) {
                    if new_ty == &Type::Unknown {
                        generics_map.insert(name.clone(), provided.clone());
                        Ok(())
                    } else if let Type::GenericParam(new_name) = new_ty
                        && new_name == name
                    {
                        Ok(())
                    } else {
                        Self::unify_types(&new_ty.clone(), generics_map, provided)
                    }
                } else {
                    if let Type::GenericParam(provided_name) = provided
                        && name == provided_name
                    {
                        Ok(())
                    } else {
                        mismatch(expected_ty, provided)
                    }
                }
            }
            Type::Struct(name, args) => {
                if let Type::Struct(provided_name, provided_args) = provided {
                    unify_complex(name, args, provided_name, provided_args, generics_map)
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Interface(name, args) => {
                if let Type::Interface(provided_name, provided_args) = provided {
                    unify_complex(name, args, provided_name, provided_args, generics_map)
                } else {
                    mismatch(expected_ty, provided)
                }
            }
            Type::Enum(name, args) => {
                if let Type::Enum(provided_name, provided_args) = provided {
                    unify_complex(name, args, provided_name, provided_args, generics_map)
                } else {
                    mismatch(expected_ty, provided)
                }
            }
        }
    }
    pub fn verify_assignment(
        &self,
        generics_map: &mut HashMap<Symbol, Type>,
        expected_type: &Type,
        expr: TypedExpr,
        line: u32,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let are_equal = Self::unify_types(expected_type, generics_map, &expr.ty);
        // TODO rethink interface cast logic

        let expected_type = if let Type::Optional(inner) = expected_type {
            inner
        } else {
            expected_type
        };

        if let (Type::Interface(iface_name, generics), Some(name)) =
            (expected_type, expr.ty.get_name())
        {
            if let Some(idx) = self.impls.get(&(name.into(), iface_name.clone())) {
                return Ok(TypedExpr {
                    ty: Type::Interface(iface_name.clone(), generics.clone()),
                    line: expr.line,
                    kind: ExprKind::InterfaceUpcast {
                        expr: Box::new(expr),
                        vtable_idx: *idx,
                    },
                });
            }
        }
        match are_equal {
            Ok(_) => Ok(expr),
            Err(msg) => Err(TypeCheckerError::ComplexTypeMismatch {
                expected: expected_type.clone(),
                line,
                message: msg,
                found: expr.ty.clone(),
            }),
        }
    }

    pub fn implement_method(implementation: &Type, expected: &Type) -> bool {
        match (expected, implementation) {
            (Type::Function(func), Type::Function(impls)) => {
                if func.is_static {
                    return implementation == expected;
                }
                if impls.params.len() != func.params.len() {
                    return false;
                }
                if impls.return_type != func.return_type {
                    return false;
                }
                for (param, impl_param) in func.params.iter().zip(impls.params.iter()).skip(1) {
                    if param.1 != impl_param.1 {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }

    pub fn bind_arguments(
        &self,
        callee_name: &str,
        generics_map: &mut HashMap<Symbol, Type>,
        params: &[(String, Type)],
        args: Vec<(Option<&str>, TypedExpr, u32)>,
        is_vararg: bool,
        call_line: u32,
    ) -> Result<Vec<TypedExpr>, TypeCheckerError> {
        let fixed_len = params.len();
        let mut fixed: Vec<Option<TypedExpr>> = vec![None; fixed_len];
        let mut used = vec![false; fixed_len];
        let mut extras = Vec::new(); // Used for varargs
        let mut pos_cursor = 0;
        let mut seen_named = false;

        for (label, expr, line) in args {
            match label {
                Some(name) => {
                    seen_named = true;

                    let idx = self.resolve_named_arg(callee_name, params, name, line)?;

                    if used[idx] {
                        return Err(TypeCheckerError::DuplicateArgument {
                            name: params[idx].0.clone(),
                            line,
                        });
                    }

                    let expected = &params[idx].1;
                    let coerced = self.verify_assignment(generics_map, expected, expr, line)?;
                    fixed[idx] = Some(coerced);
                    used[idx] = true;
                }

                None => {
                    if seen_named {
                        return Err(TypeCheckerError::PositionalArgumentAfterNamed {
                            callee: callee_name.to_string(),
                            message: "positional arguments cannot appear after named arguments",
                            line,
                        });
                    }

                    while pos_cursor < fixed_len && used[pos_cursor] {
                        pos_cursor += 1;
                    }

                    if pos_cursor < fixed_len {
                        let expected = &params[pos_cursor].1;
                        let coerced = self.verify_assignment(generics_map, expected, expr, line)?;
                        fixed[pos_cursor] = Some(coerced);
                        used[pos_cursor] = true;
                        pos_cursor += 1;
                    } else if is_vararg {
                        extras.push(self.verify_assignment(
                            generics_map,
                            &params[pos_cursor - 1].1,
                            expr,
                            line,
                        )?);
                    } else {
                        return Err(TypeCheckerError::TooManyArguments {
                            callee: callee_name.to_string(),
                            expected: fixed_len,
                            found: fixed_len + extras.len() + 1,
                            line,
                        });
                    }
                }
            }
        }

        let mut result = Vec::with_capacity(fixed_len + extras.len());

        for (i, opt) in fixed.into_iter().enumerate() {
            result.push(opt.ok_or_else(|| TypeCheckerError::MissingArgument {
                param_name: params[i].0.clone(),
                callee: callee_name.to_string(),
                line: call_line,
            })?);
        }

        result.extend(extras);
        Ok(result)
    }

    pub fn generic_to_concrete(generic_ty: Type, generics_map: &HashMap<Symbol, Type>) -> Type {
        match generic_ty {
            Type::Metatype(_, _) => generic_ty,
            Type::Nil
            | Type::Number
            | Type::String
            | Type::Boolean
            | Type::Void
            | Type::Unknown
            | Type::Any => generic_ty,
            Type::Optional(inner) => {
                Type::Optional(Box::new(Self::generic_to_concrete(*inner, generics_map)))
            }
            Type::Function(func_type) => {
                let params = func_type
                    .params
                    .iter()
                    .map(|(s, t)| {
                        (
                            s.to_string(),
                            TypeSystem::generic_to_concrete(t.clone(), &generics_map),
                        )
                    })
                    .collect();
                let return_type =
                    TypeSystem::generic_to_concrete(func_type.return_type.clone(), &generics_map);

                Type::new_function(
                    params,
                    return_type,
                    func_type
                        .type_params
                        .iter()
                        .filter(|s| !generics_map.contains_key(*s))
                        .cloned()
                        .collect(),
                )
            }
            Type::Tuple(types) => {
                let new_types = types
                    .types
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Tuple(Rc::new(TupleType { types: new_types }))
            }
            Type::GenericParam(generic) => {
                if let Some(new_type) = generics_map.get(&generic) {
                    if new_type == &Type::Unknown {
                        panic!("Generic type not found") // TODO error handling
                    }
                    new_type.clone()
                } else {
                    Type::GenericParam(generic)
                }
            }
            Type::Struct(name, args) => {
                let resolved_args = args
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Struct(name, Rc::new(resolved_args))
            }
            Type::Interface(name, args) => {
                let resolved_args = args
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Interface(name, Rc::new(resolved_args))
            }
            Type::Enum(name, args) => {
                let resolved_args = args
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Enum(name, Rc::new(resolved_args))
            }
        }
    }

    fn resolve_named_arg(
        &self,
        callee: &str,
        params: &[(String, Type)],
        name: &str,
        line: u32,
    ) -> Result<usize, TypeCheckerError> {
        params
            .iter()
            .position(|(pname, _)| pname == name)
            .ok_or_else(|| TypeCheckerError::UndefinedParameter {
                param_name: name.to_string(),
                callee: callee.to_string(),
                line,
            })
    }
}
