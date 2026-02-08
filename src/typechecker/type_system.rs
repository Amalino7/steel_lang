use crate::scanner::{Span, Token};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::types::{EnumType, InterfaceType, StructType, TupleType, Type};
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::rc::Rc;

pub type TySys = TypeSystem;

pub struct TypeSystem {
    structs: HashMap<Symbol, StructType>,
    interfaces: HashMap<Symbol, InterfaceType>,
    impls: HashMap<(Symbol, Symbol), u32>,
    enums: HashMap<Symbol, EnumType>,
    active_generics: Vec<Symbol>,
}

pub fn generics_to_map(
    generics: &[Symbol],
    generics_provided: &[Type],
    mut auto_fil: Option<&mut InferenceContext>,
) -> HashMap<Symbol, Type> {
    generics
        .iter()
        .enumerate()
        .map(|(idx, s)| {
            let type_var = generics_provided.get(idx).cloned().unwrap_or_else(|| {
                if let Some(ctx) = auto_fil.as_mut() {
                    InferenceContext::new_type_var(ctx)
                } else {
                    Type::Unknown
                }
            });
            (s.clone(), type_var)
        })
        .collect()
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

    pub fn get_generics_map(&self, ty: &Type) -> HashMap<Symbol, Type> {
        match ty {
            Type::Struct(name, args) => {
                generics_to_map(&self.get_struct(name).unwrap().generic_params, args, None)
            }
            Type::Enum(name, args) => {
                generics_to_map(&self.get_enum(name).unwrap().generic_params, args, None)
            }
            Type::List(inner) => {
                let mut map = HashMap::new();
                map.insert("T".into(), *inner.clone());
                map
            }
            Type::Map(key, value) => {
                let mut map = HashMap::new();
                map.insert("K".into(), *key.clone());
                map.insert("V".into(), *value.clone());
                map
            }
            _ => HashMap::new(),
        }
    }

    pub fn get_vtable_idx(&self, type_name: &str, iface_name: Symbol) -> Option<u32> {
        self.impls.get(&(type_name.into(), iface_name)).copied()
    }

    pub fn pop_n_generics(&mut self, count: usize) {
        self.active_generics
            .truncate(self.active_generics.len() - count);
    }
    pub fn does_generic_exist(&self, name: &Symbol) -> bool {
        self.active_generics.contains(name)
    }

    // Only the name exists
    pub fn declare_struct(&mut self, origin: Span, name: Symbol, generic_params: &[Token]) {
        self.structs.insert(
            name.clone(),
            StructType {
                origin,
                name,
                fields: HashMap::new(),
                ordered_fields: vec![],
                generic_params: generic_params.iter().map(|t| t.lexeme.into()).collect(),
            },
        );
    }

    pub fn declare_enum(&mut self, origin: Span, name: Symbol, generic_params: &[Token]) {
        self.enums.insert(
            name.clone(),
            EnumType {
                origin,
                name,
                variants: HashMap::new(),
                ordered_variants: vec![],
                generic_params: generic_params.iter().map(|t| t.lexeme.into()).collect(),
            },
        );
    }

    pub fn declare_interface(&mut self, name: Symbol, origin: Span) {
        self.interfaces.insert(
            name.clone(),
            InterfaceType {
                origin,
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
    pub fn define_enum(&mut self, name: &str, variants: HashMap<Symbol, (usize, Type)>) {
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
        if let Some(e) = self.interfaces.get_mut(name) {
            e.methods = methods;
        }
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
    #[allow(dead_code)]
    pub fn does_enum_exist(&self, name: &str) -> bool {
        self.enums.contains_key(name)
    }
    #[allow(dead_code)]
    pub fn does_type_exist(&self, name: &str) -> bool {
        self.structs.contains_key(name)
            || self.interfaces.contains_key(name)
            || self.enums.contains_key(name)
            || name == "string"
            || name == "number"
            || name == "boolean"
            || name == "void"
            || name == "List"
            || name == "Map"
    }
    pub fn get_owned_type_name(&self, name: &str) -> Option<Symbol> {
        if let Some(s) = self.structs.get(name) {
            Some(s.name.clone())
        } else if let Some(i) = self.interfaces.get(name) {
            Some(i.name.clone())
        } else if let Some(e) = self.enums.get(name) {
            Some(e.name.clone())
        } else if name == "string"
            || name == "number"
            || name == "boolean"
            || name == "void"
            || name == "List"
            || name == "Map"
        {
            Some(name.into())
        } else {
            None
        }
    }

    pub fn get_generics_count(&self, name: &str) -> usize {
        if let Some(s) = self.structs.get(name) {
            s.generic_params.len()
        } else if self.interfaces.contains_key(name) {
            0
        } else if let Some(e) = self.enums.get(name) {
            e.generic_params.len()
        } else if name == "List" {
            1
        } else if name == "Map" {
            2
        } else {
            0
        }
    }

    pub fn can_compare(left: &Type, right: &Type) -> bool {
        if let Type::Optional(inner) = left {
            if *right == Type::Nil {
                true
            } else {
                Self::can_compare(inner, right)
            }
        } else if let Type::Optional(inner) = right {
            if *left == Type::Nil {
                true
            } else {
                Self::can_compare(left, inner)
            }
        } else {
            left == right
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

    pub fn generic_to_concrete(generic_ty: Type, generics_map: &HashMap<Symbol, Type>) -> Type {
        match generic_ty {
            Type::Metatype(_, _) => generic_ty,
            Type::Nil
            | Type::Number
            | Type::String
            | Type::Boolean
            | Type::Void
            | Type::Unknown
            | Type::Never
            | Type::Infer(_)
            | Type::Error
            | Type::Any => generic_ty,
            Type::Optional(inner) => {
                Type::Optional(Box::new(Self::generic_to_concrete(*inner, generics_map)))
            }
            Type::List(inner) => {
                Type::List(Box::new(Self::generic_to_concrete(*inner, generics_map)))
            }
            Type::Map(key_type, value_type) => Type::Map(
                Box::new(Self::generic_to_concrete(*key_type, generics_map)),
                Box::new(Self::generic_to_concrete(*value_type, generics_map)),
            ),
            Type::Function(func_type) => {
                if func_type.is_vararg {
                    return Type::Function(func_type);
                }
                let params = func_type
                    .params
                    .iter()
                    .map(|(s, t)| {
                        (
                            s.to_string(),
                            TypeSystem::generic_to_concrete(t.clone(), generics_map),
                        )
                    })
                    .collect();
                let return_type =
                    TypeSystem::generic_to_concrete(func_type.return_type.clone(), generics_map);

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
                        Type::GenericParam(generic)
                    } else {
                        new_type.clone()
                    }
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

    pub(crate) fn resolve_named_arg(
        &self,
        params: &[(String, Type)],
        name: &str,
        span: Span,
    ) -> Result<usize, TypeCheckerError> {
        params
            .iter()
            .position(|(pname, _)| pname == name)
            .ok_or_else(|| TypeCheckerError::UndefinedParameter {
                param_name: name.to_string(),
                span,
            })
    }

    pub(crate) fn resolve_member_type(
        &self,
        parent_type: &Type,
        field: &Token,
    ) -> Result<(u8, Type), TypeCheckerError> {
        match parent_type {
            Type::Tuple(tuple_type) => {
                let idx = field.lexeme.parse::<u8>().map_err(|_| {
                    TypeCheckerError::InvalidTupleIndex {
                        tuple_type: parent_type.clone(),
                        index: field.lexeme.to_string(),
                        span: field.span,
                    }
                })?;

                if idx as usize >= tuple_type.types.len() {
                    return Err(TypeCheckerError::InvalidTupleIndex {
                        tuple_type: parent_type.clone(),
                        index: idx.to_string(),
                        span: field.span,
                    });
                }

                Ok((idx, tuple_type.types[idx as usize].clone()))
            }
            Type::Struct(name, generics) => {
                let struct_def = self
                    .get_struct(name)
                    .expect("Struct def missing after type check");

                let (field_id, raw_type) = struct_def
                    .fields
                    .get(field.lexeme)
                    .map(|id| (*id, struct_def.ordered_fields[*id].1.clone()))
                    .ok_or_else(|| TypeCheckerError::UndefinedField {
                        struct_name: struct_def.name.to_string(),
                        field_name: field.lexeme.to_string(),
                        span: field.span,
                    })?;
                let concrete_type = TypeSystem::generic_to_concrete(
                    raw_type,
                    &generics_to_map(&struct_def.generic_params, generics, None),
                );

                Ok((field_id as u8, concrete_type))
            }
            _ => Err(TypeCheckerError::TypeHasNoFields {
                found: parent_type.clone(),
                span: field.span,
            }),
        }
    }
}
