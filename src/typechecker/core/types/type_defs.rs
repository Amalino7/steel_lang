use crate::scanner::Span;
use crate::typechecker::core::types::{GenericArgs, Type};
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::system::{make_substitution_map, TypeSystem};
use crate::typechecker::Symbol;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub name: Symbol,
    pub origin: Span,
    pub fields: HashMap<Symbol, usize>,
    ordered_fields: Vec<(Symbol, Type)>,
    generic_params: Vec<Symbol>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InterfaceType {
    pub name: Symbol,
    pub methods: HashMap<String, (usize, Type)>,
    pub origin: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumType {
    pub name: Symbol,
    pub origin: Span,
    pub variants: HashMap<Symbol, usize>,
    ordered_variants: Vec<(Symbol, Type)>, // Void, one arg, tuple, struct
    generic_params: Vec<Symbol>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeConstructor {
    pub constructed_type: Type,
    pub resolved_args: Vec<(Symbol, Type)>,
}

impl EnumType {
    pub fn generic_params(&self) -> &[Symbol] {
        &self.generic_params
    }

    pub fn generic_count(&self) -> usize {
        self.generic_params.len()
    }

    pub fn new(name: Symbol, origin: Span, generic_params: Vec<Symbol>) -> Self {
        Self {
            name,
            origin,
            variants: HashMap::new(),
            ordered_variants: Vec::new(),
            generic_params,
        }
    }

    pub fn init(
        &mut self,
        variants: HashMap<Symbol, usize>,
        ordered_variants: Vec<(Symbol, Type)>,
    ) {
        self.variants = variants;
        self.ordered_variants = ordered_variants;
    }

    pub fn get_static_variant(
        &self,
        variant: &str,
        instance: &GenericArgs,
        ctx: &mut InferenceContext,
    ) -> Option<(u16, Type, GenericArgs)> {
        let idx = self.variants.get(variant)?;
        let fresh_generics = ctx.fresh_args(&self.generic_params, instance);
        let map = make_substitution_map(&self.generic_params, &fresh_generics);
        let raw_ty = &self.ordered_variants[*idx].1;
        let final_ty = raw_ty.generic_to_concrete(&map);
        Some((*idx as u16, final_ty, fresh_generics))
    }

    // TODO consider how to change logic
    pub fn get_variant_from_instance(
        &self,
        name: &str,
        generic_args: &GenericArgs,
    ) -> Option<(u16, Type)> {
        let map = make_substitution_map(&self.generic_params, generic_args);
        self.variants.get(name).map(|idx| {
            let raw_ty = self.ordered_variants[*idx].1.clone();
            (*idx as u16, raw_ty.generic_to_concrete(&map))
        })
    }

    pub fn get_variant_by_index(&self, index: usize, generic_args: &GenericArgs) -> Option<Type> {
        self.ordered_variants.get(index).map(|(_, ty)| {
            let map = make_substitution_map(&self.generic_params, generic_args);
            ty.clone().generic_to_concrete(&map)
        })
    }
    pub fn get_constructor(
        &self,
        instance: GenericArgs,
        variant_ty: Type,
        sys: &TypeSystem,
    ) -> Option<TypeConstructor> {
        let map = make_substitution_map(&self.generic_params, &instance);
        let params = match &variant_ty {
            Type::Tuple(tuple) => tuple
                .types
                .iter()
                .enumerate()
                .map(|(s, ty)| {
                    let name = s.to_string().into();
                    let final_ty = ty.clone().generic_to_concrete(&map);
                    (name, final_ty)
                })
                .collect(),
            Type::Struct(struct_name, _) => {
                let struct_def = sys.get_struct(struct_name).unwrap();
                struct_def
                    .ordered_fields
                    .iter()
                    .map(|(s, t)| {
                        let final_ty = t.clone().generic_to_concrete(&map);
                        (s.clone(), final_ty)
                    })
                    .collect()
            }
            other => {
                vec![("_".into(), other.clone().generic_to_concrete(&map))]
            }
        };
        let self_type = Type::Enum(self.name.clone(), instance);
        Some(TypeConstructor {
            constructed_type: self_type,
            resolved_args: params,
        })
    }
}
impl StructType {
    pub fn generic_params(&self) -> &[Symbol] {
        &self.generic_params
    }

    pub fn generic_count(&self) -> usize {
        self.generic_params.len()
    }

    pub fn new(name: Symbol, origin: Span, generic_params: Vec<Symbol>) -> Self {
        Self {
            name,
            origin,
            fields: HashMap::new(),
            ordered_fields: Vec::new(),
            generic_params,
        }
    }

    pub fn init(&mut self, fields: HashMap<Symbol, usize>, ordered_fields: Vec<(Symbol, Type)>) {
        self.fields = fields;
        self.ordered_fields = ordered_fields;
    }

    pub fn get_field_by_index(&self, index: usize, generic_args: &GenericArgs) -> Option<Type> {
        self.ordered_fields.get(index).map(|(_, ty)| {
            let map = make_substitution_map(&self.generic_params, generic_args);
            ty.clone().generic_to_concrete(&map)
        })
    }

    pub fn get_field(&self, field: &str, generic_args: &GenericArgs) -> Option<(usize, Type)> {
        self.fields.get(field).map(|idx| {
            let map = make_substitution_map(&self.generic_params, generic_args);
            let raw_type = self.ordered_fields[*idx].1.clone();
            (*idx, raw_type.generic_to_concrete(&map))
        })
    }

    pub fn get_constructor(
        &self,
        instance: &[Type],
        ctx: &mut InferenceContext,
    ) -> TypeConstructor {
        let type_args = ctx.fresh_args(&self.generic_params, instance);
        let map = make_substitution_map(&self.generic_params, &type_args);
        let args = self
            .ordered_fields
            .iter()
            .map(|(field, ty)| {
                let final_ty = ty.clone().generic_to_concrete(&map);
                (field.clone(), final_ty)
            })
            .collect();

        let self_type = Type::Struct(self.name.clone(), type_args);

        TypeConstructor {
            constructed_type: self_type,
            resolved_args: args,
        }
    }
}
