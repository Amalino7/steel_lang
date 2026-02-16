use crate::scanner::Span;
use crate::typechecker::core::types::{GenericArgs, Type};
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::system::{generics_to_map, TypeSystem};
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub name: Symbol,
    pub origin: Span,
    pub fields: HashMap<Symbol, usize>,
    pub(crate) ordered_fields: Vec<(Symbol, Type)>,
    pub(crate) generic_params: Vec<Symbol>,
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
    // TODO remove pub
    pub(crate) generic_params: Vec<Symbol>,
}

pub struct TypeConstructor {
    pub constructed_type: Type,
    pub resolved_args: Vec<(Symbol, Type)>,
}

impl EnumType {
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
    pub fn get_variant(&self, name: &str, generic_args: &GenericArgs) -> Option<(u16, Type)> {
        let map = generics_to_map(&self.generic_params, generic_args, None);
        self.variants.get(name).map(|idx| {
            let raw_ty = self.ordered_variants[*idx].1.clone();
            (*idx as u16, raw_ty.generic_to_concrete(&map))
        })
    }

    pub fn get_variant_by_index(&self, index: usize, generic_args: &GenericArgs) -> Option<Type> {
        self.ordered_variants.get(index).map(|(_, ty)| {
            let map = generics_to_map(&self.generic_params, generic_args, None);
            ty.clone().generic_to_concrete(&map)
        })
    }
    pub fn get_constructor(
        &self,
        variant_idx: u16,
        instance: &[Type],
        ctx: &mut InferenceContext,
        sys: &TypeSystem,
    ) -> Option<TypeConstructor> {
        let map = generics_to_map(&self.generic_params, instance, Some(ctx));
        let generic_args = Rc::new(self.generic_params.iter().map(|s| map[s].clone()).collect());

        let variant = self.get_variant_by_index(variant_idx as usize, &generic_args)?;
        let params = match &variant {
            Type::Tuple(tuple) => {
                let params = tuple
                    .types
                    .iter()
                    .enumerate()
                    .map(|(s, ty)| {
                        let name = s.to_string().into();
                        let final_ty = ty.clone().generic_to_concrete(&map);
                        (name, final_ty)
                    })
                    .collect();
                params
            }
            Type::Struct(struct_name, _) => {
                let struct_def = sys.get_struct(struct_name).unwrap();
                let params: Vec<_> = struct_def
                    .ordered_fields
                    .iter()
                    .map(|(s, t)| {
                        let final_ty = t.clone().generic_to_concrete(&map);
                        (s.clone(), final_ty)
                    })
                    .collect();
                params
            }
            other => {
                let params = vec![("_".into(), other.clone().generic_to_concrete(&map))];
                params
            }
        };
        let self_type = Type::Enum(self.name.clone(), generic_args);
        Some(TypeConstructor {
            constructed_type: self_type,
            resolved_args: params,
        })
    }
}
impl StructType {
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
            let map = generics_to_map(&self.generic_params, generic_args, None);
            ty.clone().generic_to_concrete(&map)
        })
    }

    pub fn get_field(&self, field: &str, generic_args: &GenericArgs) -> Option<(usize, Type)> {
        self.fields.get(field).map(|idx| {
            let map = generics_to_map(&self.generic_params, generic_args, None);
            let raw_type = self.ordered_fields[*idx].1.clone();
            (*idx, raw_type.generic_to_concrete(&map))
        })
    }

    pub fn get_constructor(
        &self,
        instance: &[Type],
        ctx: &mut InferenceContext,
    ) -> TypeConstructor {
        let map = generics_to_map(&self.generic_params, instance, Some(ctx));
        let args = self
            .ordered_fields
            .iter()
            .map(|(field, ty)| {
                let final_ty = ty.clone().generic_to_concrete(&map);
                (field.clone(), final_ty)
            })
            .collect();

        let self_type = Type::Struct(
            self.name.clone(),
            Rc::new(self.generic_params.iter().map(|s| map[s].clone()).collect()),
        );

        TypeConstructor {
            constructed_type: self_type,
            resolved_args: args,
        }
    }
}
