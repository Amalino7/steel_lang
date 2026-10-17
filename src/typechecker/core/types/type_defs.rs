use crate::scanner::{FileId, Span};
use crate::typechecker::Symbol;
use crate::typechecker::core::types::{
    EnumId, GenericArgs, GenericTypeId, InterfaceId, StructId, Type,
};
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::system::{TypeSystem, make_substitution_map};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct StructType {
    pub id: StructId,
    pub name: Symbol,
    pub origin: Span,
    pub is_public: bool,
    pub fields: HashMap<Symbol, usize>,
    ordered_fields: Vec<FieldDef>,
    generic_params: Vec<GenericTypeId>,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub index: usize,
    pub ty: Type,
    pub name: Symbol,
    pub is_public: bool,
    pub name_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InterfaceType {
    pub id: InterfaceId,
    pub name: Symbol,
    pub methods: HashMap<String, (usize, Type)>,
    pub origin: Span,
    pub is_public: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumType {
    pub id: EnumId,
    pub name: Symbol,
    pub origin: Span,
    pub is_public: bool,
    pub variants: HashMap<Symbol, usize>,
    ordered_variants: Vec<(Symbol, Type)>, // Void, one arg, tuple, struct
    generic_params: Vec<GenericTypeId>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GenericType {
    pub id: GenericTypeId,
    pub name: Symbol,
    pub origin: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeConstructor {
    pub constructed_type: Type,
    pub resolved_args: Vec<(Symbol, Type)>,
}

impl EnumType {
    pub fn generic_params(&self) -> &[GenericTypeId] {
        &self.generic_params
    }

    pub fn generic_count(&self) -> usize {
        self.generic_params.len()
    }

    /// Payload types of all variants, in declaration order.
    pub fn variant_types(&self) -> impl Iterator<Item = &Type> {
        self.ordered_variants.iter().map(|(_, ty)| ty)
    }

    pub fn new(
        id: EnumId,
        name: Symbol,
        origin: Span,
        generic_params: Vec<GenericTypeId>,
        is_public: bool,
    ) -> Self {
        Self {
            id,
            name,
            origin,
            is_public,
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
            Type::Struct(struct_id, _) => {
                let struct_def = sys.get_struct(*struct_id);
                struct_def
                    .ordered_fields
                    .iter()
                    .map(|field_def| {
                        let final_ty = field_def.ty.clone().generic_to_concrete(&map);
                        (field_def.name.clone(), final_ty)
                    })
                    .collect()
            }
            other => {
                vec![("_".into(), other.clone().generic_to_concrete(&map))]
            }
        };
        let self_type = Type::Enum(self.id, instance);
        Some(TypeConstructor {
            constructed_type: self_type,
            resolved_args: params,
        })
    }
}
impl StructType {
    pub fn generic_params(&self) -> &[GenericTypeId] {
        &self.generic_params
    }

    pub fn generic_count(&self) -> usize {
        self.generic_params.len()
    }

    pub fn new(
        id: StructId,
        name: Symbol,
        origin: Span,
        generic_params: Vec<GenericTypeId>,
        is_public: bool,
    ) -> Self {
        Self {
            id,
            name,
            origin,
            is_public,
            fields: HashMap::new(),
            ordered_fields: Vec::new(),
            generic_params,
        }
    }

    pub fn init(&mut self, fields: HashMap<Symbol, usize>, ordered_fields: Vec<FieldDef>) {
        self.fields = fields;
        self.ordered_fields = ordered_fields;
    }

    pub fn is_field_public(&self, idx: usize) -> bool {
        self.ordered_fields[idx].is_public
    }

    /// Field types paired with their visibility, in declaration order.
    pub fn fields_with_visibility(&self) -> impl Iterator<Item = (&Type, bool)> {
        self.ordered_fields
            .iter()
            .map(|field_def| (&field_def.ty, field_def.is_public))
    }

    pub fn field_span(&self, idx: usize) -> Span {
        self.ordered_fields[idx].name_span
    }

    /// Names and spans of the private fields, in declaration order.
    pub fn private_fields(&self) -> Vec<(String, Span)> {
        (0..self.ordered_fields.len())
            .filter(|&i| !self.is_field_public(i))
            .map(|i| {
                (
                    self.ordered_fields[i].name.to_string(),
                    self.ordered_fields[i].name_span,
                )
            })
            .collect()
    }

    /// Names of the fields code in `from_file` may see: all of them inside the defining module,
    /// only the public ones elsewhere. Used for "did you mean" suggestions.
    pub fn visible_field_names(&self, from_file: FileId) -> impl Iterator<Item = &Symbol> {
        let own = self.origin.file_id == from_file;
        self.ordered_fields
            .iter()
            .filter(move |field_def| own || self.is_field_public(field_def.index))
            .map(|def| &def.name)
    }

    pub fn get_field(&self, field: &str, generic_args: &GenericArgs) -> Option<(usize, Type)> {
        self.fields.get(field).map(|idx| {
            let map = make_substitution_map(&self.generic_params, generic_args);
            let raw_type = self.ordered_fields[*idx].ty.clone();
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
            .map(|field_def| {
                let final_ty = field_def.ty.clone().generic_to_concrete(&map);
                (field_def.name.clone(), final_ty)
            })
            .collect();

        let self_type = Type::Struct(self.id, type_args);

        TypeConstructor {
            constructed_type: self_type,
            resolved_args: args,
        }
    }
}
