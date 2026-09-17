use crate::scanner::{Span, Token};
use crate::typechecker::core::types::type_defs::{EnumType, InterfaceType, StructType};
use crate::typechecker::core::types::{
    EnumId, GenericTypeId, InterfaceId, NameTypeId, PrimitiveTypeId, Type,
};
use crate::typechecker::core::types::{StructId, Symbol};
use crate::typechecker::resolver::convert_generics;
use std::collections::HashMap;

/// Metadata about a method declared inside an impl block.
pub struct ImplMethod {
    /// How many of the last entries in the function's `type_params` come from
    /// the impl-block generics (rather than from the method's own generics).
    pub impl_generic_count: usize,
    pub self_type: Type,
}

pub struct TypeSystem {
    structs: HashMap<StructId, StructType>,
    interfaces: HashMap<InterfaceId, InterfaceType>,
    enums: HashMap<EnumId, EnumType>,
    generics: HashMap<GenericTypeId, (Span, Symbol)>,

    builtins: BuiltinTypes,

    impls: HashMap<(NameTypeId, InterfaceId), u32>,
    methods: HashMap<MethodId, ImplMethod>,
}
pub struct BuiltinTypes {
    pub list_id: StructId,
    pub map_id: StructId,
}

pub enum TypeBlueprint {
    Struct { id: StructId, arity: usize },
    Enum { id: EnumId, arity: usize },
    Interface { id: InterfaceId },
    Primitive(Type),
}

impl TypeSystem {
    pub fn new() -> Self {
        let (structs, builtins) = Self::built_in_structs();
        Self {
            structs,
            builtins,
            generics: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashMap::new(),
            enums: HashMap::new(),
            methods: HashMap::new(),
        }
    }
    fn built_in_structs() -> (HashMap<StructId, StructType>, BuiltinTypes) {
        let mut structs = HashMap::new();
        let list_id = StructId(structs.len());
        structs.insert(
            list_id,
            StructType::new(list_id, "List".into(), Span::default(), vec!["Val".into()]),
        );
        let map_id = StructId(structs.len());
        structs.insert(
            map_id,
            StructType::new(
                map_id,
                "Map".into(),
                Span::default(),
                vec!["Key".into(), "Val".into()],
            ),
        );
        let builtin = BuiltinTypes { list_id, map_id };
        (structs, builtin)
    }
    pub fn get_vtable_idx(&self, type_id: NameTypeId, iface_id: InterfaceId) -> Option<u32> {
        self.impls.get(&(type_id, iface_id)).copied()
    }

    /// Returns the names of the generic type parameters declared for a named type.
    pub fn get_generic_param_names(&self, name: NameTypeId) -> Vec<Symbol> {
        match name {
            NameTypeId::Struct(s) => self.get_struct(s).generic_params().to_vec(),
            NameTypeId::Enum(e) => self.get_enum(e).generic_params().to_vec(),
            _ => vec![],
        }
    }

    /// Builds a substitution map from a named type and its concrete type arguments.
    pub fn make_generics_map(&self, id: NameTypeId, args: &[Type]) -> HashMap<Symbol, Type> {
        let params = self.get_generic_param_names(id);
        make_substitution_map(&params, args)
    }

    /// Builds a substitution map from a concrete [`Type`].
    pub fn get_generics_map(&self, ty: &Type) -> HashMap<Symbol, Type> {
        // TODO maybe excessive
        let type_id: Option<NameTypeId> = match *ty {
            Type::Struct(id, _) => Some(id.into()),
            Type::Interface(id) => Some(id.into()),
            Type::Enum(id, _) => Some(id.into()),
            _ => None,
        };
        type_id
            .map(|id| self.make_generics_map(id, ty.generic_args()))
            .unwrap_or_default()
    }

    #[must_use]
    pub fn declare_struct(
        &mut self,
        origin: Span,
        name: Symbol,
        generic_params: &[Token],
    ) -> StructId {
        let id = StructId(self.structs.len());
        self.structs.insert(
            id,
            StructType::new(id, name, origin, convert_generics(generic_params)),
        );
        id
    }

    #[must_use]
    pub fn declare_enum(&mut self, origin: Span, name: Symbol, generic_params: &[Token]) -> EnumId {
        let id = EnumId(self.enums.len());
        self.enums.insert(
            id,
            EnumType::new(id, name, origin, convert_generics(generic_params)),
        );
        id
    }

    #[must_use]
    pub fn declare_interface(&mut self, name: Symbol, origin: Span) -> InterfaceId {
        let id = InterfaceId(self.interfaces.len());
        self.interfaces.insert(
            id,
            InterfaceType {
                id,
                origin,
                name,
                methods: HashMap::new(),
            },
        );
        id
    }

    pub fn define_struct(&mut self, id: StructId, fields_map: HashMap<Symbol, (usize, Type)>) {
        if let Some(s) = self.structs.get_mut(&id) {
            let fields = fields_map
                .iter()
                .map(|(k, (idx, _))| (k.clone(), *idx))
                .collect();

            let mut vec_fields = vec![None; fields_map.len()];
            for (k, (idx, t)) in fields_map {
                if idx < vec_fields.len() {
                    vec_fields[idx] = Some((k, t));
                }
            }
            s.init(
                fields,
                vec_fields.into_iter().map(|opt| opt.unwrap()).collect(),
            );
        }
    }
    pub fn define_enum(&mut self, id: &EnumId, variants: HashMap<Symbol, (usize, Type)>) {
        if let Some(e) = self.enums.get_mut(id) {
            let new_variants = variants
                .iter()
                .map(|(k, (idx, _))| (k.clone(), *idx))
                .collect();
            let mut vec_variants = vec![None; variants.len()];
            for (k, (idx, t)) in variants {
                if idx < vec_variants.len() {
                    vec_variants[idx] = Some((k, t));
                }
            }
            e.init(
                new_variants,
                vec_variants.into_iter().map(|opt| opt.unwrap()).collect(),
            );
        }
    }

    pub fn define_interface(&mut self, id: &InterfaceId, methods: HashMap<String, (usize, Type)>) {
        if let Some(e) = self.interfaces.get_mut(id) {
            e.methods = methods;
        }
    }

    pub fn define_impl(&mut self, type_id: NameTypeId, iface_id: InterfaceId) {
        self.impls
            .insert((type_id, iface_id), self.impls.len() as u32);
    }

    pub fn get_struct(&self, id: StructId) -> &StructType {
        self.structs.get(&id).expect("Invalid Id issued!")
    }

    pub fn get_interface(&self, id: InterfaceId) -> &InterfaceType {
        self.interfaces.get(&id).expect("Invalid Id issued!")
    }

    pub fn get_enum(&self, id: EnumId) -> &EnumType {
        self.enums.get(&id).expect("Invalid Id issued!")
    }

    pub fn get_generic(&self, id: GenericTypeId) -> &(Span, Symbol) {
        self.generics.get(&id).expect("Invalid Id issued!")
    }

    pub fn get_blueprint(&self, id: NameTypeId) -> TypeBlueprint {
        match id {
            NameTypeId::Struct(id) => TypeBlueprint::Struct {
                id,
                arity: self.get_struct(id).generic_count(),
            },
            NameTypeId::Enum(id) => TypeBlueprint::Enum {
                id,
                arity: self.get_enum(id).generic_count(),
            },
            NameTypeId::Interface(id) => TypeBlueprint::Interface { id },
            NameTypeId::Generic(_) => {
                todo!()
            }
            NameTypeId::Primitive(id) => {
                let ty = match id {
                    PrimitiveTypeId::Number => Type::Number,
                    PrimitiveTypeId::String => Type::String,
                    PrimitiveTypeId::Boolean => Type::Boolean,
                    PrimitiveTypeId::Any => Type::Any,
                    PrimitiveTypeId::Never => Type::Never,
                    PrimitiveTypeId::Void => Type::Void,
                };
                TypeBlueprint::Primitive(ty)
            }
        }
    }

    pub fn get_generic_count_by_name(&self, id: NameTypeId) -> usize {
        self.get_generic_param_names(id).len()
    }

    pub fn register_method(&mut self, info: ImplMethod) -> MethodId {
        let method_id = MethodId(self.methods.len());
        self.methods.insert(method_id, info);
        method_id
    }

    pub fn get_method_info(&self, method_id: MethodId) -> &ImplMethod {
        self.methods.get(&method_id).expect("Invalid Id issued!")
    }

    pub fn get_origin(&self, id: NameTypeId) -> Option<Span> {
        match id {
            NameTypeId::Struct(id) => {
                let origin = self.get_struct(id).origin;
                if origin == Span::default() {
                    None
                } else {
                    Some(origin)
                }
            }
            NameTypeId::Enum(id) => Some(self.get_enum(id).origin),
            NameTypeId::Interface(id) => Some(self.get_interface(id).origin),
            NameTypeId::Generic(id) => Some(self.get_generic(id).0),
            NameTypeId::Primitive(_) => None,
        }
    }

    pub(crate) fn get_name(&self, id: NameTypeId) -> Symbol {
        match id {
            NameTypeId::Struct(id) => self.get_struct(id).name.clone(),
            NameTypeId::Enum(id) => self.get_enum(id).name.clone(),
            NameTypeId::Interface(id) => self.get_interface(id).name.clone(),
            NameTypeId::Generic(id) => self.get_generic(id).1.clone(),
            NameTypeId::Primitive(id) => match id {
                PrimitiveTypeId::Number => "number".into(),
                PrimitiveTypeId::String => "string".into(),
                PrimitiveTypeId::Boolean => "boolean".into(),
                PrimitiveTypeId::Any => "any".into(),
                PrimitiveTypeId::Never => "never".into(),
                PrimitiveTypeId::Void => "void".into(),
            },
        }
    }

    pub fn is_map(&self, id: StructId) -> bool {
        self.builtins.map_id == id
    }

    pub fn is_list(&self, id: StructId) -> bool {
        self.builtins.list_id == id
    }

    pub fn view_builtins(&self) -> &BuiltinTypes {
        &self.builtins
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MethodId(usize);

/// Converts the given generic type parameters and concrete type arguments into a substitution map.
/// Used when there is a known concrete instance
pub fn make_substitution_map(params: &[Symbol], args: &[Type]) -> HashMap<Symbol, Type> {
    debug_assert_eq!(params.len(), args.len());
    params
        .iter()
        .enumerate()
        .map(|(idx, s)| (s.clone(), args[idx].clone()))
        .collect()
}
