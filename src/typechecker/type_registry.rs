use crate::scanner::{Span, Token};
use crate::typechecker::type_system::generics_to_map;
use crate::typechecker::types::Type::GenericParam;
use crate::typechecker::types::{EnumType, InterfaceType, StructType, Type};
use crate::typechecker::Symbol;
use std::collections::HashMap;

pub struct TypeRegistry {
    structs: HashMap<Symbol, StructType>,
    interfaces: HashMap<Symbol, InterfaceType>,
    impls: HashMap<(Symbol, Symbol), u32>,
    enums: HashMap<Symbol, EnumType>,
    primitives: HashMap<Symbol, Type>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashMap::new(),
            enums: HashMap::new(),
            primitives: Self::primitives(),
        }
    }
    fn primitives() -> HashMap<Symbol, Type> {
        let mut primitives = HashMap::new();
        primitives.insert("number".into(), Type::Number);
        primitives.insert("string".into(), Type::String);
        primitives.insert("boolean".into(), Type::Boolean);
        primitives.insert("void".into(), Type::Void);
        primitives.insert("any".into(), Type::Any);
        primitives.insert("never".into(), Type::Never);
        primitives.insert(
            "List".into(),
            Type::List(Box::new(GenericParam("Val".into()))),
        );
        primitives.insert(
            "Map".into(),
            Type::Map(
                Box::new(GenericParam("Key".into())),
                Box::new(GenericParam("Val".into())),
            ),
        );
        primitives
    }

    pub fn get_vtable_idx(&self, type_name: &str, iface_name: Symbol) -> Option<u32> {
        self.impls.get(&(type_name.into(), iface_name)).copied()
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
}
