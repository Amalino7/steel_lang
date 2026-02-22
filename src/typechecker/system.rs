use crate::scanner::{Span, Token};
use crate::typechecker::core::types::type_defs::{EnumType, InterfaceType, StructType};
use crate::typechecker::core::types::Symbol;
use crate::typechecker::core::types::Type;
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::resolver::convert_generics;
use std::collections::HashMap;

pub struct TypeSystem {
    structs: HashMap<Symbol, StructType>,
    interfaces: HashMap<Symbol, InterfaceType>,
    impls: HashMap<(Symbol, Symbol), u32>,
    enums: HashMap<Symbol, EnumType>,
    primitives: HashMap<Symbol, Type>,
}

pub enum TypeBlueprint {
    Struct { name: Symbol, arity: usize },
    Enum { name: Symbol, arity: usize },
    Interface { name: Symbol },
    Primitive(Type),
}

impl TypeSystem {
    pub fn new() -> Self {
        Self {
            structs: Self::built_in_structs(),
            interfaces: HashMap::new(),
            impls: HashMap::new(),
            enums: HashMap::new(),
            primitives: Self::primitives(),
        }
    }
    fn built_in_structs() -> HashMap<Symbol, StructType> {
        let mut structs = HashMap::new();
        structs.insert(
            "List".into(),
            StructType::new("List".into(), Span::default(), vec!["Val".into()]),
        );
        structs.insert(
            "Map".into(),
            StructType::new(
                "Map".into(),
                Span::default(),
                vec!["Key".into(), "Val".into()],
            ),
        );
        structs
    }
    fn primitives() -> HashMap<Symbol, Type> {
        let mut primitives = HashMap::new();
        primitives.insert("number".into(), Type::Number);
        primitives.insert("string".into(), Type::String);
        primitives.insert("boolean".into(), Type::Boolean);
        primitives.insert("void".into(), Type::Void);
        primitives.insert("any".into(), Type::Any);
        primitives.insert("never".into(), Type::Never);
        primitives
    }

    pub fn get_vtable_idx(&self, type_name: &str, iface_name: Symbol) -> Option<u32> {
        self.impls.get(&(type_name.into(), iface_name)).copied()
    }

    /// Returns the names of the generic type parameters declared for a named type.
    pub fn get_generic_param_names(&self, name: &str) -> Vec<Symbol> {
        if let Some(s) = self.structs.get(name) {
            s.generic_params().to_vec()
        } else if let Some(e) = self.enums.get(name) {
            e.generic_params().to_vec()
        } else {
            vec![]
        }
    }

    /// Builds a substitution map from a named type and its concrete type arguments.
    pub fn make_generics_map(&self, name: &str, args: &[Type]) -> HashMap<Symbol, Type> {
        let params = self.get_generic_param_names(name);
        generics_to_map(&params, args, None)
    }

    /// Builds a substitution map from a concrete [`Type`].
    pub fn get_generics_map(&self, ty: &Type) -> HashMap<Symbol, Type> {
        ty.get_name()
            .map(|name| self.make_generics_map(name, ty.generic_args()))
            .unwrap_or_default()
    }

    pub fn declare_struct(&mut self, origin: Span, name: Symbol, generic_params: &[Token]) {
        self.structs.insert(
            name.clone(),
            StructType::new(name, origin, convert_generics(generic_params)),
        );
    }

    pub fn declare_enum(&mut self, origin: Span, name: Symbol, generic_params: &[Token]) {
        self.enums.insert(
            name.clone(),
            EnumType::new(name, origin, convert_generics(generic_params)),
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

    pub fn define_struct(&mut self, name: &str, fields_map: HashMap<Symbol, (usize, Type)>) {
        if let Some(s) = self.structs.get_mut(name) {
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
    pub fn define_enum(&mut self, name: &str, variants: HashMap<Symbol, (usize, Type)>) {
        if let Some(e) = self.enums.get_mut(name) {
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
    pub fn get_primitive(&self, name: &str) -> Option<&Type> {
        self.primitives.get(name)
    }

    pub fn get_primitive_name(&self, name: &str) -> Option<Symbol> {
        self.primitives.get_key_value(name).map(|(k, _)| k.clone())
    }

    pub fn resolve_symbol(&self, name: &str) -> Option<Symbol> {
        if let Some(s) = self.structs.get(name) {
            Some(s.name.clone())
        } else if let Some(e) = self.enums.get(name) {
            Some(e.name.clone())
        } else if let Some(i) = self.interfaces.get(name) {
            Some(i.name.clone())
        } else {
            self.get_primitive_name(name)
        }
    }
    pub fn get_blueprint(&self, name: &str) -> Option<TypeBlueprint> {
        if let Some(s) = self.structs.get(name) {
            Some(TypeBlueprint::Struct {
                name: s.name.clone(),
                arity: s.generic_params().len(),
            })
        } else if let Some(e) = self.enums.get(name) {
            Some(TypeBlueprint::Enum {
                name: e.name.clone(),
                arity: e.generic_params().len(),
            })
        } else if let Some(i) = self.interfaces.get(name) {
            Some(TypeBlueprint::Interface {
                name: i.name.clone(),
            })
        } else {
            self.get_primitive(name)
                .map(|p| TypeBlueprint::Primitive(p.clone()))
        }
    }

    pub fn get_generic_count_by_name(&self, name: &str) -> usize {
        self.get_generic_param_names(name).len()
    }
    pub fn get_origin(&self, name: &str) -> Option<Span> {
        if let Some(s) = self.structs.get(name) {
            if s.origin == Span::default() {
                None
            } else {
                Some(s.origin)
            }
        } else if let Some(e) = self.enums.get(name) {
            Some(e.origin)
        } else {
            self.interfaces.get(name).map(|i| i.origin)
        }
    }
}
// TODO consider moving
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
                    ctx.new_named_type_var(s.clone())
                } else {
                    Type::Unknown
                }
            });
            (s.clone(), type_var)
        })
        .collect()
}
