use crate::scanner::{Span, Token};
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::types::type_defs::{EnumType, InterfaceType, StructType};
use crate::typechecker::core::types::Symbol;
use crate::typechecker::core::types::Type;
use crate::typechecker::core::types::Type::GenericParam;
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::resolver::convert_generics;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeSystem {
    structs: HashMap<Symbol, StructType>,
    interfaces: HashMap<Symbol, InterfaceType>,
    impls: HashMap<(Symbol, Symbol), u32>,
    enums: HashMap<Symbol, EnumType>,
    primitives: HashMap<Symbol, Type>,
}

impl TypeSystem {
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
        primitives.insert("List".into(), Type::new_list(GenericParam("T".into())));
        primitives.insert(
            "Map".into(),
            Type::new_map(GenericParam("K".into()), GenericParam("V".into())),
        );
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
        } else if name == "List" {
            vec!["T".into()]
        } else if name == "Map" {
            vec!["K".into(), "V".into()]
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

    pub fn get_generic_count_by_name(&self, name: &str) -> usize {
        self.get_generic_param_names(name).len()
    }
    // TODO rethink logic, missing generics
    pub fn get_owned_name(&self, name: &str) -> Option<Symbol> {
        Some(if let Some(s) = self.structs.get(name) {
            s.name.clone()
        } else if let Some(e) = self.enums.get(name) {
            e.name.clone()
        } else if let Some(i) = self.interfaces.get(name) {
            i.name.clone()
        } else if let Some(p) = self.primitives.get_key_value(name) {
            p.0.clone()
        } else {
            return None;
        })
    }

    pub fn instantiate(
        &self,
        name: &str,
        generics: Vec<Type>,
        source: Span,
    ) -> Result<Type, TypeCheckerError> {
        if let Some(primitive) = self.get_primitive(name) {
            return self.instantiate_primitive(name, primitive, generics, source);
        }

        if let Some(struct_type) = self.get_struct(name) {
            check_generic_arity(name, struct_type.generic_count(), generics.len(), source)?;
            return Ok(Type::Struct(struct_type.name.clone(), Rc::new(generics)));
        }

        if let Some(enum_type) = self.get_enum(name) {
            check_generic_arity(name, enum_type.generic_count(), generics.len(), source)?;
            return Ok(Type::Enum(enum_type.name.clone(), Rc::new(generics)));
        }

        if let Some(iface) = self.get_interface(name) {
            check_generic_arity(name, 0, generics.len(), source)?;
            return Ok(Type::Interface(iface.name.clone()));
        }
        Err(TypeCheckerError::UndefinedType {
            name: name.to_string(),
            span: source,
            message: "Could not find type with that name.",
        })
    }

    fn instantiate_primitive(
        &self,
        name: &str,
        primitive: &Type,
        generics: Vec<Type>,
        source: Span,
    ) -> Result<Type, TypeCheckerError> {
        match primitive {
            Type::List(_) => {
                check_generic_arity(name, 1, generics.len(), source)?;
                Ok(Type::new_list(generics[0].clone()))
            }
            Type::Map(_) => {
                check_generic_arity(name, 2, generics.len(), source)?;
                Ok(Type::new_map(generics[0].clone(), generics[1].clone()))
            }
            _ => Ok(primitive.clone()),
        }
    }
}

fn check_generic_arity(
    type_name: &str,
    generics_expected: usize,
    generics_provided: usize,
    span: Span,
) -> Result<(), TypeCheckerError> {
    if generics_provided != generics_expected {
        Err(TypeCheckerError::GenericCountMismatch {
            span,
            found: generics_provided,
            expected: generics_expected,
            type_name: type_name.to_string(),
        })
    } else {
        Ok(())
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
                    InferenceContext::new_type_var(ctx)
                } else {
                    Type::Unknown
                }
            });
            (s.clone(), type_var)
        })
        .collect()
}
