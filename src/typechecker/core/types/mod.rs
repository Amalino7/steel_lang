mod display;

use crate::scanner::Span;
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::inference::InferenceContext;
use crate::typechecker::system::{generics_to_map, TypeSystem};
use std::collections::HashMap;
use std::rc::Rc;

pub type Symbol = Rc<str>;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub is_vararg: bool,
    pub params: Vec<(Symbol, Type)>,
    pub return_type: Type,
    pub type_params: Vec<Symbol>,
}

impl FunctionType {
    pub fn is_static(&self) -> bool {
        self.params
            .first()
            .map(|param| param.0.as_ref() != "self")
            .unwrap_or(true)
    }
}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        self.return_type == other.return_type
            && self.params.len() == other.params.len()
            && self.is_vararg == other.is_vararg
            && self
                .params
                .iter()
                .zip(&other.params)
                .all(|(a, b)| a.1 == b.1)
    }
}

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
    pub fn get_variant(&self, name: &str, generic_args: &GenericArgs) -> Option<Type> {
        let map = generics_to_map(&self.generic_params, generic_args, None);
        self.variants.get(name).map(|idx| {
            let raw_ty = self.ordered_variants[*idx].1.clone();
            raw_ty.generic_to_concrete(&map)
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

#[derive(Debug, PartialEq, Clone)]
pub struct TupleType {
    pub types: Vec<Type>,
}

// TODO use Rc<[Type]>
pub type GenericArgs = Rc<Vec<Type>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Nil,
    Number,
    String,
    Boolean,
    Void,
    Never,
    Error,
    Infer(u32),
    Optional(Box<Type>),
    Unknown,
    Function(Rc<FunctionType>),
    Tuple(Rc<TupleType>),
    GenericParam(Symbol),
    Metatype(Symbol, GenericArgs),
    Struct(Symbol, GenericArgs),
    Interface(Symbol, GenericArgs),
    Enum(Symbol, GenericArgs),

    // TODO use Rc<Type> or migrate to struct.
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Any,
}

impl Type {
    pub fn transform(
        self,
        f: &impl Fn(Type) -> Type,
        u: &impl Fn(&[Symbol]) -> Vec<Symbol>,
    ) -> Type {
        let transformed = match self {
            Type::Nil
            | Type::Number
            | Type::String
            | Type::Boolean
            | Type::Void
            | Type::Never
            | Type::Error
            | Type::Infer(_)
            | Type::Unknown
            | Type::Any
            | Type::GenericParam(_)
            | Type::Metatype(_, _) => self,

            Type::Optional(inner) => Type::Optional(Box::new(inner.transform(f, u))),
            Type::List(inner) => Type::List(Box::new(inner.transform(f, u))),
            Type::Map(k, v) => Type::Map(Box::new(k.transform(f, u)), Box::new(v.transform(f, u))),
            Type::Tuple(tt) => {
                let types = tt.types.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Tuple(Rc::new(TupleType { types }))
            }
            Type::Function(ft) => {
                let params = ft
                    .params
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.clone().transform(f, u)))
                    .collect();
                let ret = ft.return_type.clone().transform(f, u);
                Type::new_general_function(params, ret, u(&ft.type_params), ft.is_vararg)
            }
            Type::Struct(name, args) => {
                let args = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Struct(name.clone(), Rc::new(args))
            }
            Type::Interface(name, args) => {
                let args = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Interface(name.clone(), Rc::new(args))
            }
            Type::Enum(name, args) => {
                let args = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Enum(name.clone(), Rc::new(args))
            }
        };
        f(transformed)
    }

    pub fn any(&self, f: &impl Fn(&Type) -> bool) -> bool {
        if f(self) {
            return true;
        }
        match self {
            Type::Optional(inner) | Type::List(inner) => inner.any(f),
            Type::Map(k, v) => k.any(f) || v.any(f),
            Type::Tuple(tt) => tt.types.iter().any(|t| t.any(f)),
            Type::Function(ft) => ft.params.iter().any(|(_, t)| t.any(f)) || ft.return_type.any(f),
            Type::Struct(_, args) | Type::Interface(_, args) | Type::Enum(_, args) => {
                args.iter().any(|t| t.any(f))
            }
            _ => false,
        }
    }

    pub fn all(&self, f: &impl Fn(&Type) -> bool) -> bool {
        if !f(self) {
            return false;
        }
        match self {
            Type::Optional(inner) | Type::List(inner) => inner.all(f),
            Type::Map(k, v) => k.all(f) && v.all(f),
            Type::Tuple(tt) => tt.types.iter().all(|t| t.all(f)),
            Type::Function(ft) => ft.params.iter().all(|(_, t)| t.all(f)) && ft.return_type.all(f),
            Type::Struct(_, args) | Type::Interface(_, args) | Type::Enum(_, args) => {
                args.iter().all(|t| t.all(f))
            }
            _ => true,
        }
    }
}

impl Type {
    pub fn is_concrete(&self) -> bool {
        self.all(&|ty| !matches!(ty, Type::Infer(_) | Type::Unknown | Type::Metatype(..)))
    }

    pub fn unwrap_optional_safe(&self, safe: bool, span: Span) -> Result<Type, TypeCheckerError> {
        if safe {
            match self {
                Type::Optional(inner) => Ok(inner.as_ref().clone()),
                _ => Err(TypeCheckerError::TypeMismatch {
                    expected: Type::Optional(Box::new(Type::Any)),
                    found: self.clone(),
                    span,
                    message: "Cannot access safe navigation of non-optional type. Remove ?.",
                }),
            }
        } else {
            Ok(self.clone())
        }
    }

    /// Unwraps optional for safe access, emitting a warning instead of error if used on non-optional
    pub fn unwrap_optional_safe_warn(
        &self,
        safe: bool,
        span: Span,
        warnings: &mut Vec<crate::typechecker::core::error::TypeCheckerWarning>,
    ) -> Type {
        if safe {
            match self {
                Type::Optional(inner) => inner.as_ref().clone(),
                _ => {
                    warnings.push(crate::typechecker::core::error::TypeCheckerWarning::SafeAccessOnNonOptional { span });
                    self.clone()
                }
            }
        } else {
            self.clone()
        }
    }

    pub fn wrap_in_optional(self) -> Type {
        match self {
            Type::Optional(_) => self,
            _ => Type::Optional(Box::new(self)),
        }
    }

    pub fn get_name(&self) -> Option<&str> {
        match self {
            Type::Error => None,
            Type::Infer(_) => None,
            Type::Interface(name, _) => Some(name),
            Type::Number => Some("number"),
            Type::String => Some("string"),
            Type::Boolean => Some("boolean"),
            Type::Void => Some("void"),
            Type::Never => Some("never"),
            Type::Function(_) => None,
            Type::Unknown => None,
            Type::Struct(name, _) => Some(name),
            Type::Any => None,
            Type::Optional(_) => None,
            Type::Nil => Some("nil"),
            Type::GenericParam(_) => None,
            Type::Enum(name, _) => Some(name),
            Type::Tuple(_) => None,
            Type::Metatype(_, _) => None,
            Type::List(_) => Some("List"),
            Type::Map(_, _) => Some("Map"),
        }
    }

    pub fn new_general_function(
        params: Vec<(Symbol, Type)>,
        return_type: Type,
        type_params: Vec<Symbol>,
        is_vararg: bool,
    ) -> Self {
        Type::Function(Rc::new(FunctionType {
            is_vararg,
            params,
            return_type,
            type_params,
        }))
    }

    pub fn new_function(
        params: Vec<(Symbol, Type)>,
        return_type: Type,
        type_params: Vec<Symbol>,
    ) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            params,
            return_type,
            type_params,
        }))
    }

    pub fn new_tuple(elements: Vec<Type>) -> Type {
        Type::Tuple(Rc::new(TupleType { types: elements }))
    }

    pub fn new_vararg(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_vararg: true,
            params: param_types.into_iter().map(|e| ("_".into(), e)).collect(),
            return_type,
            type_params: vec![],
        }))
    }

    // TODO change can compare logic
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

    pub fn generic_to_concrete(self, map: &HashMap<Symbol, Type>) -> Type {
        self.transform(
            &|ty| match ty {
                Type::GenericParam(ref sym) => match map.get(sym) {
                    Some(t) if t != &Type::Unknown => t.clone(),
                    _ => ty,
                },
                other => other,
            },
            &|type_params| {
                type_params
                    .iter()
                    .filter(|s| !map.contains_key(*s))
                    .cloned()
                    .collect()
            },
        )
    }

    // TODO consider using transform
    // pub fn generic_to_concrete(self, generics_map: &HashMap<Symbol, Type>) -> Type {
    //     match self {
    //         Type::Metatype(_, _) => self,
    //         Type::Nil
    //         | Type::Number
    //         | Type::String
    //         | Type::Boolean
    //         | Type::Void
    //         | Type::Unknown
    //         | Type::Never
    //         | Type::Infer(_)
    //         | Type::Error
    //         | Type::Any => self,
    //         Type::Optional(inner) => {
    //             Type::Optional(Box::new(Self::generic_to_concrete(*inner, generics_map)))
    //         }
    //         Type::List(inner) => {
    //             Type::List(Box::new(Self::generic_to_concrete(*inner, generics_map)))
    //         }
    //         Type::Map(key_type, value_type) => Type::Map(
    //             Box::new(Self::generic_to_concrete(*key_type, generics_map)),
    //             Box::new(Self::generic_to_concrete(*value_type, generics_map)),
    //         ),
    //         Type::Function(func_type) => {
    //             if func_type.is_vararg {
    //                 return Type::Function(func_type);
    //             }
    //             let params = func_type
    //                 .params
    //                 .iter()
    //                 .map(|(s, t)| (s.to_string(), t.clone().generic_to_concrete(generics_map)))
    //                 .collect();
    //             let return_type = func_type
    //                 .return_type
    //                 .clone()
    //                 .generic_to_concrete(generics_map);
    // Remove generics>
    // Type::new_function(
    //     params,
    //     return_type,
    //     func_type
    //         .type_params
    //         .iter()
    //         .filter(|s| !generics_map.contains_key(*s))
    //         .cloned()
    //         .collect(),
    // )
    //         }
    //         Type::Tuple(types) => {
    //             let new_types = types
    //                 .types
    //                 .iter()
    //                 .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
    //                 .collect();
    //             Type::Tuple(Rc::new(TupleType { types: new_types }))
    //         }
    //         Type::GenericParam(generic) => {
    //             if let Some(new_type) = generics_map.get(&generic) {
    //                 if new_type == &Type::Unknown {
    //                     Type::GenericParam(generic)
    //                 } else {
    //                     new_type.clone()
    //                 }
    //             } else {
    //                 Type::GenericParam(generic)
    //             }
    //         }
    //         Type::Struct(name, args) => {
    //             let resolved_args = args
    //                 .iter()
    //                 .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
    //                 .collect();
    //             Type::Struct(name, Rc::new(resolved_args))
    //         }
    //         Type::Interface(name, args) => {
    //             let resolved_args = args
    //                 .iter()
    //                 .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
    //                 .collect();
    //             Type::Interface(name, Rc::new(resolved_args))
    //         }
    //         Type::Enum(name, args) => {
    //             let resolved_args = args
    //                 .iter()
    //                 .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
    //                 .collect();
    //             Type::Enum(name, Rc::new(resolved_args))
    //         }
    //     }
    // }
}
