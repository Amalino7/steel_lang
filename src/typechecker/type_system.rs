use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{
    EnumType, ExprKind, InterfaceType, StructType, Type, TypedExpr,
};
use crate::typechecker::Symbol;
use std::collections::HashMap;

pub struct TypeSystem {
    structs: HashMap<Symbol, StructType>,
    interfaces: HashMap<Symbol, InterfaceType>,
    impls: HashMap<(Symbol, Symbol), u32>,
    enums: HashMap<Symbol, EnumType>,
}

impl TypeSystem {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            impls: HashMap::new(),
            enums: HashMap::new(),
        }
    }

    // Only the name exists
    pub fn declare_struct(&mut self, name: Symbol) {
        self.structs.insert(
            name.clone(),
            StructType {
                name,
                fields: HashMap::new(),
                ordered_fields: vec![],
            },
        );
    }

    pub fn declare_enum(&mut self, name: Symbol) {
        self.enums.insert(
            name.clone(),
            EnumType {
                name,
                variants: HashMap::new(),
            },
        );
    }

    pub fn define_enum(&mut self, name: &str, variants: HashMap<String, (usize, Vec<Type>)>) {
        self.enums.get_mut(name).map(|e| e.variants = variants);
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
    pub fn verify_assignment(
        &self,
        expected_type: &Type,
        expr: TypedExpr,
        line: u32,
    ) -> Result<TypedExpr, TypeCheckerError> {
        if expected_type == &Type::Any {
            return Ok(expr);
        }
        if let Type::Optional(_) = expected_type {
            if expr.ty == Type::Nil {
                return Ok(expr);
            }
        }

        if *expected_type == expr.ty {
            return Ok(expr);
        }

        let expected_type = if let Type::Optional(inner) = expected_type {
            inner
        } else {
            expected_type
        };

        if *expected_type == expr.ty {
            return Ok(expr);
        }

        if let (Type::Interface(iface_name), Some(name)) = (expected_type, expr.ty.get_name()) {
            if let Some(idx) = self.impls.get(&(name.into(), iface_name.clone())) {
                return Ok(TypedExpr {
                    ty: Type::Interface(iface_name.clone()),
                    line: expr.line,
                    kind: ExprKind::InterfaceUpcast {
                        expr: Box::new(expr),
                        vtable_idx: *idx,
                    },
                });
            }
        }
        Err(TypeCheckerError::TypeMismatch {
            expected: expected_type.clone(),
            found: expr.ty,
            line,
            message: "Type mismatch in assignment or argument passing.",
        })
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
}
