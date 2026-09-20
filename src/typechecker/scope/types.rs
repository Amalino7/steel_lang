use crate::typechecker::Symbol;
use crate::typechecker::core::types::{GenericTypeId, NameTypeId, PrimitiveTypeId, Type};
use crate::typechecker::system::MethodId;
use std::collections::HashMap;

#[derive(PartialEq)]
pub enum TypeScopeKind {
    Type,
    Function,
    Impl,
}

struct TypeScope {
    kind: TypeScopeKind,
    generics: HashMap<Symbol, GenericTypeId>,
    self_type: Option<Type>,
}

pub struct TypeScopeManager {
    globals: HashMap<Symbol, NameTypeId>,
    method_map: HashMap<(NameTypeId, Symbol), MethodId>,
    scopes: Vec<TypeScope>,
}

pub type TypeScopeError = (Symbol, usize);
impl TypeScopeManager {
    pub fn new() -> Self {
        TypeScopeManager {
            globals: Self::primitives(),
            scopes: vec![],
            method_map: HashMap::new(),
        }
    }
    pub fn begin_type_scope(
        &mut self,
        generics: HashMap<Symbol, GenericTypeId>,
        self_type: Option<Type>,
        kind: TypeScopeKind,
    ) -> Result<(), Vec<TypeScopeError>> {
        let mut errors: Vec<TypeScopeError> = Vec::new();
        for (idx, generic) in generics.iter().enumerate() {
            if self.is_generic(generic.0).is_some() {
                errors.push((generic.0.clone(), idx));
            }
        }

        self.scopes.push(TypeScope {
            generics,
            self_type,
            kind,
        });
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }

    pub fn end_type_scope(&mut self) {
        self.scopes.pop().expect("No type scope to pop.");
    }

    pub fn is_generic(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some((name, _)) = scope.generics.get_key_value(name) {
                return Some(name.clone());
            }
        }
        None
    }

    pub fn active_generics(&self) -> Vec<GenericTypeId> {
        let mut generics = vec![];
        for scope in self.scopes.iter().rev() {
            let mut new_generics = scope
                .generics
                .values()
                .copied()
                .collect::<Vec<GenericTypeId>>();

            new_generics.extend(generics);
            generics = new_generics;
            if scope.kind == TypeScopeKind::Function {
                break;
            }
        }
        generics
    }

    pub fn get_self_type(&self) -> Option<&Type> {
        self.scopes.iter().rev().find_map(|s| s.self_type.as_ref())
    }

    pub fn declare_global(&mut self, name: Symbol, id: NameTypeId) -> Result<(), NameTypeId> {
        if let Some(old_id) = self.globals.insert(name, id)
            && old_id != id
        {
            Err(old_id)
        } else {
            Ok(())
        }
    }

    pub fn lookup_type(&self, name: &str) -> Option<NameTypeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.generics.get(name) {
                return Some((*id).into());
            }
        }
        self.globals.get(name).cloned()
    }

    pub fn lookup_method(&self, ty_id: NameTypeId, method_name: &str) -> Option<&MethodId> {
        self.method_map.get(&(ty_id, method_name.into()))
    }

    pub fn declare_method(
        &mut self,
        ty_id: NameTypeId,
        method_name: Symbol,
        method_id: MethodId,
    ) -> Result<(), MethodId> {
        let old = self.method_map.insert((ty_id, method_name), method_id);
        if let Some(old_id) = old
            && old_id != method_id
        {
            Err(method_id)
        } else {
            Ok(())
        }
    }

    /// Get all method names for a given type (for suggestions)
    pub fn get_methods_for_type(&self, type_name: NameTypeId) -> Vec<Symbol> {
        let mut methods = Vec::new();
        for ((ty_id, name), _) in self.method_map.iter() {
            if *ty_id == type_name {
                methods.push(name.clone());
            }
        }

        methods
    }

    pub fn export_types(&mut self) -> HashMap<Symbol, NameTypeId> {
        std::mem::take(&mut self.globals)
    }

    pub fn export_methods(&mut self) -> HashMap<(NameTypeId, Symbol), MethodId> {
        std::mem::take(&mut self.method_map)
    }

    fn primitives() -> HashMap<Symbol, NameTypeId> {
        HashMap::from([
            ("number".into(), PrimitiveTypeId::Number.into()),
            ("string".into(), PrimitiveTypeId::String.into()),
            ("boolean".into(), PrimitiveTypeId::Boolean.into()),
            ("any".into(), PrimitiveTypeId::Any.into()),
            ("never".into(), PrimitiveTypeId::Never.into()),
            ("void".into(), PrimitiveTypeId::Void.into()),
        ])
    }
}
