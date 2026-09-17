use crate::typechecker::Symbol;
use crate::typechecker::core::types::{NameTypeId, PrimitiveTypeId, Type};
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
    generics: Vec<Symbol>,
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
        generics: Vec<Symbol>,
        self_type: Option<Type>,
        kind: TypeScopeKind,
    ) -> Result<(), Vec<TypeScopeError>> {
        let mut errors: Vec<TypeScopeError> = Vec::new();
        for (idx, generic) in generics.iter().enumerate() {
            if self.is_generic(generic.as_ref()).is_some() {
                errors.push((generic.clone(), idx));
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
            for g in &scope.generics {
                if g.as_ref() == name {
                    return Some(g.clone());
                }
            }
        }
        None
    }

    pub fn active_generics(&self) -> Vec<Symbol> {
        let mut generics = vec![];
        for scope in self.scopes.iter().rev() {
            let mut new_generics = scope.generics.clone();
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

    pub fn declare_global(&mut self, name: Symbol, id: NameTypeId) {
        // TODO naming conflicts
        self.globals.insert(name, id);
    }
    pub fn lookup_type(&self, name: &str) -> Option<NameTypeId> {
        // TODO Wire more logic
        self.globals.get(name).cloned()
    }

    pub fn lookup_method(&self, ty_name: &str, method_name: &str) -> Option<&MethodId> {
        let ty_id = self.globals.get(ty_name)?;
        self.method_map.get(&(*ty_id, method_name.into()))
    }

    pub fn declare_method(&mut self, ty_id: NameTypeId, method_name: Symbol, method_id: MethodId) {
        self.method_map.insert((ty_id, method_name), method_id);
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
