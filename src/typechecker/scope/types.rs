use crate::typechecker::Symbol;
use crate::typechecker::core::types::{GenericTypeId, NameTypeId, PrimitiveTypeId, Type};
use crate::typechecker::method_table::MethodTable;
use crate::typechecker::scope::binding::{Binding, Origin};
use crate::typechecker::system::MethodId;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

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
    globals: HashMap<Symbol, Binding<NameTypeId>>,
    /// Extension methods in scope.
    extensions: MethodTable,
    scopes: Vec<TypeScope>,
}

pub type TypeScopeError = (Symbol, usize);
impl TypeScopeManager {
    pub fn new() -> Self {
        TypeScopeManager {
            globals: Self::primitives(),
            scopes: vec![],
            extensions: MethodTable::new(),
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

    /// Binds `name`; rebinding it to a different type returns the old binding for error handling.
    /// The old binding wins.
    pub fn declare_global(
        &mut self,
        name: Symbol,
        binding: Binding<NameTypeId>,
    ) -> Result<(), Binding<NameTypeId>> {
        match self.globals.entry(name) {
            Entry::Occupied(old) if old.get().target != binding.target => Err(*old.get()),
            Entry::Occupied(_) => Ok(()),
            Entry::Vacant(slot) => {
                slot.insert(binding);
                Ok(())
            }
        }
    }

    pub fn lookup_type(&self, name: &str) -> Option<NameTypeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.generics.get(name) {
                return Some((*id).into());
            }
        }
        self.globals.get(name).map(|binding| binding.target)
    }

    pub fn lookup_method(&self, ty_id: NameTypeId, method_name: &str) -> Option<Binding<MethodId>> {
        self.extensions.lookup(ty_id, method_name)
    }

    pub fn declare_method(
        &mut self,
        ty_id: NameTypeId,
        method_name: Symbol,
        binding: Binding<MethodId>,
    ) -> Result<(), Binding<MethodId>> {
        self.extensions.declare(ty_id, method_name, binding)
    }

    /// Get all method names for a given type (for suggestions)
    pub fn get_methods_for_type(&self, type_name: NameTypeId) -> Vec<Symbol> {
        self.extensions
            .method_names_of(type_name)
            .cloned()
            .collect()
    }

    pub fn export_types(&mut self) -> HashMap<Symbol, Binding<NameTypeId>> {
        std::mem::take(&mut self.globals)
    }

    pub fn export_methods(&mut self) -> MethodTable {
        std::mem::take(&mut self.extensions)
    }

    fn primitives() -> HashMap<Symbol, Binding<NameTypeId>> {
        [
            ("number", PrimitiveTypeId::Number),
            ("string", PrimitiveTypeId::String),
            ("boolean", PrimitiveTypeId::Boolean),
            ("any", PrimitiveTypeId::Any),
            ("never", PrimitiveTypeId::Never),
            ("void", PrimitiveTypeId::Void),
        ]
        .into_iter()
        .map(|(name, id)| (name.into(), Binding::new(id.into(), Origin::Builtin)))
        .collect()
    }
}
