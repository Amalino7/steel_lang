use crate::typechecker::Symbol;
use crate::typechecker::core::types::NameTypeId;
use crate::typechecker::system::MethodId;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

/// Methods grouped by the type they belong to.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MethodTable {
    by_type: HashMap<NameTypeId, HashMap<Symbol, MethodId>>,
}

impl MethodTable {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn declare(
        &mut self,
        ty_id: NameTypeId,
        name: Symbol,
        method_id: MethodId,
    ) -> Result<(), MethodId> {
        match self.by_type.entry(ty_id).or_default().entry(name) {
            Entry::Occupied(old) if *old.get() != method_id => Err(*old.get()),
            Entry::Occupied(_) => Ok(()),
            Entry::Vacant(slot) => {
                slot.insert(method_id);
                Ok(())
            }
        }
    }

    pub fn lookup(&self, ty_id: NameTypeId, name: &str) -> Option<MethodId> {
        self.by_type.get(&ty_id)?.get(name).copied()
    }

    pub fn has_methods_for(&self, ty_id: NameTypeId) -> bool {
        self.by_type.get(&ty_id).is_some_and(|m| !m.is_empty())
    }

    /// Methods of one type, in no particular order.
    pub fn methods_of(&self, ty_id: NameTypeId) -> impl Iterator<Item = (&Symbol, MethodId)> {
        self.by_type
            .get(&ty_id)
            .into_iter()
            .flat_map(|m| m.iter().map(|(name, id)| (name, *id)))
    }

    pub fn names_of(&self, ty_id: NameTypeId) -> impl Iterator<Item = &Symbol> {
        self.methods_of(ty_id).map(|(name, _)| name)
    }

    /// Every method of every type, in no particular order.
    pub fn iter(&self) -> impl Iterator<Item = (NameTypeId, &Symbol, MethodId)> {
        self.by_type
            .iter()
            .flat_map(|(ty, m)| m.iter().map(move |(name, id)| (*ty, name, *id)))
    }

    pub fn retain(&mut self, mut keep: impl FnMut(MethodId) -> bool) {
        for methods in self.by_type.values_mut() {
            methods.retain(|_, id| keep(*id));
        }
        self.by_type.retain(|_, methods| !methods.is_empty());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typechecker::core::types::PrimitiveTypeId;
    use crate::typechecker::system::{ImplMethod, TypeSystem};

    fn ids() -> (NameTypeId, NameTypeId, MethodId, MethodId) {
        // MethodIds can only be minted by a TypeSystem, so register two dummy methods.
        use crate::compiler::analysis::ResolvedVar;
        use crate::scanner::Span;
        use crate::typechecker::core::types::Type;
        let mut sys = TypeSystem::new();
        let mut register = || {
            sys.register_method(ImplMethod {
                self_type: Type::Void,
                impl_generic_count: 0,
                func_type: Type::Void,
                location: ResolvedVar::Global(0),
                origin: Span::default(),
                is_public: true,
            })
        };
        let (a, b) = (register(), register());
        (
            PrimitiveTypeId::Number.into(),
            PrimitiveTypeId::String.into(),
            a,
            b,
        )
    }

    #[test]
    fn declare_detects_conflicts_but_allows_redeclaring_the_same_method() {
        let (num, _, a, b) = ids();
        let mut table = MethodTable::new();
        assert_eq!(table.declare(num, "f".into(), a), Ok(()));
        assert_eq!(table.declare(num, "f".into(), a), Ok(()));
        assert_eq!(table.declare(num, "f".into(), b), Err(a));
        assert_eq!(table.lookup(num, "f"), Some(a));
    }

    #[test]
    fn per_type_queries_only_see_that_type() {
        let (num, string, a, b) = ids();
        let mut table = MethodTable::new();
        table.declare(num, "f".into(), a).unwrap();
        table.declare(string, "g".into(), b).unwrap();

        let names: Vec<_> = table.names_of(num).map(|n| n.to_string()).collect();
        assert_eq!(names, ["f"]);
        assert_eq!(table.lookup(string, "f"), None);
        assert!(table.has_methods_for(string));
        assert_eq!(table.iter().count(), 2);
    }

    #[test]
    fn retain_drops_empty_types() {
        let (num, string, a, b) = ids();
        let mut table = MethodTable::new();
        table.declare(num, "f".into(), a).unwrap();
        table.declare(string, "g".into(), b).unwrap();
        table.retain(|id| id == a);
        assert!(table.has_methods_for(num));
        assert!(!table.has_methods_for(string));
    }
}
