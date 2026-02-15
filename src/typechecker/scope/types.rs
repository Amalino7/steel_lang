use crate::typechecker::core::types::Type;
use crate::typechecker::Symbol;

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
    scopes: Vec<TypeScope>,
}

impl TypeScopeManager {
    pub fn new() -> Self {
        TypeScopeManager { scopes: vec![] }
    }
    pub fn begin_type_scope(
        &mut self,
        generics: &[Symbol],
        self_type: Option<Type>,
        kind: TypeScopeKind,
    ) {
        self.scopes.push(TypeScope {
            generics: generics.to_vec(),
            self_type,
            kind,
        });
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
    pub fn all_generics(&self) -> Vec<Symbol> {
        let mut generics = vec![];
        for scope in self.scopes.iter().rev() {
            generics.extend(scope.generics.clone());
            if scope.kind == TypeScopeKind::Function {
                break;
            }
        }
        generics
    }

    pub fn get_self_type(&self) -> Option<&Type> {
        self.scopes.iter().rev().find_map(|s| s.self_type.as_ref())
    }
}
