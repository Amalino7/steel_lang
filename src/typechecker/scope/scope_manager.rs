use crate::compiler::analysis::ResolvedVar;
use crate::scanner::Span;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope::variables::{Declaration, VariableContext};
use crate::typechecker::types::Type;
use crate::typechecker::Symbol;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum ScopeKind {
    Global,
    Function,
    Block,
}
struct Scope {
    variables: HashMap<Symbol, VariableContext>,
    kind: ScopeKind,
    last_index: usize,
    max_index: usize,
}

struct TypeScope {
    generics: Vec<Symbol>,
    self_type: Option<Type>,
}

pub struct ScopeManager {
    type_scopes: Vec<TypeScope>,
    scopes: Vec<Scope>,
    closures: Vec<Symbol>,
    generics_raw: Vec<Symbol>,
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            type_scopes: vec![],
            scopes: vec![],
            closures: vec![],
            generics_raw: Vec::new(),
        }
    }

    pub fn begin_type_scope(&mut self, generics: &[Symbol], self_type: Option<Type>) {
        self.type_scopes.push(TypeScope {
            generics: generics.to_vec(),
            self_type,
        });
    }
    pub(crate) fn set_self(&mut self, self_type: Type) {
        if let Some(type_scope) = self.type_scopes.last_mut() {
            type_scope.self_type = Some(self_type)
        }
    }
    pub fn end_type_scope(&mut self) {
        self.type_scopes.pop().expect("No type scope to pop");
    }

    pub fn is_generic(&self, name: &str) -> Option<Symbol> {
        for scope in self.type_scopes.iter().rev() {
            for g in &scope.generics {
                if g.as_ref() == name {
                    return Some(g.clone());
                }
            }
        }
        None
    }
    pub fn all_generics(&self) -> Vec<Symbol> {
        self.type_scopes
            .iter()
            .flat_map(|s| s.generics.iter().cloned())
            .collect()
    }

    pub fn get_self_type(&self) -> Option<&Type> {
        self.type_scopes
            .iter()
            .rev()
            .find_map(|s| s.self_type.as_ref())
    }

    pub fn begin_scope(&mut self, scope_kind: ScopeKind) {
        let last_idx = match scope_kind {
            ScopeKind::Function => 0,
            ScopeKind::Global => 0,
            ScopeKind::Block => self
                .scopes
                .last()
                .filter(|s| s.kind != ScopeKind::Global)
                .map(|s| s.last_index)
                .unwrap_or(0),
        };

        self.scopes.push(Scope {
            max_index: last_idx,
            variables: HashMap::new(),
            kind: scope_kind,
            last_index: last_idx,
        });
    }

    pub fn end_scope(&mut self) -> usize {
        let finished_scope = self.scopes.pop().expect("No scope to end");
        let max = finished_scope.max_index;
        if let Some(parent) = self.scopes.last_mut()
            && parent.kind != ScopeKind::Global
        {
            parent.max_index = parent.max_index.max(max);
        }

        max
    }

    pub fn global_size(&self) -> u32 {
        self.scopes[0].last_index as u32
    }

    pub fn is_global(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn max_index(&self) -> usize {
        self.scopes.last().map(|s| s.max_index).unwrap_or(0)
    }

    pub fn declare(&mut self, decl: Declaration) -> Result<(), TypeCheckerError> {
        let scope = self.scopes.last_mut().expect("No scope active");

        if scope.variables.contains_key(&decl.name) && ScopeKind::Global == scope.kind {
            let prev = &scope.variables[&decl.name];
            return Err(TypeCheckerError::Redeclaration {
                name: decl.name.to_string(),
                span: decl.span,
                original: prev.span,
            });
        }

        scope.variables.insert(
            decl.name.clone(),
            VariableContext::from_declaration(scope.last_index, decl),
        );
        scope.last_index += 1;
        scope.max_index = scope.max_index.max(scope.last_index);
        Ok(())
    }

    pub fn lookup(&mut self, name: &str) -> Option<(&VariableContext, ResolvedVar)> {
        let mut is_closure = false;

        for scope in self.scopes.iter_mut().rev() {
            if let Some(ctx) = scope.variables.get_mut(name) {
                ctx.was_read = true;
                let resolved = if scope.kind == ScopeKind::Global {
                    ResolvedVar::Global(ctx.index as u16)
                } else if is_closure {
                    Self::add_closure_capture(&mut self.closures, ctx.name.clone())
                } else {
                    ResolvedVar::Local(ctx.index as u8)
                };
                return Some((ctx, resolved));
            }

            if matches!(scope.kind, ScopeKind::Function) {
                is_closure = true;
            }
        }
        None
    }

    pub fn widen_type(&mut self, name: &str, broad_type: Type, old_location: ResolvedVar) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(ctx) = scope.variables.get_mut(name) {
                if let ResolvedVar::Local(idx) = old_location {
                    ctx.original_type = None;
                    ctx.type_info = broad_type;
                    ctx.index = idx as usize;
                }
                return;
            }
        }
    }

    #[must_use]
    pub fn refine(&mut self, name: &str, new_type: Type) -> Option<(ResolvedVar, ResolvedVar)> {
        let lookup_res = self.lookup(name);
        let (ctx, resolved) = lookup_res?;

        // Globals cannot be safely refined
        if let ResolvedVar::Global(_) = resolved {
            return None;
        }
        let original_type = ctx.type_info.clone();
        let original_resolved = resolved.clone();

        if let Type::Enum(_, _) = &ctx.type_info {
            let name = ctx.name.clone();
            let new_decl = Declaration {
                mutability: ctx.mutability,
                kind: ctx.kind,
                name: name.clone(),
                type_info: new_type,
                span: Span::default(),
            };

            self.declare(new_decl).expect("Declaration Shouldn't fail");

            if let Some(scope) = self.scopes.last_mut() {
                if let Some(var_ctx) = scope.variables.get_mut(&name) {
                    var_ctx.original_type = Some((original_resolved, original_type));
                }
            }

            let (_, new_resolved) = self.lookup(name.as_ref()).unwrap();
            Some((resolved, new_resolved))
        } else {
            let mut new_ctx = VariableContext {
                type_info: new_type,
                ..ctx.clone()
            };
            let scope = self.scopes.last_mut().expect("No scope active");

            new_ctx
                .original_type
                .replace((original_resolved, original_type));
            scope.variables.insert(new_ctx.name.clone(), new_ctx);
            None
        }
    }

    fn add_closure_capture(closures: &mut Vec<Symbol>, name: Symbol) -> ResolvedVar {
        // Find if the closure is already declared.
        for (i, closure) in closures.iter().enumerate() {
            if *closure == name {
                return ResolvedVar::Closure(i as u8);
            }
        }
        closures.push(name);
        ResolvedVar::Closure((closures.len() - 1) as u8)
    }

    pub fn clear_closures(&mut self) -> Vec<Symbol> {
        std::mem::take(&mut self.closures)
    }
    pub fn return_closures(&mut self, returned: Vec<Symbol>) -> Vec<Symbol> {
        std::mem::replace(&mut self.closures, returned)
    }
}
