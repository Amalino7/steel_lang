use crate::compiler::analysis::ResolvedVar;
use crate::scanner::Span;
use crate::typechecker::Symbol;
use crate::typechecker::core::error::{BindingError, TypeCheckerError};
use crate::typechecker::core::types::Type;
use crate::typechecker::id_issuer::GlobalIdGenerator;
use crate::typechecker::scope::variables::{
    Declaration, DeclarationKind, Mutability, VariableContext,
};
use std::cmp::PartialEq;
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

struct FunctionContext {
    return_type: Type,
    origin: Span,
    captures: Vec<Symbol>,
}
impl FunctionContext {
    pub fn captures(&self) -> &[Symbol] {
        &self.captures
    }
    pub fn return_type(&self) -> (Type, Span) {
        (self.return_type.clone(), self.origin)
    }
}

pub struct ScopeManager<'ctx> {
    scopes: Vec<Scope>,
    functions: Vec<FunctionContext>,
    id_generator: &'ctx GlobalIdGenerator,
}

impl<'ctx> ScopeManager<'ctx> {
    pub fn new(id_generator: &'ctx GlobalIdGenerator) -> Self {
        Self {
            id_generator,
            scopes: vec![],
            functions: vec![],
        }
    }

    pub fn begin_function(&mut self, return_type: Type, span: Span) {
        let func_ctx = FunctionContext {
            return_type,
            origin: span,
            captures: vec![],
        };
        self.functions.push(func_ctx);
        self.begin_scope(ScopeKind::Function);
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
        if let Some(parent) = self.scopes.last_mut() {
            parent.max_index = parent.max_index.max(max);
        }

        if matches!(finished_scope.kind, ScopeKind::Function) {
            self.functions.pop();
        }

        max
    }

    pub fn return_type(&self) -> Option<(Type, Span)> {
        self.functions.last().map(FunctionContext::return_type)
    }

    pub fn is_global(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn max_index(&self) -> usize {
        self.scopes.last().map(|s| s.max_index).unwrap_or(0)
    }

    pub fn declare_existing(&mut self, ctx: &VariableContext) -> Result<(), TypeCheckerError> {
        let scope = &mut self.scopes[0];
        if let Some(prev) = scope.variables.get(&ctx.name)
            && (prev.mutability == Mutability::Unique || scope.kind == ScopeKind::Global)
            && ctx.index != prev.index
        {
            return Err(TypeCheckerError::Binding(BindingError::Redeclaration {
                name: ctx.name.to_string(),
                span: ctx.span,
                original: prev.span,
                original_kind: prev.kind,
            }));
        }

        let imported = VariableContext {
            is_public: false,
            ..ctx.clone()
        };
        scope.variables.insert(ctx.name.clone(), imported);
        Ok(())
    }

    pub fn declare(&mut self, decl: Declaration) -> Result<ResolvedVar, TypeCheckerError> {
        let scope = self.scopes.last_mut().expect("No scope active");

        if let Some(prev) = scope.variables.get(&decl.name)
            && (prev.mutability == Mutability::Unique || scope.kind == ScopeKind::Global)
        {
            return Err(TypeCheckerError::Binding(BindingError::Redeclaration {
                name: decl.name.to_string(),
                span: decl.span,
                original: prev.span,
                original_kind: prev.kind,
            }));
        }

        let (index, resolved) = match scope.kind {
            ScopeKind::Global => {
                let id = self.id_generator.next().get();
                (id, ResolvedVar::Global(id as u16))
            }
            _ => {
                let idx = scope.last_index;
                scope.last_index += 1;
                scope.max_index = scope.max_index.max(scope.last_index);
                (idx, ResolvedVar::Local(idx as u8))
            }
        };

        scope.variables.insert(
            decl.name.clone(),
            VariableContext::from_declaration(index, decl),
        );

        Ok(resolved)
    }

    pub fn lookup(&mut self, name: &str) -> Option<(&VariableContext, ResolvedVar)> {
        self.lookup_impl(name, false)
    }

    /// Like [`lookup`] but marks the binding as written rather than read.
    /// Use this when resolving the left-hand side of an assignment so that
    /// write-only bindings are not incorrectly counted as "used".
    pub fn lookup_for_write(&mut self, name: &str) -> Option<(&VariableContext, ResolvedVar)> {
        self.lookup_impl(name, true)
    }

    fn lookup_impl(
        &mut self,
        name: &str,
        is_write: bool,
    ) -> Option<(&VariableContext, ResolvedVar)> {
        let mut is_closure = false;

        for scope in self.scopes.iter_mut().rev() {
            if let Some(ctx) = scope.variables.get_mut(name) {
                if is_write {
                    ctx.was_written = true;
                } else {
                    ctx.was_read = true;
                }
                let resolved = if scope.kind == ScopeKind::Global {
                    ResolvedVar::Global(ctx.index as u16)
                } else if is_closure {
                    let captures = &mut self.functions.last_mut().unwrap().captures;
                    Self::add_closure_capture(captures, ctx.name.clone())
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
                is_public: false,
            };

            self.declare(new_decl).expect("Declaration Shouldn't fail");

            if let Some(scope) = self.scopes.last_mut()
                && let Some(var_ctx) = scope.variables.get_mut(&name)
            {
                var_ctx.original_type = Some((original_resolved, original_type));
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
    pub fn get_closures(&self) -> Vec<Symbol> {
        self.functions
            .last()
            .map(FunctionContext::captures)
            .map(|v| v.to_vec())
            .unwrap_or_default()
    }

    pub(crate) fn export_vars(&mut self) -> HashMap<Symbol, VariableContext> {
        let scope = self.scopes.first_mut().expect("No Scope exists");
        std::mem::take(&mut scope.variables)
    }

    /// Get all visible variable names in the current scope (for suggestions)
    pub fn visible_variable_names(&self) -> Vec<&str> {
        let mut names = Vec::new();
        for scope in self.scopes.iter().rev() {
            for var_name in scope.variables.keys() {
                names.push(var_name.as_ref());
            }
        }
        names
    }

    /// Returns bindings in the current scope that were never read.
    /// Only covers `Variable` and `Parameter` kinds; skips names starting with `_`
    /// and compiler-generated entries (those with a default span from `refine()`).
    pub fn drain_unused(&self) -> Vec<(String, Span)> {
        let Some(scope) = self.scopes.last() else {
            return vec![];
        };
        scope
            .variables
            .values()
            .filter(|ctx| {
                !ctx.was_read
                    && matches!(
                        ctx.kind,
                        DeclarationKind::Variable
                            | DeclarationKind::Parameter
                            | DeclarationKind::Binding
                    )
                    && !ctx.name.starts_with('_')
                    && ctx.name.as_ref() != "self"
                    && ctx.span != Span::default()
            })
            .map(|ctx| (ctx.name.to_string(), ctx.span))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_manager_uses_global_id_generator() {
        let generator = GlobalIdGenerator::new();
        let mut scope_manager = ScopeManager::new(&generator);

        scope_manager.begin_scope(ScopeKind::Global);

        let decl1 = Declaration::mutable("x".into(), Type::Number, Span::default());
        let res1 = scope_manager.declare(decl1).unwrap();
        assert_eq!(res1, ResolvedVar::Global(0));

        let decl2 = Declaration::mutable("y".into(), Type::Number, Span::default());
        let res2 = scope_manager.declare(decl2).unwrap();
        assert_eq!(res2, ResolvedVar::Global(1));

        assert_eq!(scope_manager.id_generator.count(), 2);
        assert_eq!(generator.count(), 2);

        // Generating an ID externally advances the sequence
        let id2 = generator.next();
        assert_eq!(id2.get(), 2);

        let decl3 = Declaration::mutable("z".into(), Type::Number, Span::default());
        let res3 = scope_manager.declare(decl3).unwrap();
        assert_eq!(res3, ResolvedVar::Global(3));

        assert_eq!(scope_manager.id_generator.count(), 4);
        assert_eq!(generator.count(), 4);
    }
}
