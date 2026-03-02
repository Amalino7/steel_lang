use crate::compiler::analysis::ResolvedVar;
use crate::scanner::Span;
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::variables::{
    Declaration, DeclarationKind, Mutability, VariableContext,
};
use crate::typechecker::Symbol;
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

pub struct ScopeManager {
    scopes: Vec<Scope>,
    functions: Vec<FunctionContext>,
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
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
        if let Some(parent) = self.scopes.last_mut()
            && parent.kind != ScopeKind::Global
        {
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

    pub fn global_size(&self) -> u32 {
        self.scopes[0].last_index as u32
    }

    pub fn is_global(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn max_index(&self) -> usize {
        self.scopes.last().map(|s| s.max_index).unwrap_or(0)
    }

    pub fn declare(&mut self, decl: Declaration) -> Result<ResolvedVar, TypeCheckerError> {
        let scope = self.scopes.last_mut().expect("No scope active");

        if let Some(prev) = scope.variables.get(&decl.name)
            && prev.mutability == Mutability::Unique
        {
            return Err(TypeCheckerError::Redeclaration {
                name: decl.name.to_string(),
                span: decl.span,
                original: prev.span,
                original_kind: prev.kind,
            });
        }

        scope.variables.insert(
            decl.name.clone(),
            VariableContext::from_declaration(scope.last_index, decl),
        );

        let resolved = match scope.kind {
            ScopeKind::Global => ResolvedVar::Global(scope.last_index as u16),
            _ => ResolvedVar::Local(scope.last_index as u8),
        };

        scope.last_index += 1;
        scope.max_index = scope.max_index.max(scope.last_index);

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
    pub fn get_closures(&self) -> Vec<Symbol> {
        self.functions
            .last()
            .map(FunctionContext::captures)
            .map(|v| v.to_vec())
            .unwrap_or(Vec::new())
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

    /// Get all method names for a given type (for suggestions)
    /// Methods are stored as "TypeName.method_name" in the scope
    pub fn get_methods_for_type(&self, type_name: &str) -> Vec<String> {
        let prefix = format!("{}.", type_name);
        let mut methods = Vec::new();

        for scope in self.scopes.iter().rev() {
            for var_name in scope.variables.keys() {
                if let Some(method_name) = var_name.as_ref().strip_prefix(&prefix) {
                    methods.push(method_name.to_string());
                }
            }
        }

        methods
    }
}
