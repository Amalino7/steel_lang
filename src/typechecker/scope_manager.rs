use crate::compiler::analysis::ResolvedVar;
use crate::scanner::{Span, Token};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::types::Type;
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
pub struct VariableContext {
    pub(crate) type_info: Type,
    pub(crate) name: Symbol,
    pub(crate) index: usize,
    pub(crate) span: Span,
    pub(crate) original_type: Option<(ResolvedVar, Type)>,
}

impl VariableContext {
    fn new(name: Symbol, type_info: Type, index: usize, span: Span) -> Self {
        VariableContext {
            type_info,
            name,
            index,
            span,
            original_type: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScopeType {
    Global,
    /// Adds Self type
    ImplBlock,
    TypeBlock {
        generic_count: usize,
    },
    Function,
    Block,
}
struct Scope {
    variables: HashMap<Symbol, VariableContext>,
    scope_type: ScopeType,
    last_index: usize,
    max_index: usize,
}

pub struct ScopeManager {
    scopes: Vec<Scope>,
    closures: Vec<Symbol>,
    self_type: Option<Type>,
    generics: HashSet<Symbol>,
    generics_raw: Vec<Symbol>,
}

pub struct ScopeGuard<'a, 'src>(&'a mut TypeChecker<'src>);

impl<'a, 'src> ScopeGuard<'a, 'src> {
    pub fn new(checker: &'a mut TypeChecker<'src>, scope_type: ScopeType) -> Self {
        checker.scopes.begin_scope(scope_type);
        ScopeGuard(checker)
    }

    pub fn new_impl(checker: &'a mut TypeChecker<'src>, self_ty: Type) -> Self {
        checker.scopes.begin_impl_block(self_ty);
        ScopeGuard(checker)
    }
    pub fn new_type_block(checker: &'a mut TypeChecker<'src>, generics: &[Token<'src>]) -> Self {
        let resolved_generics = generics.iter().map(|s| s.lexeme.into()).collect::<Vec<_>>();
        checker.scopes.begin_type_block(&resolved_generics);
        ScopeGuard(checker)
    }
}
impl<'a> Drop for ScopeGuard<'a, '_> {
    fn drop(&mut self) {
        self.0.scopes.end_scope();
    }
}

impl<'a, 'src> std::ops::Deref for ScopeGuard<'a, 'src> {
    type Target = TypeChecker<'src>;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, 'src> std::ops::DerefMut for ScopeGuard<'a, 'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            closures: vec![],
            self_type: None,
            generics: HashSet::new(),
            generics_raw: Vec::new(),
        }
    }

    pub fn get_self_type(&self) -> Option<&Type> {
        self.self_type.as_ref()
    }
    pub fn get_generics(&self) -> &HashSet<Symbol> {
        &self.generics
    }

    pub fn begin_impl_block(&mut self, self_type: Type) {
        self.self_type = Some(self_type);
    }

    pub(crate) fn begin_type_block(&mut self, generics: &[Symbol]) {
        // TODO redeclaration error
        self.generics_raw.extend(generics.iter().cloned());
        self.generics.extend(generics.iter().cloned());
        self.begin_scope(ScopeType::TypeBlock {
            generic_count: generics.len(),
        });
    }

    pub fn begin_scope(&mut self, scope_type: ScopeType) {
        let mut last_idx = self
            .scopes
            .last()
            .map(|s| {
                if s.scope_type == ScopeType::Global {
                    0
                } else {
                    s.last_index
                }
            })
            .unwrap_or(0);

        if matches!(scope_type, ScopeType::Function { .. }) {
            last_idx = 0;
        }

        self.scopes.push(Scope {
            max_index: last_idx,
            variables: HashMap::new(),
            scope_type,
            last_index: last_idx,
        });
    }

    pub fn global_size(&self) -> u32 {
        self.scopes[0].last_index as u32
    }

    pub fn is_global(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn end_scope(&mut self) -> usize {
        let finished_scope = self.scopes.pop().expect("No scope to end");
        match finished_scope.scope_type {
            ScopeType::ImplBlock => {
                self.self_type = None;
            }
            ScopeType::TypeBlock { generic_count } => {
                for _ in self.generics_raw.len() - generic_count..self.generics_raw.len() {
                    self.generics.remove(&self.generics_raw.pop().unwrap());
                }
            }
            ScopeType::Function => {}
            ScopeType::Block => {}
            ScopeType::Global => {}
        }

        let max = finished_scope.max_index;
        if let Some(parent) = self.scopes.last_mut()
            && parent.scope_type != ScopeType::Global
        {
            parent.max_index = parent.max_index.max(max);
        }

        max
    }
    pub fn max_index(&self) -> usize {
        self.scopes.last().map(|s| s.max_index).unwrap_or(0)
    }

    pub fn declare(
        &mut self,
        name: Symbol,
        type_info: Type,
        span: Span,
    ) -> Result<(), TypeCheckerError> {
        let scope = self.scopes.last_mut().expect("No scope active");

        if scope.variables.contains_key(&name) && ScopeType::Global == scope.scope_type {
            let prev = scope.variables.get(&name).unwrap();
            return Err(TypeCheckerError::Redeclaration {
                name: name.to_string(),
                span,
                original: prev.span,
            });
        }

        scope.variables.insert(
            name.clone(),
            VariableContext::new(name, type_info, scope.last_index, span),
        );
        scope.last_index += 1;
        scope.max_index = scope.max_index.max(scope.last_index);
        Ok(())
    }

    pub fn lookup(&mut self, name: &str) -> Option<(&VariableContext, ResolvedVar)> {
        let mut is_closure = false;

        for scope in self.scopes.iter().rev() {
            if let Some(ctx) = scope.variables.get(name) {
                let resolved = if scope.scope_type == ScopeType::Global {
                    ResolvedVar::Global(ctx.index as u16)
                } else if is_closure {
                    Self::add_closure_capture(&mut self.closures, ctx.name.clone())
                } else {
                    ResolvedVar::Local(ctx.index as u8)
                };
                return Some((ctx, resolved));
            }

            if matches!(scope.scope_type, ScopeType::Function) {
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
        if let Some((ctx, resolved)) = lookup_res {
            // Globals cannot be safely refined

            if let ResolvedVar::Global(_) = resolved {
                return None;
            }
            let original_type = ctx.type_info.clone();
            let original_resolved = resolved.clone();

            return if let Type::Enum(_, _) = &ctx.type_info {
                let name = ctx.name.clone();
                self.declare(name.clone(), new_type, Span::default())
                    .unwrap(); // Shouldn't fail

                if let Some(scope) = self.scopes.last_mut() {
                    if let Some(var_ctx) = scope.variables.get_mut(&name) {
                        var_ctx.original_type = Some((original_resolved, original_type));
                    }
                }

                let (_, new_resolved) = self.lookup(name.as_ref()).unwrap();
                Some((resolved, new_resolved))
            } else {
                let idx = ctx.index;
                let name = ctx.name.clone();
                let span = ctx.span;
                let scope = self.scopes.last_mut().expect("No scope active");

                let mut new_ctx = VariableContext::new(name.clone(), new_type, idx, span);
                new_ctx
                    .original_type
                    .replace((original_resolved, original_type));
                scope.variables.insert(name, new_ctx);
                None
            };
        }
        None
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
