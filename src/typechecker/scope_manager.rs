use crate::compiler::analysis::ResolvedVar;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::Type;
use crate::typechecker::Symbol;
use std::collections::HashMap;

#[derive(Clone)]
pub struct VariableContext {
    pub(crate) type_info: Type,
    pub(crate) name: Symbol,
    pub(crate) index: usize,
}

impl VariableContext {
    fn new(name: Symbol, type_info: Type, index: usize) -> Self {
        VariableContext {
            type_info,
            name,
            index,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScopeType {
    Global,
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
}

// Might use it instead of manual clean-up
pub struct ScopeGuard<'a>(pub &'a mut ScopeManager);
impl<'a> Drop for ScopeGuard<'a> {
    fn drop(&mut self) {
        self.0.end_scope();
    }
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            closures: vec![],
        }
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

        if scope_type == ScopeType::Function {
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
        let max = finished_scope.max_index;
        if let Some(parent) = self.scopes.last_mut() {
            if parent.scope_type != ScopeType::Global {
                parent.max_index = parent.max_index.max(max);
            }
        }
        max
    }

    pub fn declare(&mut self, name: Symbol, type_info: Type) -> Result<(), TypeCheckerError> {
        let scope = self.scopes.last_mut().expect("No scope active");

        if scope.variables.contains_key(&name) && ScopeType::Global == scope.scope_type {
            return Err(TypeCheckerError::Redeclaration {
                name: name.to_string(),
                line: 0, // TODO get line
            });
        }

        scope.variables.insert(
            name.clone(),
            VariableContext::new(name, type_info, scope.last_index),
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

            if scope.scope_type == ScopeType::Function {
                is_closure = true;
            }
        }
        None
    }

    pub fn refine(&mut self, name: &str, new_type: Type) {
        let lookup_res = self.lookup(name);
        if let Some((ctx, resolved)) = lookup_res {
            // Globals cannot be safely refined

            if let ResolvedVar::Global(_) = resolved {
                return;
            }
            let idx = ctx.index;
            let name = ctx.name.clone();
            let scope = self.scopes.last_mut().expect("No scope active");
            scope
                .variables
                .insert(name.clone(), VariableContext::new(name, new_type, idx));
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
