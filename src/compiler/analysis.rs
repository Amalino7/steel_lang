use std::cmp::max;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedVar {
    Local(usize),
    Global(usize),
    Closure(usize),
}

#[derive(Debug)]
pub struct AnalysisInfo {
    pub global_count: usize,
    pub resolved_vars: HashMap<usize, ResolvedVar>,
    pub captures: HashMap<usize, Vec<ResolvedVar>>,
    pub drop_at_scope_end: HashMap<usize, usize>,
}

impl AnalysisInfo {
    pub fn new() -> Self {
        AnalysisInfo {
            global_count: 0,
            resolved_vars: HashMap::new(),
            captures: HashMap::new(),
            drop_at_scope_end: HashMap::new(),
        }
    }

    pub fn add_var(&mut self, id: usize, var: ResolvedVar) {
        if let ResolvedVar::Global(idx) = var {
            self.global_count = max(idx + 1, self.global_count);
        }
        self.resolved_vars.insert(id, var);
    }

    pub fn add_capture(&mut self, id: usize, var: ResolvedVar) {
        self.captures.entry(id).or_default().push(var);
    }
}
