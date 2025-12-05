use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedVar {
    Local(u8),
    Global(u16),
    Closure(u8),
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

    pub fn add_capture(&mut self, id: usize, var: ResolvedVar) {
        self.captures.entry(id).or_default().push(var);
    }
}
