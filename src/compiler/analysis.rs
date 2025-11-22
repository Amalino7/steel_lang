use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedVar {
    Local(usize),
    Global(usize),
    // TODO Add upvalues
    Closure(usize),
}

#[derive(Debug)]
pub struct AnalysisInfo {
    pub global_count: usize,
    pub resolved_vars: HashMap<usize, ResolvedVar>,
}

impl AnalysisInfo {
    pub fn new() -> Self {
        AnalysisInfo {
            global_count: 0,
            resolved_vars: HashMap::new(),
        }
    }

    pub fn add_var(&mut self, id: usize, var: ResolvedVar) {
        if let ResolvedVar::Global(_) = var {
            self.global_count += 1;
        }
        self.resolved_vars.insert(id, var);
    }
}
