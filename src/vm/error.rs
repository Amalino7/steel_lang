use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub stack_trace: Vec<String>,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Runtime Error: {}", self.message)?;
        for frame in &self.stack_trace {
            writeln!(f, "  at {}", frame)?;
        }
        Ok(())
    }
}
