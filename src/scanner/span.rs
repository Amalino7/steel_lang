use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
}

impl Span {
    pub fn new(start: usize, end: usize, line: u32) -> Self {
        Self { start, end, line }
    }

    /// Extends this span to cover another span (useful for combining start/end tokens of an expression)
    pub fn merge(&self, other: Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}
