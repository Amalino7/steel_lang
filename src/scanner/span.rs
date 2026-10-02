use std::ops::Range;

/// Identifies a source file. `FileId(0)` is the prelude.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub file_id: FileId,
}

impl Span {
    pub const fn new(start: usize, end: usize, line: u32, file_id: FileId) -> Self {
        Self {
            start,
            end,
            line,
            file_id,
        }
    }

    /// Extends this span to cover another span (useful for combining start/end tokens of an expression)
    pub fn merge(&self, other: Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
            file_id: self.file_id,
        }
    }
    pub fn to_range(self) -> Range<usize> {
        self.start..self.end
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}
