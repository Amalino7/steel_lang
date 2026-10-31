use std::ops::Range;
/// `Span::default()` is a placeholder for spans that are synthetic or not yet available. Use `Span::builtin()`
/// for items that really are builtin.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub file_id: FileId,
}

/// Identifies a source file.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct FileId(pub u32);

/// The file id of the standard library module. Builtin items carry a span in this file.
pub const PRELUDE_FILE: FileId = FileId(0);

impl Span {
    pub const fn new(start: usize, end: usize, line: u32, file_id: FileId) -> Self {
        Self {
            start,
            end,
            line,
            file_id,
        }
    }

    /// The span of items that exist without source text: builtin types and vararg natives. They belong to `std`.
    pub const fn builtin() -> Self {
        Self::new(0, 0, 0, PRELUDE_FILE)
    }

    pub fn is_builtin(&self) -> bool {
        *self == Self::builtin()
    }

    /// The smallest span covering both, in either order; `line` is the line of the one that starts first.
    /// Both spans must be in the same file.
    pub fn merge(self, other: Span) -> Self {
        debug_assert_eq!(
            self.file_id, other.file_id,
            "merging spans from different files"
        );
        let first = if other.start < self.start {
            other
        } else {
            self
        };
        Self {
            start: first.start,
            end: self.end.max(other.end),
            ..first
        }
    }

    /// Zero-width span at the end of `self`: where a missing `;` or closer would go.
    pub fn point_after(self) -> Self {
        Self {
            start: self.end,
            ..self
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merge_takes_the_line_of_the_earlier_span_in_either_order() {
        let first = Span::new(0, 1, 1, FileId(1));
        let second = Span::new(10, 12, 3, FileId(1));
        assert_eq!(first.merge(second), Span::new(0, 12, 1, FileId(1)));
        assert_eq!(second.merge(first), Span::new(0, 12, 1, FileId(1)));
    }
}
