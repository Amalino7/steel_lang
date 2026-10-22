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

    /// Extends this span to cover another span (useful for combining start/end tokens of an expression).
    /// Both spans must be in the same file.
    pub fn merge(&self, other: Span) -> Self {
        debug_assert_eq!(
            self.file_id, other.file_id,
            "merging spans from different files"
        );
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
            file_id: self.file_id,
        }
    }
    /// The span from the start of `self` to the end of `end`. Unlike [`Self::merge`] the order matters:
    /// `end` must not start before `self`, which catches swapped arguments.
    pub fn to(self, end: Span) -> Self {
        debug_assert!(self.start <= end.start, "Span::to with swapped arguments");
        self.merge(end)
    }

    /// Zero-width span at the end of `self`: where a missing `;` or closer would go.
    pub fn point_after(self) -> Self {
        Self {
            start: self.end,
            ..self
        }
    }

    /// Zero-width span at the start of `self`.
    pub fn point_before(self) -> Self {
        Self {
            end: self.start,
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
