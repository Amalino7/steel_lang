//! Parser and lexer errors, and their conversion into diagnostics.
//!
//! A [`ParseError`] is a [`ParseErrorKind`] and the span it is reported at. Expected-token errors compute that span
//! once, in `Parser::err_expected`, so the position rule lives in one place. Kinds hold spans and token kinds, never
//! tokens, so errors don't borrow the source.
//!
//! Codes are `E1000`–`E1099`, one per kind, grouped by tens:
//!
//! | Range | Group                     |
//! |-------|---------------------------|
//! | E100x | lexical and literals      |
//! | E101x | expected tokens and lists |
//! | E102x | declarations              |
//! | E103x | expressions               |
//! | E104x | imports                   |

use crate::diagnostics::builder::DiagBuilder;
use crate::diagnostics::{Diagnostic, DiagnosticContext, IntoDiagnostic};
use crate::scanner::{Span, TokenType, Unterminated};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    /// A character no token starts with.
    InvalidCharacter,
    /// A string or comment that runs to the end of the file; the span starts at its opener.
    Unterminated(Unterminated),
    /// Reported on the escape sequence itself.
    InvalidEscape(EscapeError),
    InvalidNumber(NumberError),
    /// `t.01`; the access is still built.
    TupleIndexLeadingZero,

    /// Something required is missing. The span is `found`'s, or the point after the previous token when `found`
    /// starts a new line.
    Expected {
        expected: Expected,
        /// Noun phrase for "expected X after <after>".
        after: Option<&'static str>,
        found: Found,
    },
    /// A keyword where a name is required (`let match = 1;`).
    KeywordAsName {
        keyword: TokenType,
    },
    /// A closing delimiter is missing. The span is where it belongs.
    UnclosedDelimiter {
        /// The opener it would close.
        open: Span,
        close: TokenType,
        found: Found,
        /// A list stopped right after an item, on a token that doesn't end a statement, so a `,` would continue
        /// it as well as the closer would end it (`f(a = b)`).
        after_item: bool,
    },
    /// A `,` is missing between two items. The span is the point after the first; `found` is the second.
    MissingSeparator {
        item: &'static str,
        found: Found,
    },

    /// `public` on a construct that doesn't take it; the span is the `public` token.
    VisibilityNotAllowed {
        reason: &'static str,
    },
    /// A `self` parameter outside a method.
    SelfOutsideMethod,
    /// A `self` parameter after the first position; `first` is the first parameter.
    SelfNotFirst {
        first: Span,
        duplicate: bool,
    },
    /// `()` where something must be inside.
    EmptyParens(ParenContext),
    /// `(x)` where a one-element tuple is meant; only `(x,)` is one.
    OneElementTuple(ParenContext),
    /// A shorthand field pattern (`: pattern`) whose pattern isn't a plain name, so there is no field name.
    FieldPatternWithoutName,

    InvalidAssignmentTarget,
    /// A list literal item in a map literal or the reverse. `first` is the first item, which decided the kind.
    MixedCollection {
        first: Span,
        is_map: bool,
    },

    /// An `import` after other declarations.
    ImportNotAtTop,
    EmptyImportGroup,
    NonAsciiImportPath,
}

/// What an [`ParseErrorKind::Expected`] error wanted.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expected {
    Token(TokenType),
    Name,
    Expression,
    Type,
    /// One item of a list, by its singular noun ("argument").
    Item(&'static str),
}

/// The token found where something else was expected.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Found {
    pub kind: TokenType,
    pub span: Span,
}

/// Where a parenthesised list was found, for the messages about its shape.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParenContext {
    Expression,
    TupleType,
    TuplePattern,
    VariantPayload,
    PatternPayload,
}

/// Why a number literal has no value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberError {
    /// A prefix or exponent without digits (`0x`, `1e`).
    Malformed,
    /// A prefixed literal beyond `i64` (`0xFFFFFFFFFFFFFFFFFF`).
    OutOfRange,
}

/// A bad escape sequence in a string literal.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EscapeError {
    Unknown(char),
    TrailingBackslash,
    UnicodeMissingBrace,
    UnicodeInvalidDigit,
    UnicodeUnterminated,
    UnicodeEmpty,
    UnicodeTooLong,
    /// Not a Unicode scalar value (a surrogate or above `10FFFF`).
    UnicodeNotScalar,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        ParseError { kind, span }
    }

    pub fn code(&self) -> &'static str {
        use ParseErrorKind as K;
        match self.kind {
            K::InvalidCharacter => "E1000",
            K::Unterminated(_) => "E1001",
            K::InvalidEscape(_) => "E1002",
            K::InvalidNumber(_) => "E1003",
            K::TupleIndexLeadingZero => "E1004",

            K::Expected { .. } => "E1010",
            K::KeywordAsName { .. } => "E1011",
            K::UnclosedDelimiter { .. } => "E1012",
            K::MissingSeparator { .. } => "E1013",

            K::VisibilityNotAllowed { .. } => "E1020",
            K::SelfOutsideMethod => "E1021",
            K::SelfNotFirst { .. } => "E1022",
            K::EmptyParens(_) => "E1023",
            K::OneElementTuple(_) => "E1024",
            K::FieldPatternWithoutName => "E1025",

            K::InvalidAssignmentTarget => "E1030",
            K::MixedCollection { .. } => "E1031",

            K::ImportNotAtTop => "E1040",
            K::EmptyImportGroup => "E1041",
            K::NonAsciiImportPath => "E1042",
        }
    }

    /// The headline: "expected X after Y, found Z".
    pub fn message(&self) -> String {
        use ParseErrorKind as K;
        match &self.kind {
            K::InvalidCharacter => "invalid character".to_string(),
            K::Unterminated(what) => format!("unterminated {}", unterminated_noun(*what)),
            K::InvalidEscape(e) => escape_message(*e),
            K::InvalidNumber(NumberError::Malformed) => "malformed number literal".to_string(),
            K::InvalidNumber(NumberError::OutOfRange) => {
                "number literal is out of range".to_string()
            }
            K::TupleIndexLeadingZero => "tuple index has a leading zero".to_string(),

            K::Expected {
                expected,
                after,
                found,
            } => {
                let after = after.map_or(String::new(), |a| format!(" after {a}"));
                format!(
                    "expected {}{after}, found {}",
                    describe_expected(*expected),
                    describe(found.kind)
                )
            }
            K::KeywordAsName { keyword } => {
                format!("`{keyword}` is a keyword and can't be used as a name")
            }
            K::UnclosedDelimiter {
                close,
                found,
                after_item: true,
                ..
            } => format!("expected `,` or `{close}`, found {}", describe(found.kind)),
            K::UnclosedDelimiter { close, found, .. } => format!(
                "expected `{close}` to close `{}`, found {}",
                opener(*close),
                describe(found.kind)
            ),
            K::MissingSeparator { item, .. } => format!("missing `,` after {item}"),

            K::VisibilityNotAllowed { reason } => format!("`public` is not allowed here: {reason}"),
            K::SelfOutsideMethod => "`self` is only allowed in methods".to_string(),
            K::SelfNotFirst {
                duplicate: true, ..
            } => "duplicate `self` parameter".to_string(),
            K::SelfNotFirst { .. } => "`self` must be the first parameter".to_string(),
            K::EmptyParens(ctx) => match ctx {
                ParenContext::Expression => "`()` is not an expression".to_string(),
                ParenContext::TupleType => "empty tuple type".to_string(),
                ParenContext::TuplePattern => "empty tuple pattern".to_string(),
                ParenContext::VariantPayload => "empty variant payload".to_string(),
                ParenContext::PatternPayload => "empty pattern payload".to_string(),
            },
            K::OneElementTuple(ctx) => {
                let what = match ctx {
                    ParenContext::TuplePattern => "tuple pattern",
                    _ => "tuple type",
                };
                format!("one-element {what} without a trailing comma")
            }
            K::FieldPatternWithoutName => "a shorthand field pattern must be a name".to_string(),

            K::InvalidAssignmentTarget => "invalid assignment target".to_string(),
            K::MixedCollection { is_map: true, .. } => {
                "expected `key: value` in a map literal".to_string()
            }
            K::MixedCollection { is_map: false, .. } => {
                "unexpected `key: value` in a list literal".to_string()
            }

            K::ImportNotAtTop => "imports must come before all other declarations".to_string(),
            K::EmptyImportGroup => "empty import group".to_string(),
            K::NonAsciiImportPath => "import paths must be ASCII".to_string(),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message())
    }
}

/// Everything a diagnostic shows besides the headline, one arm per kind: the primary label, then secondary labels,
/// notes and help.
impl IntoDiagnostic for ParseError {
    fn into_diagnostic(self, _: &DiagnosticContext) -> Diagnostic {
        use ParseErrorKind as K;
        let span = self.span;
        let (code, message) = (self.code(), self.message());
        let error = |label: &str| DiagBuilder::error(span, code, message.clone(), label);
        let builder = match self.kind {
            K::InvalidCharacter => error("unexpected character"),
            K::Unterminated(what) => error("starts here and never ends").with_help(format!(
                "add the closing `{}`",
                match what {
                    Unterminated::String => r#"""#,
                    Unterminated::RawString => r#"""""#,
                    Unterminated::BlockComment => "*/",
                }
            )),
            K::InvalidEscape(_) => error("invalid escape").with_note(
                r#"valid escapes are `\n`, `\t`, `\r`, `\\`, `\"`, `\0`, `\$` and `\u{…}`"#,
            ),
            K::InvalidNumber(NumberError::Malformed) => error("missing digits"),
            K::InvalidNumber(NumberError::OutOfRange) => error("too large")
                .with_note("binary, octal and hex literals must fit in a 64-bit integer"),
            K::TupleIndexLeadingZero => error("leading zero").with_help("remove the leading zeros"),

            K::Expected {
                expected, found, ..
            } => with_found(
                error(&format!("expected {}", describe_expected(expected))),
                found,
                span,
            ),
            K::KeywordAsName { keyword } => {
                error("keyword").with_help(format!("rename it, e.g. `{keyword}_`"))
            }
            K::UnclosedDelimiter {
                open,
                close,
                found,
                after_item,
            } => {
                let label = if after_item {
                    format!("expected `,` or `{close}`")
                } else {
                    format!("expected `{close}`")
                };
                let builder =
                    error(&label).with_secondary(open, format!("unclosed `{}`", opener(close)));
                with_found(builder, found, span)
            }
            K::MissingSeparator { found, .. } => with_found(error("expected `,`"), found, span),

            K::VisibilityNotAllowed { .. } => error("not allowed here"),
            K::SelfOutsideMethod => error("`self` parameter")
                .with_note("`self` parameters belong to methods inside an `impl` or `interface`"),
            K::SelfNotFirst { first, duplicate } => error("`self` parameter").with_secondary(
                first,
                if duplicate {
                    "`self` is already declared here"
                } else {
                    "`self` must come before this parameter"
                },
            ),
            K::EmptyParens(ctx) => error("empty").with_help(match ctx {
                ParenContext::VariantPayload | ParenContext::PatternPayload => "remove the `()`",
                _ => "a tuple needs at least one element: `(x,)`",
            }),
            K::OneElementTuple(_) => error("needs a trailing comma")
                .with_help("add a trailing comma: `(x,)`; without one, `(x)` is just `x`"),
            K::FieldPatternWithoutName => {
                error("not a name").with_help("name the field it destructures: `field: pattern`")
            }

            K::InvalidAssignmentTarget => error("can't be assigned to"),
            K::MixedCollection { first, is_map } => {
                let (label, origin) = if is_map {
                    ("expected `key: value`", "this makes it a map literal")
                } else {
                    ("expected a plain element", "this makes it a list literal")
                };
                error(label).with_secondary(first, origin)
            }

            K::ImportNotAtTop => {
                error("import after other declarations").with_help("move it to the top of the file")
            }
            K::EmptyImportGroup => error("empty group"),
            K::NonAsciiImportPath => error("non-ASCII name"),
        };
        builder.build()
    }
}

/// Labels the found token when the error is reported elsewhere (the point after the previous token).
fn with_found(builder: DiagBuilder, found: Found, span: Span) -> DiagBuilder {
    if found.span != span {
        builder.with_secondary(found.span, format!("found {}", describe(found.kind)))
    } else {
        builder
    }
}

/// A token kind in a message: punctuation and keywords quoted, token classes as words.
fn describe(kind: TokenType) -> String {
    match kind {
        TokenType::Identifier
        | TokenType::Number
        | TokenType::String
        | TokenType::RawString
        | TokenType::PartialString
        | TokenType::EOF
        | TokenType::Unterminated(_)
        | TokenType::UnexpectedSymbolError => kind.to_string(),
        _ => format!("`{kind}`"),
    }
}

fn describe_expected(expected: Expected) -> String {
    match expected {
        Expected::Token(TokenType::Identifier) | Expected::Name => "a name".to_string(),
        Expected::Token(kind) => describe(kind),
        Expected::Expression => "an expression".to_string(),
        Expected::Type => "a type".to_string(),
        Expected::Item(noun) if noun.starts_with(['a', 'e', 'i', 'o', 'u']) => format!("an {noun}"),
        Expected::Item(noun) => format!("a {noun}"),
    }
}

/// The opener of a closer that [`crate::parser`] tracks as a delimiter.
fn opener(close: TokenType) -> TokenType {
    match close {
        TokenType::RightParen => TokenType::LeftParen,
        TokenType::RightBracket => TokenType::LeftBracket,
        TokenType::RightBrace => TokenType::LeftBrace,
        TokenType::Greater => TokenType::Less,
        other => other,
    }
}

fn unterminated_noun(what: Unterminated) -> &'static str {
    match what {
        Unterminated::String => "string",
        Unterminated::RawString => "raw string",
        Unterminated::BlockComment => "block comment",
    }
}

fn escape_message(error: EscapeError) -> String {
    match error {
        EscapeError::Unknown(c) => format!(r"unknown escape sequence `\{c}`"),
        EscapeError::TrailingBackslash => r"trailing `\` at the end of the string".to_string(),
        EscapeError::UnicodeMissingBrace => r"expected `{` after `\u`".to_string(),
        EscapeError::UnicodeInvalidDigit => {
            "invalid character in unicode escape; expected a hex digit or `}`".to_string()
        }
        EscapeError::UnicodeUnterminated => "unterminated unicode escape".to_string(),
        EscapeError::UnicodeEmpty => "unicode escape must have at least one hex digit".to_string(),
        EscapeError::UnicodeTooLong => "unicode escape must have at most 6 hex digits".to_string(),
        EscapeError::UnicodeNotScalar => "unicode escape is not a valid character".to_string(),
    }
}
