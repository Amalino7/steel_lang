use crate::parser::error::{EscapeError, NumberError};
use std::iter::Peekable;
use std::num::IntErrorKind;
use std::ops::Range;
use std::str::CharIndices;

/// Processes the escape sequences in `raw`, the text between a string's delimiters. On failure, returns the error
/// and the byte range in `raw` of the escape sequence, up to where it went wrong.
pub fn process_escapes(raw: &str) -> Result<String, (EscapeError, Range<usize>)> {
    let mut result = String::with_capacity(raw.len());
    let mut chars = raw.char_indices().peekable();
    while let Some((start, c)) = chars.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }
        // The escape runs from its `\` to the last character consumed.
        let fail = |chars: &mut Peekable<CharIndices>, error| {
            let end = chars.peek().map_or(raw.len(), |&(i, _)| i);
            Err((error, start..end))
        };
        match chars.next().map(|(_, c)| c) {
            Some('n') => result.push('\n'),
            Some('t') => result.push('\t'),
            Some('r') => result.push('\r'),
            Some('\\') => result.push('\\'),
            Some('"') => result.push('"'),
            Some('0') => result.push('\0'),
            Some('$') => result.push('$'),
            Some('u') => {
                // Expect `{HHHH}` (1-6 hex digits).
                if chars.next_if(|&(_, c)| c == '{').is_none() {
                    return fail(&mut chars, EscapeError::UnicodeMissingBrace);
                }
                let mut hex = String::with_capacity(6);
                loop {
                    match chars.next().map(|(_, c)| c) {
                        Some('}') => break,
                        Some(h) if h.is_ascii_hexdigit() => hex.push(h),
                        Some(_) => return fail(&mut chars, EscapeError::UnicodeInvalidDigit),
                        None => return fail(&mut chars, EscapeError::UnicodeUnterminated),
                    }
                    if hex.len() > 6 {
                        return fail(&mut chars, EscapeError::UnicodeTooLong);
                    }
                }
                if hex.is_empty() {
                    return fail(&mut chars, EscapeError::UnicodeEmpty);
                }
                // At most 6 hex digits always fit in a `u32`.
                let codepoint = u32::from_str_radix(&hex, 16).unwrap_or(u32::MAX);
                match char::from_u32(codepoint) {
                    Some(ch) => result.push(ch),
                    None => return fail(&mut chars, EscapeError::UnicodeNotScalar),
                }
            }
            Some(other) => return fail(&mut chars, EscapeError::Unknown(other)),
            None => return fail(&mut chars, EscapeError::TrailingBackslash),
        }
    }
    Ok(result)
}

/// Parses a number literal
///
/// Handles:
/// - Underscore separators: `1_000_000`, `3.14_15`
/// - Binary prefix: `0b1010`, `0B1010`
/// - Octal prefix: `0o755`, `0O755`
/// - Hexadecimal prefix: `0xFF`, `0XFF`
/// - Scientific notation: `1.5e10`, `2.0E-3`
///
/// The scanner only guarantees the characters, so a prefix or exponent without digits (`0x`, `1e`) is
/// [`NumberError::Malformed`]. Prefixed literals must fit in an `i64`.
pub fn parse_number(lexeme: &str) -> Result<f64, NumberError> {
    let strip_underscores = |s: &str| -> String { s.chars().filter(|&c| c != '_').collect() };
    let radix = |rest: &str, radix| {
        i64::from_str_radix(&strip_underscores(rest), radix)
            .map(|n| n as f64)
            .map_err(|e| match e.kind() {
                IntErrorKind::PosOverflow => NumberError::OutOfRange,
                _ => NumberError::Malformed,
            })
    };

    if let Some(rest) = lexeme
        .strip_prefix("0b")
        .or_else(|| lexeme.strip_prefix("0B"))
    {
        radix(rest, 2)
    } else if let Some(rest) = lexeme
        .strip_prefix("0o")
        .or_else(|| lexeme.strip_prefix("0O"))
    {
        radix(rest, 8)
    } else if let Some(rest) = lexeme
        .strip_prefix("0x")
        .or_else(|| lexeme.strip_prefix("0X"))
    {
        radix(rest, 16)
    } else {
        strip_underscores(lexeme)
            .parse::<f64>()
            .map_err(|_| NumberError::Malformed)
    }
}

/// Strips `"""` delimiters and applies Swift-style indentation normalisation.
///
/// If the closing `"""` is on its own line, the whitespace that precedes it
/// on that line is treated as the "indent prefix" and stripped from the start
/// of every content line.  A leading newline immediately after the opening
/// `"""` is also removed.
pub fn process_raw_string(lexeme: &str) -> String {
    // Strip opening and closing `"""`.
    let inner = &lexeme[3..lexeme.len() - 3];

    // Swift-style alignment: look for a final newline whose suffix is all whitespace.
    if let Some(last_nl) = inner.rfind('\n') {
        let indent = &inner[last_nl + 1..];
        if indent.chars().all(|c| c.is_whitespace()) {
            // Strip optional leading newline (the one right after the opening `"""`).
            let body_start = if inner.starts_with('\n') { 1 } else { 0 };
            let body = &inner[body_start..last_nl];

            if indent.is_empty() {
                return body.to_string();
            }
            // Strip the indent prefix from every line.
            return body
                .split('\n')
                .map(|line| {
                    if let Some(stripped) = line.strip_prefix(indent) {
                        stripped
                    } else {
                        // Partial indentation: strip as much leading whitespace
                        line.trim_start_matches([' ', '\t'])
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");
        }
    }

    inner.to_string()
}
