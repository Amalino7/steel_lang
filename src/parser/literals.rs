use crate::parser::error::{EscapeError, NumberError};
use std::iter::Peekable;
use std::num::IntErrorKind;
use std::ops::Range;
use std::str::CharIndices;

/// Processes the escape sequences in `raw`, the text between a string's delimiters. On failure, returns the error
/// and the byte range in `raw` of the escape sequence, up to where it went wrong.
pub(super) fn process_escapes(raw: &str) -> Result<String, (EscapeError, Range<usize>)> {
    let mut result = String::with_capacity(raw.len());
    let mut chars = raw.char_indices().peekable();

    while let Some((start, c)) = chars.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }

        let unescaped = parse_escape(&mut chars).map_err(|error| {
            let end = chars.peek().map_or(raw.len(), |&(i, _)| i);
            (error, start..end)
        })?;

        result.push(unescaped);
    }
    Ok(result)
}

type CharIter<'a> = Peekable<CharIndices<'a>>;

fn parse_escape(chars: &mut CharIter<'_>) -> Result<char, EscapeError> {
    let (_, c) = chars.next().ok_or(EscapeError::TrailingBackslash)?;
    match c {
        'n' => Ok('\n'),
        't' => Ok('\t'),
        'r' => Ok('\r'),
        '0' => Ok('\0'),
        '\\' | '"' | '$' => Ok(c),
        'u' => parse_unicode_escape(chars),
        other => Err(EscapeError::Unknown(other)),
    }
}

/// Parses `{H...}` (1-6 hex digits) after `\u`.
fn parse_unicode_escape(chars: &mut CharIter<'_>) -> Result<char, EscapeError> {
    if chars.next_if(|&(_, c)| c == '{').is_none() {
        return Err(EscapeError::UnicodeMissingBrace);
    }

    let mut codepoint = 0u32;
    let mut digits = 0;
    loop {
        let (_, c) = chars.next().ok_or(EscapeError::UnicodeUnterminated)?;
        if c == '}' {
            break;
        }

        let digit = c.to_digit(16).ok_or(EscapeError::UnicodeInvalidDigit)?;
        digits += 1;
        if digits > 6 {
            return Err(EscapeError::UnicodeTooLong);
        }
        codepoint = codepoint * 16 + digit;
    }

    if digits == 0 {
        return Err(EscapeError::UnicodeEmpty);
    }
    char::from_u32(codepoint).ok_or(EscapeError::UnicodeNotScalar)
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
pub(super) fn parse_number(lexeme: &str) -> Result<f64, NumberError> {
    let prefix = lexeme.get(0..2);
    let radix = match prefix {
        Some("0b" | "0B") => 2,
        Some("0o" | "0O") => 8,
        Some("0x" | "0X") => 16,
        _ => 10,
    };

    if radix == 10 {
        if lexeme.contains(['e', 'E', '.']) {
            parse_float(lexeme)
        } else {
            parse_int(lexeme, 10)
        }
    } else {
        let rest = &lexeme[2..];
        parse_int(rest, radix)
    }
}

fn parse_float(digits: &str) -> Result<f64, NumberError> {
    let value = digits
        .replace("_", "")
        .parse::<f64>()
        .map_err(|_| NumberError::MissingDigits)?;

    if value.is_infinite() {
        Err(NumberError::OutOfRange)
    } else {
        Ok(value)
    }
}

fn parse_int(digits: &str, radix: u32) -> Result<f64, NumberError> {
    let n = u64::from_str_radix(&digits.replace("_", ""), radix).map_err(|e| match e.kind() {
        IntErrorKind::PosOverflow => NumberError::OutOfRange,
        IntErrorKind::Empty => NumberError::MissingDigits,
        _ => NumberError::InvalidCharacter,
    })?;
    let value = n as f64;
    if value as u128 != n as u128 {
        return Err(NumberError::ImpreciseInt);
    }
    Ok(value)
}

/// Strips `"""` delimiters and applies Swift-style indentation normalisation.
///
/// If the closing `"""` is on its own line, the whitespace that precedes it
/// on that line is treated as the "indent prefix" and stripped from the start
/// of every content line.  A leading newline immediately after the opening
/// `"""` is also removed.
pub(super) fn process_raw_string(lexeme: &str) -> String {
    // Strip opening and closing `"""`.
    let inner = &lexeme[3..lexeme.len() - 3];

    // Swift-style alignment: look for a final newline whose suffix is all whitespace.
    if let Some(last_nl) = inner.rfind('\n') {
        let indent = &inner[last_nl + 1..];

        if indent.chars().all(char::is_whitespace) {
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
