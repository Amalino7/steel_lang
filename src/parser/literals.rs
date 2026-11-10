use crate::parser::error::{EscapeError, NumberError};
use std::borrow::Cow;
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
/// The scanner only finds where the literal ends, so everything else is checked here: characters that don't
/// fit the shape (`123abc`, `0b12`) are [`NumberError::InvalidCharacter`], and a prefix or exponent without
/// digits (`0x`, `1e`) is [`NumberError::MissingDigits`]. Integer literals must fit in a `u64` and be exact as an `f64`
/// ([`NumberError::ImpreciseInt`] otherwise); float literals are rounded and only fail when infinite.
pub(super) fn parse_number(lexeme: &str) -> Result<f64, NumberError> {
    let prefix = lexeme.get(0..2);
    let radix = match prefix {
        Some("0b" | "0B") => 2,
        Some("0o" | "0O") => 8,
        Some("0x" | "0X") => 16,
        _ => 10,
    };

    if radix == 10 {
        if is_float_shape(lexeme)? {
            parse_float(lexeme)
        } else {
            parse_int(lexeme, 10)
        }
    } else {
        let rest = &lexeme[2..];
        parse_int(rest, radix)
    }
}

/// Checks that a decimal literal is `digits[.digits][(e|E)[+|-]digits]` and returns whether it is a float.
fn is_float_shape(lexeme: &str) -> Result<bool, NumberError> {
    fn skip_digits(s: &str) -> &str {
        s.trim_start_matches(|c: char| c.is_ascii_digit() || c == '_')
    }

    let rest = skip_digits(lexeme);
    let (fraction, rest) = match rest.strip_prefix('.') {
        Some(fraction) => (true, skip_digits(fraction)),
        None => (false, rest),
    };
    let Some(exponent) = rest.strip_prefix(['e', 'E']) else {
        return if rest.is_empty() {
            Ok(fraction)
        } else {
            Err(NumberError::InvalidCharacter)
        };
    };

    let exponent = exponent.strip_prefix(['+', '-']).unwrap_or(exponent);
    let rest = skip_digits(exponent);
    if !rest.is_empty() {
        Err(NumberError::InvalidCharacter)
    } else if exponent.is_empty() {
        Err(NumberError::MissingDigits)
    } else {
        Ok(true)
    }
}

fn parse_float(digits: &str) -> Result<f64, NumberError> {
    let value = strip_underscores(digits)
        .parse::<f64>()
        .map_err(|_| NumberError::MissingDigits)?;

    if value.is_infinite() {
        Err(NumberError::OutOfRange)
    } else {
        Ok(value)
    }
}

fn parse_int(digits: &str, radix: u32) -> Result<f64, NumberError> {
    let n = u64::from_str_radix(&strip_underscores(digits), radix).map_err(|e| match e.kind() {
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

/// `digits` without its `_` separators, borrowed when it has none.
pub(super) fn strip_underscores(digits: &str) -> Cow<'_, str> {
    if digits.contains('_') {
        Cow::Owned(digits.replace('_', ""))
    } else {
        Cow::Borrowed(digits)
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number_values() {
        assert_eq!(parse_number("1_000"), Ok(1000.0));
        assert_eq!(parse_number("1.5e3"), Ok(1500.0));
        assert_eq!(parse_number("2E-1"), Ok(0.2));
        assert_eq!(parse_number("1e+2"), Ok(100.0));
        assert_eq!(parse_number("0b1_01"), Ok(5.0));
        assert_eq!(parse_number("0o17"), Ok(15.0));
        assert_eq!(parse_number("0xFf"), Ok(255.0));
    }

    #[test]
    fn test_parse_number_missing_digits() {
        for lexeme in ["0x", "0b_", "1e", "1E+", "1.5e-", "1e_"] {
            assert_eq!(
                parse_number(lexeme),
                Err(NumberError::MissingDigits),
                "{lexeme}"
            );
        }
    }

    #[test]
    fn test_parse_number_invalid_characters() {
        for lexeme in [
            "123abc", "0b12", "0o79", "0x1g", "0bz", "1e5x", "1ex", "1e+x", "1.5_f", "0z",
        ] {
            assert_eq!(
                parse_number(lexeme),
                Err(NumberError::InvalidCharacter),
                "{lexeme}"
            );
        }
    }
}
