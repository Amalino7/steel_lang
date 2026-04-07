/// Handles escape characters
pub fn process_escapes(raw: &str) -> Result<String, String> {
    let mut result = String::with_capacity(raw.len());
    let mut chars = raw.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => result.push('\n'),
            Some('t') => result.push('\t'),
            Some('r') => result.push('\r'),
            Some('\\') => result.push('\\'),
            Some('"') => result.push('"'),
            Some('0') => result.push('\0'),
            Some('$') => result.push('$'),
            Some('u') => {
                // Expect `{HHHH}` (1-6 hex digits).
                match chars.next() {
                    Some('{') => {}
                    _ => return Err("Expected '{' after \\u in unicode escape.".to_string()),
                }
                let mut hex = String::with_capacity(6);
                loop {
                    match chars.next() {
                        Some('}') => break,
                        Some(h) if h.is_ascii_hexdigit() => hex.push(h),
                        Some(_) => {
                            return Err(
                                "Invalid character in unicode escape; expected hex digit or '}'."
                                    .to_string(),
                            );
                        }
                        None => return Err("Unterminated unicode escape.".to_string()),
                    }
                    if hex.len() > 6 {
                        return Err("Unicode escape must be at most 6 hex digits.".to_string());
                    }
                }
                if hex.is_empty() {
                    return Err("Unicode escape must have at least one hex digit.".to_string());
                }
                let codepoint = u32::from_str_radix(&hex, 16)
                    .map_err(|_| "Invalid unicode escape.".to_string())?;
                let ch = char::from_u32(codepoint)
                    .ok_or_else(|| "Unicode escape is not a valid scalar value.".to_string())?;
                result.push(ch);
            }
            Some(other) => {
                return Err(format!("Unknown escape sequence '\\{other}'."));
            }
            None => return Err("Trailing backslash at end of string.".to_string()),
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
pub fn parse_number(lexeme: &str) -> Option<f64> {
    let strip_underscores = |s: &str| -> String { s.chars().filter(|&c| c != '_').collect() };

    if let Some(rest) = lexeme
        .strip_prefix("0b")
        .or_else(|| lexeme.strip_prefix("0B"))
    {
        let digits = strip_underscores(rest);
        i64::from_str_radix(&digits, 2).ok().map(|n| n as f64)
    } else if let Some(rest) = lexeme
        .strip_prefix("0o")
        .or_else(|| lexeme.strip_prefix("0O"))
    {
        let digits = strip_underscores(rest);
        i64::from_str_radix(&digits, 8).ok().map(|n| n as f64)
    } else if let Some(rest) = lexeme
        .strip_prefix("0x")
        .or_else(|| lexeme.strip_prefix("0X"))
    {
        let digits = strip_underscores(rest);
        i64::from_str_radix(&digits, 16).ok().map(|n| n as f64)
    } else {
        strip_underscores(lexeme).parse::<f64>().ok()
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
