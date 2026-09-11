pub(crate) use crate::scanner::span::Span;
pub use crate::scanner::token::{Token, TokenType};
use std::collections::HashMap;

mod span;
pub mod token;

pub struct Scanner<'src> {
    src: &'src str,
    key_words: HashMap<&'static str, TokenType>,
    start: usize,
    current: usize,
    line: u32,
    context_stack: Vec<InterpolationFrame>,
}

struct InterpolationFrame {
    /// Tracks `{` depth inside a `${...}` expression so that `}` inside
    /// nested blocks does not close the interpolation.
    brace_depth: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(src: &'src str) -> Self {
        Scanner {
            src,
            key_words: keywords(),
            start: 0,
            current: 0,
            line: 1,
            context_stack: Vec::new(),
        }
    }
    pub fn next_token(&mut self) -> Token<'src> {
        let error = self.skip_whitespace();
        if let Some(token) = error {
            return token;
        }

        self.start = self.current;
        if self.is_at_end() {
            let span = Span::new(self.current, self.current, self.line);
            return Token::new(TokenType::EOF, span, "end");
        }
        let char = self.advance();

        match char {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => {
                if let Some(frame) = self.context_stack.last_mut() {
                    frame.brace_depth += 1;
                }
                self.make_token(TokenType::LeftBrace)
            }
            '}' => {
                if let Some(frame) = self.context_stack.last_mut() {
                    if frame.brace_depth == 0 {
                        self.context_stack.pop();
                        // Exclude the `}` from the continuation lexeme.
                        self.start = self.current;
                        self.string()
                    } else {
                        frame.brace_depth -= 1;
                        self.make_token(TokenType::RightBrace)
                    }
                } else {
                    self.make_token(TokenType::RightBrace)
                }
            }
            '[' => self.make_token(TokenType::LeftBracket),
            ']' => self.make_token(TokenType::RightBracket),
            ',' => self.make_token(TokenType::Comma),
            ':' => self.make_token(TokenType::Colon),
            ';' => self.make_token(TokenType::Semicolon),
            '.' => self.make_token(TokenType::Dot),

            '+' if self.matches('=') => self.make_token(TokenType::PlusEqual),
            '+' => self.make_token(TokenType::Plus),

            '*' if self.matches('*') => self.make_token(TokenType::StarStar),
            '*' if self.matches('=') => self.make_token(TokenType::StarEqual),
            '*' => self.make_token(TokenType::Star),

            '%' if self.matches('=') => self.make_token(TokenType::PercentEqual),
            '%' => self.make_token(TokenType::Percent),

            '/' if self.matches('=') => self.make_token(TokenType::SlashEqual),
            '/' => self.make_token(TokenType::Slash),

            '-' if self.matches('=') => self.make_token(TokenType::MinusEqual),
            '-' => self.make_token(TokenType::Minus),

            '!' if self.matches('=') => self.make_token(TokenType::BangEqual),
            '!' => self.make_token(TokenType::Bang),

            '=' if self.matches('>') => self.make_token(TokenType::Arrow),
            '=' if self.matches('=') => self.make_token(TokenType::EqualEqual),
            '=' => self.make_token(TokenType::Equal),

            '<' if self.matches('=') => self.make_token(TokenType::LessEqual),
            '<' => self.make_token(TokenType::Less),

            '>' if self.matches('=') => self.make_token(TokenType::GreaterEqual),
            '>' => self.make_token(TokenType::Greater),

            '|' => self.make_token(TokenType::Pipe),

            '?' => {
                if self.matches('?') {
                    self.make_token(TokenType::QuestionQuestion)
                } else if self.matches('.') {
                    self.make_token(TokenType::QuestionDot)
                } else if self.matches('(') {
                    self.make_token(TokenType::QuestionParen)
                } else {
                    self.make_token(TokenType::Question)
                }
            }

            '"' => {
                // Three consecutive `"` for a raw string.
                if self.peek_char() == '"' && self.peek_next_char() == '"' {
                    self.advance(); // second "
                    self.advance(); // third "
                    self.raw_string()
                } else {
                    self.string()
                }
            }
            c @ '0'..='9' => self.number(c),
            c if is_valid_identifier(c) => self.identifier(),
            _ => self.make_token(TokenType::UnexpectedSymbolError),
        }
    }
    fn make_token(&self, token_type: TokenType) -> Token<'src> {
        let span = Span::new(self.start, self.current, self.line);
        Token::new(token_type, span, &self.src[self.start..self.current])
    }
    fn matches(&mut self, expected: char) -> bool {
        if self.peek_char() == expected {
            self.advance();
            true
        } else {
            false
        }
    }
    pub fn is_at_end(&self) -> bool {
        self.current >= self.src.len()
    }
    fn error(&self, message: &'static str) -> Token<'src> {
        let span = Span::new(self.start, self.current, self.line);
        Token::new(TokenType::Error, span, message)
    }
    fn advance(&mut self) -> char {
        let char = self.peek_char();
        self.current += char.len_utf8();
        char
    }
    fn peek_char(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.src[self.current..].chars().next().unwrap_or('\0')
    }

    fn peek_next_char(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.src[self.current..].chars().nth(1).unwrap_or('\0')
    }

    fn skip_whitespace(&mut self) -> Option<Token<'src>> {
        loop {
            match self.peek_char() {
                '\n' => {
                    self.advance();
                    self.line += 1;
                }
                c if c.is_whitespace() => {
                    self.advance();
                }
                '/' if self.peek_next_char() == '*' => {
                    self.advance();
                    self.advance();
                    let mut depth = 1;
                    while depth > 0 && !self.is_at_end() {
                        if self.peek_char() == '\n' {
                            self.line += 1;
                        } else if self.peek_char() == '*' && self.peek_next_char() == '/' {
                            depth -= 1;
                            self.advance();
                        } else if self.peek_char() == '/' && self.peek_next_char() == '*' {
                            depth += 1;
                            self.advance();
                        }

                        self.advance();
                    }
                    if depth > 0 {
                        return Some(self.error("Unterminated block comment."));
                    }
                }
                '/' if self.peek_next_char() == '/' => {
                    while self.peek_char() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                _ => return None,
            };
        }
    }

    /// Scans a regular (non-raw) string starting just after the opening `"`.
    /// Returns `String` when it is the final piece, or `PartialString`
    /// when `${` is encountered (leaving `current` past `${`).
    fn string(&mut self) -> Token<'src> {
        loop {
            if self.is_at_end() {
                return self.error("Unterminated string.");
            }
            let c = self.peek_char();
            match c {
                '"' => {
                    self.advance();
                    return self.make_token(TokenType::String);
                }
                '\\' => {
                    self.advance();
                    self.advance(); // escaped character - skip without interpretation
                }
                '$' if self.peek_next_char() == '{' => {
                    // Emit the content up to (not including) `${`.
                    let token = self.make_token(TokenType::PartialString);
                    self.advance(); // $
                    self.advance(); // {
                    self.context_stack
                        .push(InterpolationFrame { brace_depth: 0 });
                    return token;
                }
                '\n' => {
                    self.advance();
                    self.line += 1;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Scans a raw string `"""..."""`.  No escape processing, no interpolation.
    fn raw_string(&mut self) -> Token<'src> {
        loop {
            if self.is_at_end() {
                return self.error("Unterminated raw string.");
            }
            let c = self.advance();
            if c == '\n' {
                self.line += 1;
            } else if c == '"' {
                let c2 = self.peek_char();
                let c3 = self.peek_next_char();
                if c2 == '"' && c3 == '"' {
                    self.advance();
                    self.advance();
                    return self.make_token(TokenType::RawString);
                }
            }
        }
    }

    fn number(&mut self, first: char) -> Token<'src> {
        // Base prefixes
        if first == '0' {
            match self.peek_char() {
                'b' | 'B' => {
                    self.advance();
                    while matches!(self.peek_char(), '0' | '1' | '_') {
                        self.advance();
                    }
                    return self.make_token(TokenType::Number);
                }
                'o' | 'O' => {
                    self.advance();
                    while matches!(self.peek_char(), '0'..='7' | '_') {
                        self.advance();
                    }
                    return self.make_token(TokenType::Number);
                }
                'x' | 'X' => {
                    self.advance();
                    while self.peek_char().is_ascii_hexdigit() || self.peek_char() == '_' {
                        self.advance();
                    }
                    return self.make_token(TokenType::Number);
                }
                _ => {}
            }
        }

        // Decimal part
        while is_valid_digit(self.peek_char()) {
            self.advance();
        }

        // Fractional part
        if self.peek_char() == '.' && self.peek_next_char().is_ascii_digit() {
            self.advance(); // consume '.'
            while is_valid_digit(self.peek_char()) {
                self.advance();
            }
        }

        // Exponent part: `e` or `E`, optional sign, digits.
        if matches!(self.peek_char(), 'e' | 'E') {
            self.advance();
            if matches!(self.peek_char(), '+' | '-') {
                self.advance();
            }
            while is_valid_digit(self.peek_char()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token<'src> {
        while self.peek_char().is_alphanumeric() || self.peek_char() == '_' {
            self.advance();
        }
        let token_type = self
            .key_words
            .get(&self.src[self.start..self.current])
            .unwrap_or(&TokenType::Identifier);

        self.make_token(token_type.clone())
    }
}

fn is_valid_identifier(identifier_char: char) -> bool {
    identifier_char.is_alphabetic() || identifier_char == '_'
}

fn is_valid_digit(digit_char: char) -> bool {
    digit_char.is_ascii_digit() || digit_char == '_'
}

fn keywords() -> HashMap<&'static str, TokenType> {
    HashMap::from([
        ("try", TokenType::Try),
        ("and", TokenType::And),
        ("or", TokenType::Or),
        ("is", TokenType::Is),
        ("if", TokenType::If),
        ("else", TokenType::Else),
        ("true", TokenType::True),
        ("false", TokenType::False),
        ("func", TokenType::Func),
        ("return", TokenType::Return),
        ("let", TokenType::Let),
        ("while", TokenType::While),
        ("struct", TokenType::Struct),
        ("impl", TokenType::Impl),
        ("self", TokenType::Self_),
        ("interface", TokenType::Interface),
        ("extern", TokenType::Extern),
        ("nil", TokenType::Nil),
        ("enum", TokenType::Enum),
        ("match", TokenType::Match),
    ])
}
