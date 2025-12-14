use crate::token::{Token, TokenType};
use std::collections::HashMap;

pub struct Scanner<'src> {
    src: &'src str,
    key_words: HashMap<&'static str, TokenType>,
    start: usize,
    current: usize,
    line: u32,
}

impl<'src> Scanner<'src> {
    pub fn new(src: &'src str) -> Self {
        Scanner {
            src,
            key_words: keywords(),
            start: 0,
            current: 0,
            line: 1,
        }
    }
    pub fn next_token(&mut self) -> Token<'src> {
        let error = self.skip_whitespace();
        if let Some(token) = error {
            return token;
        }

        self.start = self.current;
        if self.is_at_end() {
            return Token::new(TokenType::EOF, self.line, "end");
        }
        let char = self.advance();

        match char {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            '[' => self.make_token(TokenType::LeftBracket),
            ']' => self.make_token(TokenType::RightBracket),
            ',' => self.make_token(TokenType::Comma),
            ':' => self.make_token(TokenType::Colon),
            ';' => self.make_token(TokenType::Semicolon),
            '.' => self.make_token(TokenType::Dot),

            '+' if self.matches('=') => self.make_token(TokenType::PlusEqual),
            '+' => self.make_token(TokenType::Plus),

            '*' if self.matches('=') => self.make_token(TokenType::StarEqual),
            '*' => self.make_token(TokenType::Star),

            '/' if self.matches('=') => self.make_token(TokenType::SlashEqual),
            '/' => self.make_token(TokenType::Slash),

            '-' if self.matches('=') => self.make_token(TokenType::MinusEqual),
            '-' => self.make_token(TokenType::Minus),

            '!' if self.matches('=') => self.make_token(TokenType::BangEqual),
            '!' => self.make_token(TokenType::Bang),

            '=' if self.matches('=') => self.make_token(TokenType::EqualEqual),
            '=' => self.make_token(TokenType::Equal),

            '<' if self.matches('=') => self.make_token(TokenType::LessEqual),
            '<' => self.make_token(TokenType::Less),

            '>' if self.matches('=') => self.make_token(TokenType::GreaterEqual),
            '>' => self.make_token(TokenType::Greater),

            '?' => {
                if self.matches('?') {
                    self.make_token(TokenType::QuestionQuestion)
                } else if self.matches('.') {
                    self.make_token(TokenType::QuestionDot)
                } else {
                    self.make_token(TokenType::Question)
                }
            }

            '"' => self.string(),
            '0'..='9' => self.number(),
            c if is_valid_identifier(c) => self.identifier(),
            _ => self.make_token(TokenType::UnexpectedSymbolError),
        }
    }
    fn make_token(&self, token_type: TokenType) -> Token<'src> {
        Token::new(token_type, self.line, &self.src[self.start..self.current])
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
        Token::new(TokenType::Error, self.line, message)
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

    fn string(&mut self) -> Token<'src> {
        while self.peek_char() != '"' && !self.is_at_end() {
            if self.peek_char() == '\n' {
                self.line += 1
            } else if self.peek_char() == '\\' {
                self.advance();
            }
            self.advance();
        }
        if self.is_at_end() {
            return self.error("Unterminated string.");
        }
        self.advance();
        self.make_token(TokenType::String)
    }

    fn number(&mut self) -> Token<'src> {
        let mut is_float = false;
        while self.peek_char().is_ascii_digit() || self.peek_char() == '.' {
            if self.peek_char() == '.' {
                if is_float {
                    return self.make_token(TokenType::Number);
                } else if self.peek_next_char().is_ascii_digit() {
                    is_float = true;
                } else {
                    return self.make_token(TokenType::Number);
                }
            }
            self.advance();
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

fn keywords() -> HashMap<&'static str, TokenType> {
    HashMap::from([
        ("and", TokenType::And),
        ("or", TokenType::Or),
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
        ("nil", TokenType::Nil),
    ])
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_basic_lexing() {
        let input = "let five = 5;
        let ten = 10;
        let add = func(x, y) {
            x + y;
        };";
        let mut scanner = Scanner::new(input);
        assert_eq!(scanner.next_token(), Token::new(TokenType::Let, 1, "let"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 1, "five")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Equal, 1, "="));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 1, "5"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Semicolon, 1, ";")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Let, 2, "let"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 2, "ten")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Equal, 2, "="));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 2, "10"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Semicolon, 2, ";")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Let, 3, "let"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 3, "add")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Equal, 3, "="));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Func, 3, "func"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::LeftParen, 3, "(")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 3, "x")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Comma, 3, ","));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 3, "y")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::RightParen, 3, ")")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::LeftBrace, 3, "{")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 4, "x")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Plus, 4, "+"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 4, "y")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Semicolon, 4, ";")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::RightBrace, 5, "}")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Semicolon, 5, ";")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::EOF, 5, "end"));
    }
    #[test]
    fn test_unterminated_string() {
        let input = "\"Hello World!";
        let mut scanner = Scanner::new(input);
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Error, 1, "Unterminated string.")
        );
    }
    #[test]
    fn test_unterminated_comment() {
        let input = "/* Hello World!";
        let mut scanner = Scanner::new(input);
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Error, 1, "Unterminated block comment.")
        );
    }
    #[test]
    fn test_error_token() {
        let input = "?";
        let mut scanner = Scanner::new(input);
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::UnexpectedSymbolError, 1, "?")
        );
    }
    #[test]
    fn test_utf8_identifier() {
        let input = "ʃɨðœεæɔ";
        let mut scanner = Scanner::new(input);
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 1, "ʃɨðœεæɔ")
        );
    }
    #[test]
    fn test_emoji_identifier() {
        let input = "😏";
        let mut scanner = Scanner::new(input);
        // emojis not yet supported
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::UnexpectedSymbolError, 1, "😏")
        );
    }
    #[test]
    fn test_utf8_string() {
        let input = r#""😏😏fdj12æœɨʲεɣεþəɣðʃɣʲ""#;
        let mut scanner = Scanner::new(input);
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::String, 1, r#""😏😏fdj12æœɨʲεɣεþəɣðʃɣʲ""#)
        );
    }

    #[test]
    fn test_block_comment() {
        let input = "8/* Hello World!
        gfj
        ggf
        */ + 5";
        let mut scanner = Scanner::new(input);
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 1, "8"));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Plus, 4, "+"));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 4, "5"));
        assert_eq!(scanner.next_token(), Token::new(TokenType::EOF, 4, "end"));
    }
    #[test]
    fn test_nested_block_comment() {
        let input = "8 /* Hello World!
            /*
              nested comment
            */
            485
        */ + 6";
        let mut scanner = Scanner::new(input);
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 1, "8"));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Plus, 6, "+"));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 6, "6"));
        assert_eq!(scanner.next_token(), Token::new(TokenType::EOF, 6, "end"));
    }
    #[test]
    fn test_multiline_string() {
        let input = "\"Hello World!\ngfj\nggf\n\"";
        let mut scanner = Scanner::new(input);
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::String, 4, "\"Hello World!\ngfj\nggf\n\"")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::EOF, 4, "end"));
    }

    #[test]
    fn test_empty_string() {
        let source = r#"let a = "";"#;
        let mut scanner = Scanner::new(source);
        assert_eq!(scanner.next_token(), Token::new(TokenType::Let, 1, "let"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 1, "a")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Equal, 1, "="));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::String, 1, "\"\"")
        );
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Semicolon, 1, ";")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::EOF, 1, "end"));
    }
    #[test]
    fn test_escape_character() {
        let source = r#"let a = "\"";"#;
        let mut scanner = Scanner::new(source);
        assert_eq!(scanner.next_token(), Token::new(TokenType::Let, 1, "let"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 1, "a")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Equal, 1, "="));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::String, 1, r#""\"""#)
        )
    }
    #[test]
    fn test_underscore_identifier() {
        let source = "let _ = 10; let __ = 20;";
        let mut scanner = Scanner::new(source);
        assert_eq!(scanner.next_token(), Token::new(TokenType::Let, 1, "let"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 1, "_")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Equal, 1, "="));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 1, "10"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Semicolon, 1, ";")
        );

        assert_eq!(scanner.next_token(), Token::new(TokenType::Let, 1, "let"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Identifier, 1, "__")
        );
        assert_eq!(scanner.next_token(), Token::new(TokenType::Equal, 1, "="));
        assert_eq!(scanner.next_token(), Token::new(TokenType::Number, 1, "20"));
        assert_eq!(
            scanner.next_token(),
            Token::new(TokenType::Semicolon, 1, ";")
        );
    }
}
