use crate::LoxError;
use std::{collections::HashMap, str};

pub struct ScanError();

pub struct Scanner<'a> {
    source: &'a [u8],
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: u32,
    token_keyword_map: HashMap<&'static str, TokenType>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        let source = source.as_bytes();
        let tokens = Vec::new();
        let start = 0;
        let current = 0;
        let line = 1;
        let token_keyword_map = TokenType::get_token_keyword_map();
        Scanner {
            source,
            tokens,
            start,
            current,
            line,
            token_keyword_map,
        }
    }
    pub fn scan_tokens(mut self) -> (Vec<Token<'a>>, bool) {
        let mut has_scan_error = false;
        while !self.is_at_end() {
            self.start = self.current;
            if self.scan_token().is_err() {
                has_scan_error = true;
            };
        }
        self.tokens.push(Token::new(
            TokenType::Eof,
            "",
            LiteralValue::None,
            self.line,
        ));
        (self.tokens, has_scan_error)
    }

    fn scan_token(&mut self) -> Result<(), ScanError> {
        let c = self.advance();
        match c {
            b'(' => self.add_token(TokenType::LeftParen),
            b')' => self.add_token(TokenType::RightParen),
            b'{' => self.add_token(TokenType::LeftBrace),
            b'}' => self.add_token(TokenType::RightBrace),
            b',' => self.add_token(TokenType::Comma),
            b'.' => self.add_token(TokenType::Dot),
            b'-' => self.add_token(TokenType::Minus),
            b'+' => self.add_token(TokenType::Plus),
            b';' => self.add_token(TokenType::Semicolon),
            b'*' => self.add_token(TokenType::Star),

            b'!' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type);
            }
            b'=' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type);
            }
            b'<' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type);
            }
            b'>' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type);
            }
            b'/' => {
                if self.match_char(b'/') {
                    while self.peek() != b'\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }

            b' ' => {}
            b'\r' => {}
            b'\t' => {}

            b'\n' => self.line += 1,

            b'"' => self.string()?,

            b'0'..=b'9' => self.number(),

            b'a'..=b'z' => self.identifier(),
            b'A'..=b'Z' => self.identifier(),
            b'_' => self.identifier(),

            c => {
                LoxError::error_line(self.line, &format!("Unexpected character {}.", c));
                return Err(ScanError());
            }
        }
        Ok(())
    }

    fn advance(&mut self) -> u8 {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    fn match_char(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.source[self.current]
        }
    }
    fn peek_next(&self) -> u8 {
        if self.current + 1 >= self.source.len() {
            b'\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_literal(token_type, LiteralValue::None);
    }

    fn add_token_literal(&mut self, token_type: TokenType, literal: LiteralValue) {
        let text = &self.source[self.start..self.current];
        let text = str::from_utf8(text).unwrap();
        self.tokens
            .push(Token::new(token_type, text, literal, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn string(&mut self) -> Result<(), ScanError> {
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            LoxError::error_line(self.line, "Unterminated string.");
            return Err(ScanError());
        }
        // for closing "
        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        let value = str::from_utf8(value).unwrap().to_string();
        self.add_token_literal(TokenType::String, LiteralValue::String(value));
        Ok(())
    }

    fn is_digit(c: u8) -> bool {
        // c >= b'0' && c <= b'9'
        // (b'0'..=b'9').contains(&c)
        c.is_ascii_digit()
    }

    fn number(&mut self) {
        while Scanner::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' && Scanner::is_digit(self.peek_next()) {
            // Consume '.'
            self.advance();

            while Scanner::is_digit(self.peek()) {
                self.advance();
            }
        }

        let number = &self.source[self.start..self.current];
        let number = str::from_utf8(number).unwrap();
        let number: f64 = number.parse().unwrap();
        self.add_token_literal(TokenType::Number, LiteralValue::Number(number));
    }

    fn is_alpha_numeric(c: u8) -> bool {
        c.is_ascii_alphanumeric()
    }

    fn identifier(&mut self) {
        while Scanner::is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let text = str::from_utf8(text).unwrap();
        let token_type = self
            .token_keyword_map
            .get(text)
            .unwrap_or(&TokenType::Identifier);
        self.add_token(token_type.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    String,
    Number,
    Identifier,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl TokenType {
    fn get_token_keyword_map() -> HashMap<&'static str, TokenType> {
        let mut token_keyword_map = HashMap::new();
        token_keyword_map.insert("and", TokenType::And);
        token_keyword_map.insert("class", TokenType::Class);
        token_keyword_map.insert("else", TokenType::Else);
        token_keyword_map.insert("false", TokenType::False);
        token_keyword_map.insert("for", TokenType::For);
        token_keyword_map.insert("fun", TokenType::Fun);
        token_keyword_map.insert("if", TokenType::If);
        token_keyword_map.insert("nil", TokenType::Nil);
        token_keyword_map.insert("or", TokenType::Or);
        token_keyword_map.insert("print", TokenType::Print);
        token_keyword_map.insert("return", TokenType::Return);
        token_keyword_map.insert("super", TokenType::Super);
        token_keyword_map.insert("this", TokenType::This);
        token_keyword_map.insert("true", TokenType::True);
        token_keyword_map.insert("var", TokenType::Var);
        token_keyword_map.insert("while", TokenType::While);
        token_keyword_map
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    None,
    String(String),
    Number(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub literal: LiteralValue,
    pub line: u32,
}

impl<'a> Token<'a> {
    pub fn new(
        token_type: TokenType,
        lexeme: &'a str,
        literal: LiteralValue,
        line: u32,
    ) -> Token<'a> {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_scanner() {
        let source = "// aaa \n / b * + \n \"hm\" 5";
        let scanner = Scanner::new(source);
        let (tokens, has_scan_error) = scanner.scan_tokens();
        if has_scan_error {
            panic!("Unexpected scan error.")
        }
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::Slash, "/", LiteralValue::None, 2),
                Token::new(TokenType::Identifier, "b", LiteralValue::None, 2),
                Token::new(TokenType::Star, "*", LiteralValue::None, 2),
                Token::new(TokenType::Plus, "+", LiteralValue::None, 2),
                Token::new(
                    TokenType::String,
                    "\"hm\"",
                    LiteralValue::String("hm".to_string()),
                    3
                ),
                Token::new(TokenType::Number, "5", LiteralValue::Number(5.0), 3),
                Token::new(TokenType::Eof, "", LiteralValue::None, 3),
            ]
        );
    }
}
