use crate::LoxError;
use std::{collections::HashMap, mem::take, str};

macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {
        $sub
    };
}

macro_rules! token_keyword_map {
    ($($key:literal => $value:expr),*) => {{
        let count = <[()]>::len(&[$(replace_expr!($key())),*]);
        let mut token_keyword_map = HashMap::with_capacity(count);
        $(token_keyword_map.insert($key, $value);)*
        token_keyword_map
    }};
}

pub struct ScanError();

pub struct Scanner<'source> {
    source: &'source [u8],
    tokens: Vec<Token>,
    pub start: usize,
    pub current: usize,
    line: u32,
    token_keyword_map: HashMap<&'static str, TokenType>,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Scanner<'source> {
        let source = source.as_bytes();
        let tokens = Vec::new();
        let start = 0;
        let current = 0;
        let line = 1;
        // let token_keyword_map = TokenType::get_token_keyword_map();
        let token_keyword_map = token_keyword_map! {
            "and"=> TokenType::And,
            "class"=> TokenType::Class,
            "else"=> TokenType::Else,
            "false"=> TokenType::False,
            "for"=> TokenType::For,
            "fun"=> TokenType::Fun,
            "if"=> TokenType::If,
            "nil"=> TokenType::Nil,
            "or"=> TokenType::Or,
            "print"=> TokenType::Print,
            "return"=> TokenType::Return,
            "super"=> TokenType::Super,
            "this"=> TokenType::This,
            "true"=> TokenType::True,
            "var"=> TokenType::Var,
            "while"=> TokenType::While
        };
        Scanner {
            source,
            tokens,
            start,
            current,
            line,
            token_keyword_map,
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<Token>, bool) {
        let mut has_scan_error = false;
        while !self.is_at_end() {
            self.start = self.current;
            if self.scan_token().is_err() {
                has_scan_error = true;
            };
        }
        self.tokens
            .push(Token::new(TokenType::Eof, self.start as u32, self.line));
        let tokens = take(&mut self.tokens);
        (tokens, has_scan_error)
    }

    pub fn scan_token(&mut self) -> Result<(), ScanError> {
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
        self.tokens
            .push(Token::new(token_type, self.start as u32, self.line));
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

        self.add_token(TokenType::String);
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

        self.add_token(TokenType::Number);
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

    pub fn get_lexeme(&mut self) -> &[u8] {
        &self.source[self.start..self.current]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub start: u32,
    pub line: u32,
}

impl Token {
    pub fn new(token_type: TokenType, start: u32, line: u32) -> Token {
        Token {
            token_type,
            start,
            line,
        }
    }
}
