use std::{
    iter::Peekable,
    str::{Chars, from_utf8},
    thread::current,
};

use crate::LoxError;

pub struct Scanner<'a> {
    source: &'a [u8],
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        let source = source.as_bytes();
        let tokens = Vec::new();
        let start = 0;
        let current = 0;
        let line = 1;
        Scanner {
            source,
            tokens,
            start,
            current,
            line,
        }
    }
    pub fn scan_tokens(mut self, lox_error: &mut LoxError) -> Vec<Token<'a>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token(lox_error);
        }
        self.tokens
            .push(Token::new(TokenType::EOF, b"", "", self.line));
        self.tokens
    }

    fn scan_token(&mut self, lox_error: &mut LoxError) {
        let c = self.advance();
        match c {
            b'(' => self.add_token(TokenType::LEFT_PAREN),
            b')' => self.add_token(TokenType::RIGHT_PAREN),
            b'{' => self.add_token(TokenType::LEFT_BRACE),
            b'}' => self.add_token(TokenType::RIGHT_BRACE),
            b',' => self.add_token(TokenType::COMMA),
            b'.' => self.add_token(TokenType::DOT),
            b'-' => self.add_token(TokenType::MINUS),
            b'+' => self.add_token(TokenType::PLUS),
            b';' => self.add_token(TokenType::SEMICOLON),
            b'*' => self.add_token(TokenType::STAR),

            b'!' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::BANG_EQUAL
                } else {
                    TokenType::BANG
                };
                self.add_token(token_type);
            }
            b'=' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::EQUAL_EQUAL
                } else {
                    TokenType::EQUAL
                };
                self.add_token(token_type);
            }
            b'<' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::LESS_EQUAL
                } else {
                    TokenType::LESS
                };
                self.add_token(token_type);
            }
            b'>' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::GREATER_EQUAL
                } else {
                    TokenType::GREATER
                };
                self.add_token(token_type);
            }
            b'/' => {
                if self.match_char(b'/') {
                    while self.peek() != b'\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::SLASH);
                }
            }

            c => {
                lox_error.error(
                    self.line,
                    &format!(
                        "Unexpected character {}",
                        c // std::str::from_utf8(&[c]).unwrap()
                    ),
                );
            }
        }
        // todo!()
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

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_literal(token_type, "");
    }

    fn add_token_literal(&mut self, token_type: TokenType, literal: &'a str) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(token_type, text, literal, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[derive(Debug)]
enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    EOF,
}

#[derive(Debug)]
pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a [u8],
    literal: &'a str,
    line: u32,
}

impl<'a> Token<'a> {
    fn new(token_type: TokenType, lexeme: &'a [u8], literal: &'a str, line: u32) -> Token<'a> {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}
