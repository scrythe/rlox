macro_rules! define_ast {
     ($expr_class:ident<$expr_lt:lifetime>;
     $($class_method_name:ident, $class_types:ident $(<$lt:lifetime>)? -> $($field_names:ident: $field_class_types:ty),+;)+) => {
        #[derive(Debug, PartialEq)]
        pub enum $expr_class<$expr_lt> {
            $($class_types(Box<$class_types $(<$lt>)? >)),+
        }
        impl<$expr_lt> $expr_class<$expr_lt> {
        $(
         pub fn $class_method_name ($($field_names: $field_class_types),+) -> $expr_class<$expr_lt> { $expr_class::$class_types(Box::new($class_types {$($field_names),*}))  }
        )+
        }
        $(
            #[derive(Debug, PartialEq)]
            pub struct $class_types $(<$lt>)? {
                $(pub $field_names: $field_class_types,)+
            }
        )+
    };
}

use crate::LoxError;
use crate::scanner::TokenType;

use super::scanner::LiteralValue;
use super::scanner::Token;

define_ast!(
    Expr<'a>;
    binary_expr, Binary<'a> -> left:Expr<'a> , operator: Token<'a> , right:Expr<'a> ;
    grouping_expr, Grouping<'a> -> expression:Expr<'a> ;
    literal_expr, Literal -> value: LiteralValue ;
    unary_expr, Unary<'a> -> operator:Token<'a> , right:Expr<'a> ;
);

pub struct Parser<'a, 'l> {
    tokens: Vec<Token<'a>>,
    current: usize,
    lox_error: &'l mut LoxError,
}

impl<'a, 'l> Parser<'a, 'l> {
    pub fn new(tokens: Vec<Token<'a>>, lox_error: &'l mut LoxError) -> Parser<'a, 'l> {
        let current = 0;
        Parser {
            tokens,
            current,
            lox_error,
        }
    }

    pub fn parse(mut self) -> Expr<'a> {
        match self.expression() {
            Ok(expression) => expression,
            Err(_) => Expr::literal_expr(LiteralValue::None),
        }
    }

    fn expression(&mut self) -> Result<Expr<'a>, ()> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'a>, ()> {
        // equality -> comparison ( ( "!=" | "==" ) comparison )*
        let mut expr = self.comparison()?;
        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'a>, ()> {
        // comparison -> term ( ( ">" | ">=" | "<" | "<=") term )*
        let mut expr = self.term()?;
        while self.match_token(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'a>, ()> {
        // term -> factor ( ( "-" | "+" ) factor )*
        let mut expr = self.factor()?;
        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'a>, ()> {
        // factor -> unary ( ( "/" | "*" ) unary )*
        let mut expr = self.unary()?;
        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'a>, ()> {
        // my attempt: unary -> ( "!" | "-" )* primary
        // unary -> ( "!" | "-" ) unary
        //       | primary
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            Ok(Expr::unary_expr(operator, right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr<'a>, ()> {
        // primary ->  Number | String | "true" | "false "| "nil" | "(" expression ")"
        if self.match_token(&[TokenType::False]) {
            Ok(Expr::literal_expr(LiteralValue::Bool(false)))
        } else if self.match_token(&[TokenType::True]) {
            Ok(Expr::literal_expr(LiteralValue::Bool(true)))
        } else if self.match_token(&[TokenType::Nil]) {
            Ok(Expr::literal_expr(LiteralValue::None))
        } else if self.match_token(&[TokenType::String, TokenType::Number]) {
            Ok(Expr::literal_expr(self.previous().literal.clone()))
        } else if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            Ok(Expr::grouping_expr(expr))
        } else {
            let token = self.peek().clone();
            self.error(&token, "Expect expression")
        }
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<&Token<'a>, ()> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let token = self.peek().clone();
            self.error(&token, message)
        }
    }

    fn match_token(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) -> &Token<'a> {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == *token_type
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token<'a> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token<'a> {
        &self.tokens[self.current - 1]
    }

    fn error<T>(&mut self, token: &Token, message: &str) -> Result<T, ()> {
        self.lox_error.error_token(token, message);
        Err(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Lox, scanner::TokenType};

    #[test]
    fn test_parser() {
        let tokens = vec![
            Token::new(
                TokenType::String,
                "\"hm\"",
                LiteralValue::String("hm".to_string()),
                1,
            ),
            Token::new(TokenType::BangEqual, "!=", LiteralValue::None, 1),
            Token::new(TokenType::Number, "5", LiteralValue::Number(5.0), 1),
            Token::new(TokenType::Eof, "", LiteralValue::None, 2),
        ];
        let mut lox = Lox::new();
        let parser = Parser::new(tokens, &mut lox.lox_error);
        let expression = parser.parse();
        assert_eq!(
            expression,
            Expr::binary_expr(
                Expr::literal_expr(LiteralValue::String("hm".to_string())),
                Token::new(TokenType::BangEqual, "!=", LiteralValue::None, 1),
                Expr::literal_expr(LiteralValue::Number(5.0))
            )
        );
    }
}
