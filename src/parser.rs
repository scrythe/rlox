pub struct AstPrinter {}

macro_rules! define_single_ast {
    // Binary          <'a>:                Expr: left, Expr: right
    ($class_type:ident $(<$lt:lifetime>)? -> $($field_names:ident: $field_class_types:ty),+;) => {
        #[derive(Debug)]
        struct $class_type $(<$lt>)? {
            $($field_names: $field_class_types,)+
        }

        impl $(<$lt>)?  $class_type $(<$lt>)? {
            fn new($($field_names: $field_class_types,)+)->$class_type $(<$lt>)? {
                $class_type {$($field_names),*}
            }
            fn boxed_new($($field_names: $field_class_types,)+)->Box<$class_type $(<$lt>)?> {
                Box::new($class_type {$($field_names),*})
            }
        }
    };
}

macro_rules! define_ast {
     ($expr_class:ident<$expr_lt:lifetime>;
     $($class_type_expr:ident, $class_types:ident $(<$lt:lifetime>)? -> $($field_names:ident: $field_class_types:ty),+;)+) => {
        #[derive(Debug)]
        pub enum $expr_class<$expr_lt> {
            $($class_types(Box<$class_types $(<$lt>)? >)),+
        }
        impl<$expr_lt> $expr_class<$expr_lt> {
        $(
         fn $class_type_expr ($($field_names: $field_class_types),+) -> $expr_class<$expr_lt> { $expr_class::$class_types(Box::new($class_types {$($field_names),*}))  }
        )+
        }
        $(
            define_single_ast!($class_types $(<$lt>)? -> $($field_names: $field_class_types),+;);
        )+
    };
}

use crate::scanner::TokenType;

use super::scanner::LiteralValue;
use super::scanner::Token;

define_ast!(
    Expr<'a>;
    binary_expr, Binary<'a> -> left:Expr<'a> , operator: Token<'a> , right:Expr<'a> ;
    grouping_expr, Grouping<'a> -> expression:Expr<'a> ;
    literal_expr, Literal<'a> -> value: LiteralValue<'a> ;
    unary_expr, Unary<'a> -> operator:Token<'a> , right:Expr<'a> ;
);

impl AstPrinter {
    // pub fn print_stdout(expr: Expr) {
    //     let result = AstPrinter::print(expr);
    //     println!("{result}");
    // }
    pub fn print(expr: Expr) -> String {
        match expr {
            Expr::Binary(binary_expr) => {
                format!(
                    "({} {} {})",
                    binary_expr.operator.lexeme,
                    AstPrinter::print(binary_expr.left),
                    AstPrinter::print(binary_expr.right)
                )
            }
            Expr::Grouping(group_expr) => {
                format!("(group {})", AstPrinter::print(group_expr.expression))
            }
            Expr::Literal(literal_expr) => match literal_expr.value {
                LiteralValue::None => String::from(""),
                LiteralValue::String(text) => text.to_string(),
                LiteralValue::Number(val) => val.to_string(),
                LiteralValue::False => false.to_string(),
                LiteralValue::True => true.to_string(),
            },
            Expr::Unary(unary_expr) => format!(
                "({} {})",
                unary_expr.operator.lexeme,
                AstPrinter::print(unary_expr.right),
            ),
        }
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser<'a> {
        let current = 0;
        Parser { tokens, current }
    }

    pub fn parse(&mut self) -> Expr<'a> {
        self.expression()
    }

    fn expression(&mut self) -> Expr<'a> {
        self.equality()
    }

    fn equality(&mut self) -> Expr<'a> {
        // equality -> comparison ( ( "!=" | "==" ) comparison )*
        let mut expr = self.comparison();
        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison();
            expr = Expr::binary_expr(expr, operator, right);
        }
        expr
    }

    fn comparison(&mut self) -> Expr<'a> {
        // comparison -> term ( ( ">" | ">=" | "<" | "<=") term )*
        let mut expr = self.term();
        while self.match_token(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term();
            // expr = Expr::BinaryExpr(expr, operator, right);
            expr = Expr::binary_expr(expr, operator, right);
        }
        expr
    }

    fn term(&mut self) -> Expr<'a> {
        // term -> factor ( ( "-" | "+" ) factor )*
        let mut expr = self.factor();
        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor();
            expr = Expr::binary_expr(expr, operator, right);
        }
        expr
    }

    fn factor(&mut self) -> Expr<'a> {
        // factor -> unary ( ( "/" | "*" ) unary )*
        let mut expr = self.unary();
        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary();
            expr = Expr::binary_expr(expr, operator, right);
        }
        expr
    }

    fn unary(&mut self) -> Expr<'a> {
        // my attempt: unary -> ( "!" | "-" )* primary
        // unary -> ( "!" | "-" ) unary
        //       | primary
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary();
            Expr::Unary(Unary::boxed_new(operator, right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expr<'a> {
        if self.match_token(&[TokenType::False]) {
            return Expr::literal_expr(LiteralValue::False);
        }
        if self.match_token(&[TokenType::True]) {
            return Expr::literal_expr(LiteralValue::True);
        }
        if self.match_token(&[TokenType::Nil]) {
            return Expr::literal_expr(LiteralValue::None);
        }

        if self.match_token(&[TokenType::String, TokenType::Number]) {
            let previous = self.previous();
            if previous.token_type == TokenType::String {
                return Expr::literal_expr(LiteralValue::String(previous.lexeme));
            } else {
                let number: f64 = previous.lexeme.parse().unwrap();
                return Expr::literal_expr(LiteralValue::Number(number));
            }
        }

        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(&TokenType::RightParen, "Expect ')' after expression.");
            return Expr::grouping_expr(expr);
        }
        // TODO:
        panic!()
    }

    fn consume(&mut self, token_type: &TokenType, error_message: &str) -> &Token<'a> {
        if self.check(token_type) {
            return self.advance();
        }
        // TODO:
        panic!("{}", error_message);
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
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::TokenType;

    #[test]
    fn test_parser() {
        // let concat_idents!(hm, hm) = 5;
        // let test!(hm) = 5;
        let tokens = vec![
            Token::new(TokenType::String, "hm", LiteralValue::String("hm"), 1),
            Token::new(TokenType::BangEqual, "!=", LiteralValue::None, 1),
            Token::new(TokenType::Number, "5", LiteralValue::Number(5.0), 1),
            Token::new(TokenType::Eof, "", LiteralValue::None, 2),
        ];
        let mut parser = Parser::new(tokens);
        dbg!(parser.expression());
    }

    #[test]
    fn test_ast() {
        let token = Token::new(TokenType::Plus, "+", LiteralValue::None, 1);
        let expr = Binary::new(
            Expr::literal_expr(LiteralValue::Number(5.0)),
            token,
            Expr::literal_expr(LiteralValue::Number(4.3)),
        );
        let expr = Expr::Binary(Box::new(expr));
        let out = AstPrinter::print(expr);
        assert_eq!(out, "+ 5 4.3")
    }
}
