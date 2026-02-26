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
     $($class_types:ident $(<$lt:lifetime>)? -> $($field_names:ident: $field_class_types:ty),+;)+) => {
        #[derive(Debug)]
        enum $expr_class<$expr_lt> {
            $($class_types(Box<$class_types $(<$lt>)? >)),+
        }
        $(
            define_single_ast!($class_types $(<$lt>)? -> $($field_names: $field_class_types),+;);
        )+
    };
}

use crate::scanner::TokenType;

use super::scanner::Literal;
use super::scanner::Token;

define_ast!(
    Expr<'a>;
    Binary<'a> -> left:Expr<'a> , operator:Token<'a> , right:Expr<'a> ;
    Grouping<'a> -> expression:Expr<'a> ;
    LiteralExpr<'a> -> value: Literal<'a> ;
    Unary<'a> -> operator:Token<'a> , right:Expr<'a> ;
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
                    "{} {} {}",
                    binary_expr.operator.lexeme,
                    AstPrinter::print(binary_expr.left),
                    AstPrinter::print(binary_expr.right)
                )
            }
            Expr::Grouping(group_expr) => {
                format!("group {}", AstPrinter::print(group_expr.expression))
            }
            Expr::LiteralExpr(literal_expr) => match literal_expr.value {
                Literal::None => String::from(""),
                Literal::String(text) => text.to_string(),
                Literal::Number(val) => val.to_string(),
                Literal::False => false.to_string(),
                Literal::True => true.to_string(),
            },
            Expr::Unary(unary_expr) => format!(
                "{} {}",
                unary_expr.operator.lexeme,
                AstPrinter::print(unary_expr.right),
            ),
        }
    }
}

struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Parser<'a> {
        let current = 0;
        Parser { tokens, current }
    }

    fn expression(&self) -> Expr<'a> {
        let expr = Parser::comparison();
        expr
    }

    fn comparison() -> Expr<'a> {
        Expr::LiteralExpr(Box::new(LiteralExpr::new(Literal::None)))
    }

    fn primary(&mut self) -> Expr<'a> {
        if self.match_token(&[TokenType::False]) {
            return Expr::LiteralExpr(LiteralExpr::boxed_new(Literal::False));
        }
        if self.match_token(&[TokenType::True]) {
            return Expr::LiteralExpr(LiteralExpr::boxed_new(Literal::True));
        }
        if self.match_token(&[TokenType::Nil]) {
            return Expr::LiteralExpr(LiteralExpr::boxed_new(Literal::None));
        }

        if self.match_token(&[TokenType::String, TokenType::Number]) {
            let previous = self.previous();
            if previous.token_type == TokenType::String {
                return Expr::LiteralExpr(LiteralExpr::boxed_new(Literal::String(previous.lexeme)));
            } else {
                return Expr::LiteralExpr(LiteralExpr::boxed_new(Literal::None));
            }
        }
        if self.match_token(&[TokenType::Number]) {
            return Expr::LiteralExpr(LiteralExpr::boxed_new(Literal::None));
        }
        Expr::LiteralExpr(LiteralExpr::boxed_new(Literal::None))
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

    fn advance(&mut self) -> &Token<'_> {
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
        let tokens = vec![
            Token::new(TokenType::String, "hm", Literal::String("hm"), 1),
            Token::new(TokenType::Number, "5", Literal::Number(5.0), 1),
        ];
        let mut parser = Parser::new(tokens);
        dbg!(parser.primary());
    }

    #[test]
    fn test_ast() {
        let token = Token::new(TokenType::Plus, "+", Literal::None, 1);
        let expr = Binary::new(
            Expr::LiteralExpr(Box::new(LiteralExpr::new(Literal::Number(5.0)))),
            token,
            Expr::LiteralExpr(Box::new(LiteralExpr::new(Literal::Number(4.3)))),
        );
        let expr = Expr::Binary(Box::new(expr));
        let out = AstPrinter::print(expr);
        assert_eq!(out, "+ 5 4.3")
    }
}
