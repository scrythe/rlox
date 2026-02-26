pub struct AstPrinter {}

macro_rules! define_ast {
     ($expr_class:ident<$expr_lt:lifetime>;
     $($class_method_name:ident, $class_types:ident $(<$lt:lifetime>)? -> $($field_names:ident: $field_class_types:ty),+;)+) => {
        #[derive(Debug, PartialEq)]
        pub enum $expr_class<$expr_lt> {
            $($class_types(Box<$class_types $(<$lt>)? >)),+
        }
        impl<$expr_lt> $expr_class<$expr_lt> {
        $(
         fn $class_method_name ($($field_names: $field_class_types),+) -> $expr_class<$expr_lt> { $expr_class::$class_types(Box::new($class_types {$($field_names),*}))  }
        )+
        }
        $(
            #[derive(Debug, PartialEq)]
            pub struct $class_types $(<$lt>)? {
                $($field_names: $field_class_types,)+
            }
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

    pub fn parse(mut self) -> Expr<'a> {
        match self.expression() {
            Ok(expression) => expression,
            Err(_) => Expr::literal_expr(LiteralValue::None),
        }
    }

    fn expression<'b>(&mut self) -> Result<Expr<'a>, &'b str> {
        self.equality()
    }

    fn equality<'b>(&mut self) -> Result<Expr<'a>, &'b str> {
        // equality -> comparison ( ( "!=" | "==" ) comparison )*
        let mut expr = self.comparison()?;
        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn comparison<'b>(&mut self) -> Result<Expr<'a>, &'b str> {
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

    fn term<'b>(&mut self) -> Result<Expr<'a>, &'b str> {
        // term -> factor ( ( "-" | "+" ) factor )*
        let mut expr = self.factor()?;
        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn factor<'b>(&mut self) -> Result<Expr<'a>, &'b str> {
        // factor -> unary ( ( "/" | "*" ) unary )*
        let mut expr = self.unary()?;
        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary<'b>(&mut self) -> Result<Expr<'a>, &'b str> {
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

    fn primary<'b>(&mut self) -> Result<Expr<'a>, &'b str> {
        // primary ->  Number | String | "true" | "false "| "nil" | "(" expression ")"
        if self.match_token(&[TokenType::False]) {
            Ok(Expr::literal_expr(LiteralValue::False))
        } else if self.match_token(&[TokenType::True]) {
            Ok(Expr::literal_expr(LiteralValue::True))
        } else if self.match_token(&[TokenType::Nil]) {
            Ok(Expr::literal_expr(LiteralValue::None))
        } else if self.match_token(&[TokenType::String, TokenType::Number]) {
            Ok(Expr::literal_expr(self.previous().literal.clone()))
        } else if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.");
            Ok(Expr::grouping_expr(expr))
        } else {
            Err("wrong")
        }
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
        let tokens = vec![
            Token::new(TokenType::String, "\"hm\"", LiteralValue::String("hm"), 1),
            Token::new(TokenType::BangEqual, "!=", LiteralValue::None, 1),
            Token::new(TokenType::Number, "5", LiteralValue::Number(5.0), 1),
            Token::new(TokenType::Eof, "", LiteralValue::None, 2),
        ];
        let parser = Parser::new(tokens);
        let expression = parser.parse();
        assert_eq!(
            expression,
            Expr::binary_expr(
                Expr::literal_expr(LiteralValue::String("hm")),
                Token::new(TokenType::BangEqual, "!=", LiteralValue::None, 1),
                Expr::literal_expr(LiteralValue::Number(5.0))
            )
        );
    }

    #[test]
    fn test_ast_printer() {
        let token = Token::new(TokenType::Plus, "+", LiteralValue::None, 1);
        let expr = Expr::binary_expr(
            Expr::literal_expr(LiteralValue::Number(5.0)),
            token,
            Expr::literal_expr(LiteralValue::Number(4.3)),
        );
        let out = AstPrinter::print(expr);
        assert_eq!(out, "(+ 5 4.3)")
    }
}
