use crate::LoxError;
use crate::scanner::{LiteralValue, Token, TokenType};

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

define_ast!(
    Expr<'source>;
    assign_expr, Assign<'source> -> name: Token<'source> , value: Expr<'source>;
    binary_expr, Binary<'source> -> left: Expr<'source> , operator: Token<'source> , right: Expr<'source>;
    grouping_expr, Grouping<'source> -> expression: Expr<'source>;
    literal_expr, Literal -> value: LiteralValue;
    unary_expr, Unary<'source> -> operator: Token<'source> , right: Expr<'source>;
    variable_expr, Variable<'source> -> name: Token<'source>;
);
define_ast!(
    Stmt<'source>;
    block_stmt, Block<'source> -> statements: Vec<Stmt<'source>>;
    expression_stmt, Expression<'source> -> expression: Expr<'source>;
    print_stmt, Pritn<'source> -> expression: Expr<'source>;
    var_stmt, Var<'source> -> name: Token<'source>, initializer: Expr<'source>;
);

#[derive(Debug)]
struct LoxParseError();

pub struct Parser<'tokens> {
    tokens: Vec<Token<'tokens>>,
    current: usize,
}

impl<'tokens> Parser<'tokens> {
    pub fn new(tokens: Vec<Token<'tokens>>) -> Parser<'tokens> {
        let current = 0;
        Parser { tokens, current }
    }

    pub fn parse(mut self) -> (Vec<Stmt<'tokens>>, bool) {
        // program -> statement* EOF
        let mut statements: Vec<Stmt> = Vec::new();
        let mut has_error = false;
        while !self.is_at_end() {
            let statement = self.declaration();
            match statement {
                Ok(statement) => statements.push(statement),
                Err(_) => {
                    has_error = true;
                    self.synchonize()
                }
            }
        }
        (statements, has_error)
    }

    fn synchonize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn declaration(&mut self) -> Result<Stmt<'tokens>, LoxParseError> {
        if self.match_token(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt<'tokens>, LoxParseError> {
        // varDecl -> "var" IDENTIFIER ( "=" expression )? ";"
        let name = self
            .consume(&TokenType::Identifier, "Expect variable name.")?
            .clone();

        let initializer = if self.match_token(&[TokenType::Equal]) {
            self.expression()?
        } else {
            Expr::literal_expr(LiteralValue::None)
        };

        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;
        Ok(Stmt::var_stmt(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt<'tokens>, LoxParseError> {
        // statement -> exprStmt | printStmt | block
        if self.match_token(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_token(&[TokenType::LeftBrace]) {
            Ok(Stmt::block_stmt(self.block_statement()?))
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt<'tokens>, LoxParseError> {
        // statement -> "print" expression ";"
        // print already matched from fn statement
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon, "Exprect ';' after value.")?;
        Ok(Stmt::print_stmt(value))
    }

    fn block_statement(&mut self) -> Result<Vec<Stmt<'tokens>>, LoxParseError> {
        // block -> "{" declaration "}"
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            let statement = self.declaration()?;
            statements.push(statement);
        }
        self.consume(&TokenType::RightBrace, "Expect '}' after block")?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Stmt<'tokens>, LoxParseError> {
        // statement -> expression ";"
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::expression_stmt(expr))
    }

    fn expression(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
        // expression -> assignment
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
        // assigntment -> IDENTIFIER "=" assignment
        //              | equality
        let expr = self.equality()?;
        if self.match_token(&[TokenType::Equal]) {
            let equals_token = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable(var) = expr {
                return Ok(Expr::assign_expr(var.name, value));
            }

            self.error(&equals_token, "Invalid assignment target.");
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
        // equality -> comparison ( ( "!=" | "==" ) comparison )*
        let mut expr = self.comparison()?;
        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
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

    fn term(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
        // term -> factor ( ( "-" | "+" ) factor )*
        let mut expr = self.factor()?;
        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
        // factor -> unary ( ( "/" | "*" ) unary )*
        let mut expr = self.unary()?;
        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::binary_expr(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
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

    fn primary(&mut self) -> Result<Expr<'tokens>, LoxParseError> {
        // primary ->  Number | String | "true" | "false "| "nil" | "(" expression ")" | IDENTIFIER
        if self.match_token(&[TokenType::False]) {
            Ok(Expr::literal_expr(LiteralValue::Bool(false)))
        } else if self.match_token(&[TokenType::True]) {
            Ok(Expr::literal_expr(LiteralValue::Bool(true)))
        } else if self.match_token(&[TokenType::Nil]) {
            Ok(Expr::literal_expr(LiteralValue::None))
        } else if self.match_token(&[TokenType::String, TokenType::Number]) {
            Ok(Expr::literal_expr(self.previous().literal.clone()))
        } else if self.match_token(&[TokenType::Identifier]) {
            Ok(Expr::variable_expr(self.previous().clone()))
        } else if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            Ok(Expr::grouping_expr(expr))
        } else {
            let token = self.peek().clone();
            Err(self.error(&token, "Expect expression"))
        }
    }

    fn consume(
        &mut self,
        token_type: &TokenType,
        message: &str,
    ) -> Result<&Token<'tokens>, LoxParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let token = self.peek().clone();
            Err(self.error(&token, message))
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

    fn advance(&mut self) -> &Token<'tokens> {
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

    fn peek(&self) -> &Token<'tokens> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token<'tokens> {
        &self.tokens[self.current - 1]
    }

    fn error(&mut self, token: &Token, message: &str) -> LoxParseError {
        LoxError::error_token(token, message);
        LoxParseError()
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
            Token::new(TokenType::Semicolon, ";", LiteralValue::None, 2),
            Token::new(TokenType::Eof, "", LiteralValue::None, 2),
        ];
        let parser = Parser::new(tokens);
        let (expression, has_parse_error) = parser.parse();
        if has_parse_error {
            panic!("Unexpected parse error.");
        }
        assert_eq!(
            expression,
            vec![Stmt::expression_stmt(Expr::binary_expr(
                Expr::literal_expr(LiteralValue::String("hm".to_string())),
                Token::new(TokenType::BangEqual, "!=", LiteralValue::None, 1),
                Expr::literal_expr(LiteralValue::Number(5.0))
            ))]
        );
    }
}
