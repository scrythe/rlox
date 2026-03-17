use core::panic;
use std::{marker::PhantomData, mem::take};

use crate::scanner_new::{Scanner, Token, TokenType};

pub enum LoxError {
    CompileError,
    RuntimeError,
}

impl LoxError {
    pub fn error_token(token: Token, message: &str, lexeme: &str) {
        if token.token_type == TokenType::Eof {
            LoxError::report(token.line, " at end", message);
        } else {
            LoxError::report(token.line, &format!(" at '{}'", lexeme), message);
        }
    }

    pub fn error_line(line: u32, message: &str) {
        LoxError::report(line, "", message);
    }

    fn report(line: u32, err_where: &str, message: &str) {
        println!("[line {line}] Error {err_where}: {message}");
    }

    pub fn runtime_error(line: u32, message: String) {
        println!("{message}\n[line {}]", line);
    }
}

macro_rules! lt_to_PhantomData {
    ($temp:tt) => {
        PhantomData
    };
}

macro_rules! phantom_lt_data {
    ($lt:lifetime) => {
        PhantomData<&$lt ()>
    };
    ($($lt:lifetime),*) => {
        ( $(PhantomData<&$lt ()>),* )
    };
}

macro_rules! define_ast {
    (
        $enum_class:ident<$($enum_lts:lifetime),+>;
        $(
            $class_method_name:ident,
            $class_types:ident $(<$($lt:lifetime),*>)?
                -> $(
                    $field_names:ident: $field_class_types:ty $(= $field_class_res_types:ty)?
                ),+;
        )+
    ) => {
        #[derive(Clone, Debug)]
        pub enum $enum_class<$($enum_lts),*> {
            $(
                $class_types( $class_types $(<$($lt),*>)?)
            ),+
        }
        impl<$($enum_lts),+> $enum_class<$($enum_lts),+> {
            $(
                pub fn $class_method_name ($($field_names: $field_class_types),+)
                    -> Self {
                        $enum_class::$class_types(
                            $class_types {
                                $($field_names),*,
                                $(
                                    _markers: ( $(lt_to_PhantomData!($lt)),* )
                                )?
                            }
                        )
                    }
            )+
        }
        $(
            #[derive(Clone, Debug)]
            pub struct $class_types $(<$($lt),*>)? {
                $(
                    pub $field_names: $field_class_types,
                )+
                 $(
                    _markers: phantom_lt_data!($($lt),*),
                )?
            }
        )+
    };
}

// pub enum Expr<'expr, 'strings> {
//     Assign(Assign<'expr, 'strings>),
// }
//
// impl<'expr, 'strings> Expr<'expr, 'strings> {
//     pub fn assign_expr(name: u32, value: u32) -> Expr<'expr, 'strings> {
//         Expr::Assign(Assign {
//             name,
//             value,
//             _markers: (PhantomData::<'expr_lt>, PhantomData),
//         })
//     }
// }
// pub struct Assign<'expr, 'strings> {
//     pub name: u32,
//     pub value: u32,
//     _markers: (PhantomData<&'expr ()>, PhantomData<&'strings ()>),
// }

define_ast!(
    Expr<'expr_lt, 'strings_lt>;
    assign_expr, Assign<'expr_lt, 'strings_lt> -> name: u32 = &'strings_lt str, value: u32 = Expr<'expr_lt>;
    binary_expr, Binary<'expr_lt> -> left: u32 = Expr<'expr_lt>, operator: TokenType, right: u32 = Expr<'expr_lt>;
    grouping_expr, Grouping<'expr_lt> -> expression: u32 = Expr<'expr_lt>;
    literal_expr, Literal<'strings_lt> -> value: Object<'strings_lt>;
    logiccal_expr, Logical<'expr_lt> -> left: u32 = Expr<'expr_lt> , operator: TokenType , right: u32 = Expr<'expr_lt>;
    unary_expr, Unary<'expr_lt> -> operator: TokenType , right: u32 = Expr<'expr_lt>;
    variable_expr, Variable<'expr_lt> -> name: u32 = &'strings_lt str;
);

define_ast!(
    Stmt<'stmt_lt, 'expr_lt, 'strings_lt>;
    block_stmt, Block<'stmt_lt, 'expr_lt, 'strings_lt> -> statements_start: u32, statements_end: u32, statements: PhantomData<Vec<Stmt<'stmt_lt, 'expr_lt, 'strings_lt>>>;
    expression_stmt, Expression<'expr_lt, 'strings_lt> -> expression: u32 = Expr<'expr_lt, 'strings_lt>;
    if_stmt, If<'stmt_lt, 'expr_lt, 'strings_lt> -> condition: u32 = Expr<'expr_lt, 'strings_lt>, then_branch: u32 = Stmt<'stmt_lt, 'expr_lt, 'strings_lt>, else_branch: Option<u32> = Stmt<'stmt_lt, 'expr_lt, 'strings_lt>;
    print_stmt, Pritn<'expr_lt, 'strings_lt> -> expression: u32 = Expr<'expr_lt, 'strings_lt>;
    var_stmt, Var<'expr_lt, 'strings_lt> -> name: u32 = &'strings_lt str, initializer: u32 = Expr<'expr_lt, 'strings_lt>;
    while_stmt, While<'stmt_lt, 'expr_lt, 'strings_lt> -> condition: u32 = Expr<'expr_lt, 'strings_lt>, body: u32 = Stmt<'stmt_lt, 'expr_lt, 'strings_lt>;
);

#[derive(PartialEq, Clone, Debug)]
pub enum Object<'strings_lt> {
    None,
    String(u32, PhantomData<&'strings_lt u32>),
    Number(f64),
    Bool(bool),
}

#[derive(Debug)]
struct LoxParseError();

pub struct Parser<'source, 'stmt_lt, 'expr_lt, 'string_lt> {
    tokens: Vec<Token>,
    current: usize,
    scanner: Scanner<'source>,
    pub statements: Vec<Stmt<'stmt_lt, 'expr_lt, 'string_lt>>,
    pub expressions: Vec<Expr<'expr_lt, 'string_lt>>,
    pub strings: Vec<String>,
}

impl<'source, 'stmt_lt, 'expr_lt, 'string_lt> Parser<'source, 'stmt_lt, 'expr_lt, 'string_lt> {
    pub fn new(
        tokens: Vec<Token>,
        scanner: Scanner<'source>,
    ) -> Parser<'source, 'stmt_lt, 'expr_lt, 'string_lt> {
        let current = 0;
        let expressions = Vec::new();
        let statements = Vec::new();
        let strings = Vec::new();
        Parser {
            tokens,
            current,
            scanner,
            statements,
            expressions,
            strings,
        }
    }

    pub fn parse(
        &mut self,
    ) -> (
        Vec<Stmt<'stmt_lt, 'expr_lt, 'string_lt>>,
        Vec<Stmt<'stmt_lt, 'expr_lt, 'string_lt>>,
        Vec<Expr<'expr_lt, 'string_lt>>,
        Vec<String>,
        bool,
    ) {
        // program -> statement* EOF
        let mut statements = Vec::new();

        let mut has_error = false;
        while !self.is_at_end() {
            let statement = self.declaration();
            match statement {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(_) => {
                    has_error = true;
                    self.synchonize()
                }
            }
        }
        let statements_arena = take(&mut self.statements);
        let expressions = take(&mut self.expressions);
        let strings = take(&mut self.strings);
        (
            statements,
            statements_arena,
            expressions,
            strings,
            has_error,
        )
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

    fn declaration(&mut self) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        if self.match_token(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        // varDecl -> "var" IDENTIFIER ( "=" expression )? ";"
        let name = self
            .consume(&TokenType::Identifier, "Expect variable name.")?
            .clone();

        let name = self.get_lexeme(name).to_string();
        let name_id = self.add_string(name);

        let initializer = if self.match_token(&[TokenType::Equal]) {
            self.expression()?
        } else {
            Expr::literal_expr(Object::None)
        };

        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        let initializer_id = self.add_expression(initializer);

        Ok(Stmt::var_stmt(name_id, initializer_id))
    }

    fn statement(&mut self) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        // statement -> exprStmt | forStmt | ifStmt | printStmt | whileStmt | block
        if self.match_token(&[TokenType::For]) {
            self.for_statement()
        } else if self.match_token(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_token(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_token(&[TokenType::While]) {
            self.while_stmt()
        } else if self.match_token(&[TokenType::LeftBrace]) {
            let (start, end) = self.block_statement()?;
            Ok(Stmt::block_stmt(start, end, PhantomData))
        } else {
            self.expression_statement()
        }
    }

    fn for_statement(&mut self) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        self.consume(&TokenType::LeftParen, "Exprect '(' after for.")?;
        let initializer = if self.match_token(&[TokenType::Semicolon]) {
            None
        } else if self.match_token(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            self.expression()?
        } else {
            Expr::literal_expr(Object::Bool(true))
        };
        self.consume(&TokenType::Semicolon, "Exprect ';' after loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TokenType::RightParen, "Exprect ')' after loop clause.")?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            let block_start = self.statements.len() as u32;
            self.statements.push(body);

            let increment_id = self.add_expression(increment);
            let expr_stmt = Stmt::expression_stmt(increment_id);

            let block_end = self.statements.len() as u32;
            self.statements.push(expr_stmt);
            body = Stmt::block_stmt(block_start, block_end, PhantomData)
        }

        let condition_id = self.add_expression(condition);
        let body_id = self.add_statements(body);
        body = Stmt::while_stmt(condition_id, body_id);

        if let Some(initializer) = initializer {
            let block_start = self.statements.len() as u32;
            self.statements.push(initializer);
            let block_end = self.statements.len() as u32;
            self.statements.push(body);

            body = Stmt::block_stmt(block_start, block_end, PhantomData)
        }
        Ok(body)
    }

    fn while_stmt(&mut self) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        // while -> "while" "(" expression ")" statement
        self.consume(&TokenType::LeftParen, "Exprect '(' after while.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Exprect ')' after while condition.")?;
        let condition_id = self.add_expression(condition);
        let body = self.statement()?;
        let body_id = self.add_statements(body);
        Ok(Stmt::while_stmt(condition_id, body_id))
    }

    fn if_statement(&mut self) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        // ifStmt -> "if" "(" expression ")" statement ( "else" statement )?
        self.consume(&TokenType::LeftParen, "Exprect '(' after if.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Exprect ')' after if condition.")?;
        let condition_id = self.add_expression(condition);

        let then_branch = self.statement()?;
        let then_branch_id = self.add_statements(then_branch);
        let opt_else_branch_id = if self.match_token(&[TokenType::Else]) {
            let else_branch_stmt = self.statement()?;
            let else_branch_stmt_id = self.add_statements(else_branch_stmt);
            Some(else_branch_stmt_id)
        } else {
            None
        };
        Ok(Stmt::if_stmt(
            condition_id,
            then_branch_id,
            opt_else_branch_id,
        ))
    }

    fn print_statement(&mut self) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        // statement -> "print" expression ";"
        // print already matched from fn statement
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon, "Exprect ';' after value.")?;
        let value_id = self.add_expression(value);
        Ok(Stmt::print_stmt(value_id))
    }

    fn block_statement(&mut self) -> Result<(u32, u32), LoxParseError> {
        // block -> "{" declaration "}"
        let statements_start = self.statements.len() as u32;
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            let statement = self.declaration()?;
            self.statements.push(statement);
        }
        let statements_end = self.statements.len() as u32 - 1;
        self.consume(&TokenType::RightBrace, "Expect '}' after block")?;
        Ok((statements_start, statements_end))
    }

    fn expression_statement(
        &mut self,
    ) -> Result<Stmt<'stmt_lt, 'expr_lt, 'string_lt>, LoxParseError> {
        // statement -> expression ";"
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after expression.")?;
        let expr_id = self.add_expression(expr);
        Ok(Stmt::expression_stmt(expr_id))
    }

    fn expression(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // expression -> assignment
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // assigntment -> IDENTIFIER "=" assignment
        //              | logir_or
        let expr = self.or()?;
        if self.match_token(&[TokenType::Equal]) {
            let equals_token = self.previous().clone();
            let value = self.assignment()?;
            let id = self.add_expression(value);

            if let Expr::Variable(var) = expr {
                return Ok(Expr::assign_expr(var.name, id));
            }

            self.error(equals_token, "Invalid assignment target.");
        }
        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // logic_or -> logic_and ( "or" logic_and )*
        let mut expr = self.and()?;
        while self.match_token(&[TokenType::Or]) {
            let operator = self.previous().token_type.clone();
            let right = self.and()?;
            let expr_id = self.add_expression(expr);
            let right_id = self.add_expression(right);
            expr = Expr::logiccal_expr(expr_id, operator, right_id);
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // logic_and -> equality ( "or" equality )*
        let mut expr = self.equality()?;
        while self.match_token(&[TokenType::And]) {
            let operator = self.previous().token_type.clone();
            let right = self.equality()?;
            let expr_id = self.add_expression(expr);
            let right_id = self.add_expression(right);
            expr = Expr::logiccal_expr(expr_id, operator, right_id);
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // equality -> comparison ( ( "!=" | "==" ) comparison )*
        let mut expr = self.comparison()?;
        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().token_type.clone();
            let right = self.comparison()?;
            let expr_id = self.add_expression(expr);
            let right_id = self.add_expression(right);
            expr = Expr::binary_expr(expr_id, operator, right_id);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // comparison -> term ( ( ">" | ">=" | "<" | "<=") term )*
        let mut expr = self.term()?;
        while self.match_token(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
            let operator = self.previous().token_type.clone();
            let right = self.term()?;
            let expr_id = self.add_expression(expr);
            let right_id = self.add_expression(right);
            expr = Expr::binary_expr(expr_id, operator, right_id);
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // term -> factor ( ( "-" | "+" ) factor )*
        let mut expr = self.factor()?;
        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().token_type.clone();
            let right = self.factor()?;
            let expr_id = self.add_expression(expr);
            let right_id = self.add_expression(right);
            expr = Expr::binary_expr(expr_id, operator, right_id);
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // factor -> unary ( ( "/" | "*" ) unary )*
        let mut expr = self.unary()?;
        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().token_type.clone();
            let right = self.unary()?;
            let expr_id = self.add_expression(expr);
            let right_id = self.add_expression(right);
            expr = Expr::binary_expr(expr_id, operator, right_id);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // my attempt: unary -> ( "!" | "-" )* primary
        // unary -> ( "!" | "-" ) unary
        //       | primary
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().token_type.clone();
            let right = self.unary()?;
            let right_id = self.add_expression(right);
            Ok(Expr::unary_expr(operator, right_id))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr<'expr_lt, 'string_lt>, LoxParseError> {
        // primary ->  Number | String | "true" | "false "| "nil" | "(" expression ")" | IDENTIFIER
        if self.match_token(&[TokenType::False]) {
            Ok(Expr::literal_expr(Object::Bool(false)))
        } else if self.match_token(&[TokenType::True]) {
            Ok(Expr::literal_expr(Object::Bool(true)))
        } else if self.match_token(&[TokenType::Nil]) {
            Ok(Expr::literal_expr(Object::None))
        } else if self.match_token(&[TokenType::String, TokenType::Number]) {
            let token = self.previous().clone();
            let literal = self.get_lexeme(token.clone()).to_string();
            if token.token_type == TokenType::String {
                let id = self.add_string(literal);
                Ok(Expr::literal_expr(Object::String(id, PhantomData)))
            } else if token.token_type == TokenType::Number {
                let number: f64 = literal.parse().unwrap();
                Ok(Expr::literal_expr(Object::Number(number)))
            } else {
                panic!("should not happen");
            }
        } else if self.match_token(&[TokenType::Identifier]) {
            let token = self.previous().clone();
            let literal = self.get_lexeme(token).to_string();
            self.strings.push(literal);
            Ok(Expr::variable_expr(self.strings.len() as u32 - 1))
        } else if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            let id = self.add_expression(expr);
            Ok(Expr::grouping_expr(id))
        } else {
            let token = self.peek().clone();
            Err(self.error(token, "Expect expression"))
        }
    }

    fn add_string(&mut self, string: String) -> u32 {
        let id = self.strings.len() as u32;
        self.strings.push(string);
        id
    }

    fn add_expression(&mut self, expr: Expr<'expr_lt, 'string_lt>) -> u32 {
        let id = self.expressions.len() as u32;
        self.expressions.push(expr);
        id
    }

    fn add_statements(&mut self, stmt: Stmt<'stmt_lt, 'expr_lt, 'string_lt>) -> u32 {
        let id = self.statements.len() as u32;
        self.statements.push(stmt);
        id
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<&Token, LoxParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let token = self.peek().clone();
            Err(self.error(token.clone(), message))
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

    fn advance(&mut self) -> &Token {
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

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn error(&mut self, token: Token, message: &str) -> LoxParseError {
        let lexeme = self.get_lexeme(token.clone());
        LoxError::error_token(token, message, lexeme);
        LoxParseError()
    }

    fn get_lexeme(&mut self, token: Token) -> &str {
        self.scanner.start = token.start as usize;
        self.scanner.current = token.start as usize;
        let _ = self.scanner.scan_token();
        let lexeme = self.scanner.get_lexeme();
        str::from_utf8(lexeme).unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn sizes() {
        dbg!(size_of::<Stmt>());
        dbg!(size_of::<Block>());
        dbg!(size_of::<Expression>());
        dbg!(size_of::<If>());
        dbg!(size_of::<Pritn>());
        dbg!(size_of::<Var>());
        dbg!(size_of::<While>());
    }
}
