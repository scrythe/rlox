use std::marker::PhantomData;

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

pub enum Object<'strings_lt> {
    None,
    String(u32, PhantomData<&'strings_lt u32>),
    Number(f64),
    Bool(bool),
}

#[derive(Debug)]
struct LoxParseError();

pub struct Parser<'source, 'expr_lt, 'string_lt> {
    tokens: Vec<Token>,
    current: usize,
    scanner: Scanner<'source>,
    expressions: Vec<Expr<'expr_lt, 'string_lt>>,
    strings: Vec<String>,
}

impl<'source, 'expr_lt, 'string_lt> Parser<'source, 'expr_lt, 'string_lt> {
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
            let literal = self.get_lexeme(token).to_string();
            let id = self.add_string(literal);
            Ok(Expr::literal_expr(Object::String(id, PhantomData)))
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
