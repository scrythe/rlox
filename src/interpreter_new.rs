use std::marker::PhantomData;

use crate::{
    environment_new::{self as environment, Environment},
    parser_new::{Expr, Object, Stmt},
    scanner_new::TokenType,
};

pub struct RuntimeError {
    pub line: u32,
    pub message: String,
}

impl RuntimeError {
    pub fn new(line: u32, message: String) -> RuntimeError {
        RuntimeError { line, message }
    }
}

enum StmtOrId<'stmt_lt, 'expr_lt, 'string_lt> {
    Stmt(&'stmt_lt Stmt<'stmt_lt, 'expr_lt, 'string_lt>),
    Id(u32),
}

pub struct Interpreter<'stmt_lt, 'expr_lt, 'string_lt> {
    environment: environment::Environment<'string_lt>,
    statements: Vec<Stmt<'stmt_lt, 'expr_lt, 'string_lt>>,
    expressions: Vec<Expr<'expr_lt, 'string_lt>>,
    strings: Vec<String>,
}

impl<'stmt_lt, 'expr_lt, 'string_lt> Interpreter<'stmt_lt, 'expr_lt, 'string_lt> {
    pub fn new(
        statements: Vec<Stmt<'stmt_lt, 'expr_lt, 'string_lt>>,
        expressions: Vec<Expr<'expr_lt, 'string_lt>>,
        strings: Vec<String>,
    ) -> Interpreter<'stmt_lt, 'expr_lt, 'string_lt> {
        let environment = Environment::new();
        Interpreter {
            environment,
            statements,
            expressions,
            strings,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute(StmtOrId::Stmt(&statement))?
        }
        Ok(())
    }

    fn execute(&mut self, stmt_or_id: StmtOrId) -> Result<(), RuntimeError> {
        // self.environment = Environment::new_enclosing(self.environment);
        let stmt = match stmt_or_id {
            StmtOrId::Stmt(stmt) => stmt,
            StmtOrId::Id(id) => self.get_statement(id),
        };

        match stmt {
            Stmt::Var(stmt) => {
                let name = stmt.name;
                let value = self.evaluate(stmt.initializer)?;
                let name = &self.strings[name as usize];
                self.environment.define(name, value);
            }
            Stmt::Pritn(stmt) => {
                let value = self.evaluate(stmt.expression)?;
                println!("{}", self.stringify(value));
            }
            Stmt::Expression(stmt) => {
                self.evaluate(stmt.expression)?;
            }
            Stmt::Block(stmt) => {
                let statements_start = stmt.statements_start;
                let statements_end = stmt.statements_end;
                self.environment.swap_new_scoped_env();
                for statement_id in statements_start..=statements_end {
                    if let Err(err) = self.execute(StmtOrId::Id(statement_id)) {
                        self.environment = self.environment.get_upper_env();
                        return Err(err);
                    }
                }
                self.environment = self.environment.get_upper_env();
            }
            Stmt::If(stmt) => {
                let then_branch_id = stmt.then_branch;
                let else_branch_id = stmt.else_branch;
                let value = self.evaluate(stmt.condition)?;
                if Interpreter::is_truthy(&value) {
                    self.execute(StmtOrId::Id(then_branch_id))?;
                } else if let Some(stmt_id) = else_branch_id {
                    self.execute(StmtOrId::Id(stmt_id))?;
                };
            }
            Stmt::While(stmt) => {
                let condition_id = stmt.condition;
                let body_id = stmt.body;
                while Interpreter::is_truthy(&self.evaluate(condition_id)?) {
                    // dbg!(self.get_expression(condition_id));
                    self.execute(StmtOrId::Id(body_id))?;
                }
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expression_id: u32) -> Result<Object<'string_lt>, RuntimeError> {
        match self.get_expression(expression_id) {
            Expr::Variable(var) => {
                let name = self.get_string(var.name);
                let value = self.environment.get(name)?;
                Ok(value.clone())
            }
            Expr::Literal(value) => Ok(value.value.clone()),
            Expr::Grouping(val) => self.evaluate(val.expression),
            Expr::Unary(val) => {
                let literal = match val.operator {
                    TokenType::Minus => {
                        let right = self.evaluate(val.right)?;
                        let val = Interpreter::convert_number_operator(right)?;
                        Object::Number(-val)
                    }
                    TokenType::Bang => {
                        let right = self.evaluate(val.right)?;
                        Object::Bool(Interpreter::is_truthy(&right))
                    }
                    // Unreachable.
                    _ => Object::None,
                };
                Ok(literal)
            }
            Expr::Binary(val) => {
                let token_line = 0; // TODO:
                let literal = match val.operator {
                    TokenType::Minus => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        let (left, right) =
                            Interpreter::convert_number_operators(token_line, left, right)?;
                        Object::Number(left - right)
                    }
                    TokenType::Slash => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        let (left, right) =
                            Interpreter::convert_number_operators(token_line, left, right)?;
                        Object::Number(left / right)
                    }
                    TokenType::Star => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        let (left, right) =
                            Interpreter::convert_number_operators(token_line, left, right)?;
                        Object::Number(left * right)
                    }
                    TokenType::Plus => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        match (left, right) {
                            (Object::Number(left), Object::Number(right)) => {
                                Object::Number(left + right)
                            }
                            (Object::String(left, _), Object::String(right, _)) => {
                                // let left = self.strings[left as usize];
                                // let right = self.strings[right as usize];
                                // TODO: push strings
                                Object::String(left, PhantomData)
                            }
                            _ => Err(RuntimeError::new(
                                token_line,
                                "Operands must be numbers.".to_string(),
                            ))?,
                        }
                    }

                    TokenType::Greater => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        let (left, right) =
                            Interpreter::convert_number_operators(token_line, left, right)?;
                        Object::Bool(left > right)
                    }

                    TokenType::GreaterEqual => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        let (left, right) =
                            Interpreter::convert_number_operators(token_line, left, right)?;
                        Object::Bool(left >= right)
                    }
                    TokenType::Less => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        let (left, right) =
                            Interpreter::convert_number_operators(token_line, left, right)?;
                        Object::Bool(left < right)
                    }
                    TokenType::LessEqual => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        let (left, right) =
                            Interpreter::convert_number_operators(token_line, left, right)?;
                        Object::Bool(left <= right)
                    }

                    TokenType::BangEqual => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        Object::Bool(!Interpreter::is_equal(left, right))
                    }
                    TokenType::EqualEqual => {
                        let right = val.right;
                        let left = self.evaluate(val.left)?;
                        let right = self.evaluate(right)?;
                        Object::Bool(Interpreter::is_equal(left, right))
                    }
                    // Unreachable.
                    _ => Object::None,
                };
                Ok(literal)
            }
            Expr::Logical(val) => {
                let lexeme = ""; // TODO:
                let right = val.right;
                match val.operator {
                    TokenType::Or => {
                        let left = self.evaluate(val.left)?;
                        if Interpreter::is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                    TokenType::And => {
                        let left = self.evaluate(val.left)?;
                        if !Interpreter::is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                    _ => {
                        panic!("Error, expected Logical Operator but found: {}", lexeme)
                    }
                }
                self.evaluate(right)
            }
            Expr::Assign(val) => {
                let name = val.name;
                let value = self.evaluate(val.value)?;
                let name = &self.strings[name as usize];
                self.environment.assign(name, value.clone())?;
                Ok(value)
            }
        }
    }

    fn stringify(&self, evaluated: Object) -> String {
        match evaluated {
            Object::None => "nil".to_string(),
            Object::Number(number) => number.to_string(),
            Object::Bool(bool) => bool.to_string(),
            Object::String(text_id, _) => self.strings[text_id as usize].clone(),
        }
    }

    fn is_truthy(val: &Object) -> bool {
        match val {
            Object::Bool(bool_val) => *bool_val,
            Object::None => false,
            _ => true,
        }
    }

    fn convert_number_operator(operand: Object) -> Result<f64, RuntimeError> {
        let line = 0; // TODO:
        match operand {
            Object::Number(operand) => Ok(operand),
            _ => Err(RuntimeError::new(
                line,
                "Operand must be a number.".to_string(),
            )),
        }
    }

    fn convert_number_operators(
        line: u32,
        left: Object,
        right: Object,
    ) -> Result<(f64, f64), RuntimeError> {
        match (left, right) {
            (Object::Number(left), Object::Number(right)) => Ok((left, right)),
            _ => Err(RuntimeError::new(
                line,
                "Operands must be numbers.".to_string(),
            )),
        }
    }
    fn is_equal(a: Object, b: Object) -> bool {
        if a == Object::None && b == Object::None {
            true
        } else if a == Object::None {
            false
        } else {
            a == b
        }
    }

    #[inline]
    fn get_string(&self, string_id: u32) -> &str {
        &self.strings[string_id as usize]
    }

    #[inline]
    fn get_statement(&self, statement_id: u32) -> &Stmt<'stmt_lt, 'expr_lt, 'string_lt> {
        &self.statements[statement_id as usize]
    }

    #[inline]
    fn get_expression(&self, expression_id: u32) -> &Expr<'expr_lt, 'string_lt> {
        &self.expressions[expression_id as usize]
    }

    // #[inline]
    // fn eval_expr(&mut self, expression_id: u32) -> &Object<'string_lt> {
    //     self.evaluate(&self.expressions[expression_id as usize])
    //         .unwrap_unchecked()
    // }

    // #[inline]
    // fn get_expression_unsafe(&self, expression_id: u32) -> &Expr<'expr_lt, 'string_lt> {
    //     unsafe { self.expressions.as_ptr() + expression_id }
    //     // & self.expressions[expression_id as usize]
    // }
}

#[cfg(test)]
mod test {
    use crate::parser_new;

    #[test]
    fn size() {
        // type a = (u32, PhantomData<u32>);
        dbg!(size_of::<parser_new::Expr>());
        dbg!(size_of::<parser_new::Object>());
        dbg!(size_of::<f64>());

        dbg!(size_of::<parser_new::Assign>());
        dbg!(size_of::<parser_new::Binary>());
        dbg!(size_of::<parser_new::Grouping>());
        dbg!(size_of::<parser_new::Literal>());
        dbg!(size_of::<parser_new::Logical>());
        dbg!(size_of::<parser_new::Unary>());
        dbg!(size_of::<parser_new::Variable>());
    }
}
