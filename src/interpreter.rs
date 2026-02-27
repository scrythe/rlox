use crate::{
    environment::{self, Environment},
    parser::{Expr, Stmt},
    scanner::{LiteralValue, Token, TokenType},
};

pub struct RuntimeError<'token> {
    pub token: Token<'token>,
    pub message: String,
}

impl<'token> RuntimeError<'token> {
    pub fn new(token: Token<'token>, message: String) -> RuntimeError<'token> {
        RuntimeError { token, message }
    }
}

pub struct Interpreter {
    environment: environment::Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let environment = Environment::new();
        Interpreter { environment }
    }

    pub fn interpret<'statements>(
        &mut self,
        statements: Vec<Stmt<'statements>>,
    ) -> Result<(), RuntimeError<'statements>> {
        for statement in statements {
            self.execute(statement)?
        }
        Ok(())
    }

    fn execute<'statements>(
        &mut self,
        stmt: Stmt<'statements>,
    ) -> Result<(), RuntimeError<'statements>> {
        match stmt {
            Stmt::Var(expr) => {
                let value = self.evaluate(expr.initializer)?;
                self.environment.define(expr.name.lexeme, value);
            }
            Stmt::Pritn(expr) => {
                let value = self.evaluate(expr.expression)?;
                println!("{}", Interpreter::stringify(value));
            }
            Stmt::Expression(expr) => {
                self.evaluate(expr.expression)?;
            }
        }
        Ok(())
    }

    fn stringify(evaluated: LiteralValue) -> String {
        match evaluated {
            LiteralValue::None => "nil".to_string(),
            LiteralValue::Number(number) => number.to_string(),
            LiteralValue::Bool(bool) => bool.to_string(),
            LiteralValue::String(text) => text,
        }
    }
    fn evaluate<'token>(
        &mut self,
        expr: Expr<'token>,
    ) -> Result<LiteralValue, RuntimeError<'token>> {
        match expr {
            Expr::Variable(name) => Ok(self.environment.get(name.name)?.clone()),
            Expr::Literal(val) => Ok(val.value),
            Expr::Grouping(val) => self.evaluate(val.expression),
            Expr::Unary(val) => {
                let right = self.evaluate(val.right)?;
                let literal = match val.operator.token_type {
                    TokenType::Minus => {
                        let val = Interpreter::convert_number_operator(val.operator, right)?;
                        LiteralValue::Number(-val)
                    }
                    TokenType::Bang => match right {
                        LiteralValue::Bool(_) => right,
                        _ => LiteralValue::Bool(false),
                    },
                    // Unreachable.
                    _ => LiteralValue::None,
                };
                Ok(literal)
            }
            Expr::Binary(val) => {
                let left = self.evaluate(val.left)?;
                let right = self.evaluate(val.right)?;

                let literal = match val.operator.token_type {
                    TokenType::Minus => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator, left, right)?;
                        LiteralValue::Number(left - right)
                    }
                    TokenType::Slash => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator, left, right)?;
                        LiteralValue::Number(left / right)
                    }
                    TokenType::Star => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator, left, right)?;
                        LiteralValue::Number(left * right)
                    }
                    TokenType::Plus => match (left, right) {
                        (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                            LiteralValue::Number(left + right)
                        }
                        (LiteralValue::String(left), LiteralValue::String(right)) => {
                            LiteralValue::String(left + &right)
                        }
                        _ => Err(RuntimeError::new(
                            val.operator,
                            "Operands must be numbers.".to_string(),
                        ))?,
                    },

                    TokenType::Greater => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator, left, right)?;
                        LiteralValue::Bool(left > right)
                    }

                    TokenType::GreaterEqual => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator, left, right)?;
                        LiteralValue::Bool(left >= right)
                    }
                    TokenType::Less => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator, left, right)?;
                        LiteralValue::Bool(left < right)
                    }
                    TokenType::LessEqual => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator, left, right)?;
                        LiteralValue::Bool(left <= right)
                    }

                    TokenType::BangEqual => LiteralValue::Bool(!Interpreter::is_equal(left, right)),
                    TokenType::EqualEqual => LiteralValue::Bool(Interpreter::is_equal(left, right)),
                    // Unreachable.
                    _ => LiteralValue::None,
                };
                Ok(literal)
            }
        }
    }
    fn convert_number_operator<'token>(
        operator: Token<'token>,
        operand: LiteralValue,
    ) -> Result<f64, RuntimeError<'token>> {
        match operand {
            LiteralValue::Number(operand) => Ok(operand),
            _ => Err(RuntimeError::new(
                operator,
                "Operand must be a number.".to_string(),
            )),
        }
    }
    fn convert_number_operators<'token>(
        operator: Token<'token>,
        left: LiteralValue,
        right: LiteralValue,
    ) -> Result<(f64, f64), RuntimeError<'token>> {
        match (left, right) {
            (LiteralValue::Number(left), LiteralValue::Number(right)) => Ok((left, right)),
            _ => Err(RuntimeError::new(
                operator,
                "Operands must be numbers.".to_string(),
            )),
        }
    }
    fn is_equal(a: LiteralValue, b: LiteralValue) -> bool {
        if a == LiteralValue::None && b == LiteralValue::None {
            true
        } else if a == LiteralValue::None {
            false
        } else {
            a == b
        }
    }
}
