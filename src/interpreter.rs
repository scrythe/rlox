use crate::{
    environment::{self, Environment},
    functions,
    parser::{Expr, Stmt},
    scanner::{LiteralValue, TokenType},
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

pub struct Interpreter {
    environment: environment::Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let environment = Environment::new();
        Interpreter { environment }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RuntimeError> {
        // let a = functions::LoxFunction {};
        for statement in statements {
            self.execute(&statement)?
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        // self.environment = Environment::new_enclosing(self.environment);
        match stmt {
            Stmt::Var(expr) => {
                let value = self.evaluate(&expr.initializer)?;
                self.environment.define(expr.name.lexeme, value);
            }
            Stmt::Pritn(expr) => {
                let value = self.evaluate(&expr.expression)?;
                println!("{}", Interpreter::stringify(value));
            }
            Stmt::Expression(expr) => {
                self.evaluate(&expr.expression)?;
            }
            Stmt::Block(expr) => {
                self.environment.swap_new_scoped_env();
                for statement in &expr.statements {
                    if let Err(err) = self.execute(statement) {
                        self.environment = self.environment.get_upper_env();
                        return Err(err);
                    }
                }
                self.environment = self.environment.get_upper_env();
            }
            Stmt::If(expr) => {
                let value = self.evaluate(&expr.condition)?;
                if Interpreter::is_truthy(&value) {
                    self.execute(&expr.then_branch)?;
                } else if let Some(stmt) = &expr.else_branch {
                    self.execute(stmt)?;
                };
            }
            Stmt::While(stmt) => {
                while Interpreter::is_truthy(&self.evaluate(&stmt.condition)?) {
                    self.execute(&stmt.body)?;
                }
            }
            Stmt::Function(stmt) => {}
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
    fn evaluate<'token>(&mut self, expr: &Expr<'token>) -> Result<LiteralValue, RuntimeError> {
        match expr {
            Expr::Variable(name) => Ok(self.environment.get(&name.name)?.clone()),
            Expr::Literal(val) => Ok(val.value.clone()),
            Expr::Grouping(val) => self.evaluate(&val.expression),
            Expr::Unary(val) => {
                let right = self.evaluate(&val.right)?;
                let literal = match val.operator.token_type {
                    TokenType::Minus => {
                        let val = Interpreter::convert_number_operator(val.operator.line, right)?;
                        LiteralValue::Number(-val)
                    }
                    TokenType::Bang => LiteralValue::Bool(Interpreter::is_truthy(&right)),
                    // Unreachable.
                    _ => LiteralValue::None,
                };
                Ok(literal)
            }
            Expr::Call(expr) => {
                let callee = self.evaluate(&expr.callee)?;
                let arguments: Vec<LiteralValue> = expr
                    .arguments
                    .iter()
                    .map(|arg| self.evaluate(arg))
                    .collect::<Result<Vec<LiteralValue>, RuntimeError>>()?;
                todo!()
                // callee();
            }
            Expr::Binary(val) => {
                let left = self.evaluate(&val.left)?;
                let right = self.evaluate(&val.right)?;

                let literal = match val.operator.token_type {
                    TokenType::Minus => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator.line, left, right)?;
                        LiteralValue::Number(left - right)
                    }
                    TokenType::Slash => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator.line, left, right)?;
                        LiteralValue::Number(left / right)
                    }
                    TokenType::Star => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator.line, left, right)?;
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
                            val.operator.line,
                            "Operands must be numbers.".to_string(),
                        ))?,
                    },

                    TokenType::Greater => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator.line, left, right)?;
                        LiteralValue::Bool(left > right)
                    }

                    TokenType::GreaterEqual => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator.line, left, right)?;
                        LiteralValue::Bool(left >= right)
                    }
                    TokenType::Less => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator.line, left, right)?;
                        LiteralValue::Bool(left < right)
                    }
                    TokenType::LessEqual => {
                        let (left, right) =
                            Interpreter::convert_number_operators(val.operator.line, left, right)?;
                        LiteralValue::Bool(left <= right)
                    }

                    TokenType::BangEqual => LiteralValue::Bool(!Interpreter::is_equal(left, right)),
                    TokenType::EqualEqual => LiteralValue::Bool(Interpreter::is_equal(left, right)),
                    // Unreachable.
                    _ => LiteralValue::None,
                };
                Ok(literal)
            }
            Expr::Logical(val) => {
                let left = self.evaluate(&val.left)?;
                match val.operator.token_type {
                    TokenType::Or => {
                        if Interpreter::is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                    TokenType::And => {
                        if !Interpreter::is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                    _ => {
                        panic!(
                            "Error, expected Logical Operator but found: {}",
                            val.operator.lexeme
                        )
                    }
                }
                self.evaluate(&val.right)
            }
            Expr::Assign(assign_expr) => {
                let value = self.evaluate(&assign_expr.value)?;
                self.environment.assign(&assign_expr.name, value.clone())?;
                Ok(value)
            }
        }
    }

    fn is_truthy(val: &LiteralValue) -> bool {
        match val {
            LiteralValue::Bool(bool_val) => *bool_val,
            LiteralValue::None => false,
            _ => true,
        }
    }

    fn convert_number_operator(line: u32, operand: LiteralValue) -> Result<f64, RuntimeError> {
        match operand {
            LiteralValue::Number(operand) => Ok(operand),
            _ => Err(RuntimeError::new(
                line,
                "Operand must be a number.".to_string(),
            )),
        }
    }
    fn convert_number_operators(
        line: u32,
        left: LiteralValue,
        right: LiteralValue,
    ) -> Result<(f64, f64), RuntimeError> {
        match (left, right) {
            (LiteralValue::Number(left), LiteralValue::Number(right)) => Ok((left, right)),
            _ => Err(RuntimeError::new(
                line,
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
