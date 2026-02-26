use crate::{
    LoxError,
    parser::Expr,
    scanner::{LiteralValue, Token, TokenType},
};

struct RuntimeError<'a, 'b> {
    token: Token<'a>,
    message: &'b str,
}

impl<'a, 'b> RuntimeError<'a, 'b> {
    fn new(token: Token<'a>, message: &'b str) -> RuntimeError<'a, 'b> {
        RuntimeError { token, message }
    }
}

pub struct Interpreter<'a> {
    lox_error: &'a mut LoxError,
}

impl<'a> Interpreter<'a> {
    pub fn new(lox_error: &'a mut LoxError) -> Interpreter<'a> {
        Interpreter { lox_error }
    }

    pub fn interpret(self, expr: Expr) {
        match Interpreter::evaluate(expr) {
            Ok(val) => println!("{}", Interpreter::stringify(val)),
            Err(runtime_error) => self
                .lox_error
                .runtime_error(runtime_error.token, runtime_error.message),
        }
    }
    fn stringify(evaluated: LiteralValue) -> String {
        match evaluated {
            LiteralValue::None => "nil".to_string(),
            LiteralValue::Number(number) => number.to_string(),
            LiteralValue::Bool(bool) => bool.to_string(),
            LiteralValue::String(text) => text,
        }
    }
    fn evaluate<'b, 'c>(expr: Expr<'b>) -> Result<LiteralValue, RuntimeError<'b, 'c>> {
        match expr {
            Expr::Literal(val) => Ok(val.value),
            Expr::Grouping(val) => Interpreter::evaluate(val.expression),
            Expr::Unary(val) => {
                let right = Interpreter::evaluate(val.right)?;
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
                let left = Interpreter::evaluate(val.left)?;
                let right = Interpreter::evaluate(val.right)?;

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
                        _ => Err(RuntimeError::new(val.operator, "Operands must be numbers"))?,
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
    fn convert_number_operator<'b, 'c>(
        operator: Token<'b>,
        operand: LiteralValue,
    ) -> Result<f64, RuntimeError<'b, 'c>> {
        match operand {
            LiteralValue::Number(operand) => Ok(operand),
            _ => Err(RuntimeError::new(operator, "Operand must be a number")),
        }
    }
    fn convert_number_operators<'b, 'c>(
        operator: Token<'b>,
        left: LiteralValue,
        right: LiteralValue,
    ) -> Result<(f64, f64), RuntimeError<'b, 'c>> {
        match (left, right) {
            (LiteralValue::Number(left), LiteralValue::Number(right)) => Ok((left, right)),
            _ => Err(RuntimeError::new(operator, "Operands must be numbers")),
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
