use crate::{
    parser::Expr,
    scanner::{LiteralValue, TokenType},
};

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(expr: Expr) -> LiteralValue {
        match expr {
            Expr::Literal(val) => val.value,
            Expr::Grouping(val) => Interpreter::interpret(val.expression),
            Expr::Unary(val) => {
                let right = Interpreter::interpret(val.right);
                match val.operator.token_type {
                    TokenType::Minus => LiteralValue::Number(-right.to_number().unwrap()),
                    TokenType::Bang => match right {
                        LiteralValue::Bool(_) => right,
                        _ => LiteralValue::Bool(false),
                    },
                    // Unreachable.
                    _ => LiteralValue::None,
                }
            }
            Expr::Binary(val) => {
                let left = Interpreter::interpret(val.left);
                let right = Interpreter::interpret(val.right);

                match val.operator.token_type {
                    TokenType::Minus => {
                        LiteralValue::Number(left.to_number().unwrap() - right.to_number().unwrap())
                    }
                    TokenType::Slash => {
                        LiteralValue::Number(left.to_number().unwrap() / right.to_number().unwrap())
                    }
                    TokenType::Star => {
                        LiteralValue::Number(left.to_number().unwrap() * right.to_number().unwrap())
                    }
                    TokenType::Plus => match (left, right) {
                        (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                            LiteralValue::Number(left + right)
                        }
                        (LiteralValue::String(left), LiteralValue::String(right)) => {
                            LiteralValue::String(left + &right)
                        }
                        _ => LiteralValue::None,
                    },

                    TokenType::Greater => {
                        LiteralValue::Bool(left.to_number().unwrap() > right.to_number().unwrap())
                    }

                    TokenType::GreaterEqual => {
                        LiteralValue::Bool(left.to_number().unwrap() >= right.to_number().unwrap())
                    }
                    TokenType::Less => {
                        LiteralValue::Bool(left.to_number().unwrap() < right.to_number().unwrap())
                    }
                    TokenType::LessEqual => {
                        LiteralValue::Bool(left.to_number().unwrap() <= right.to_number().unwrap())
                    }

                    TokenType::BangEqual => LiteralValue::Bool(!Interpreter::is_equal(left, right)),
                    TokenType::EqualEqual => LiteralValue::Bool(Interpreter::is_equal(left, right)),
                    // Unreachable.
                    _ => LiteralValue::None,
                }
            }
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
