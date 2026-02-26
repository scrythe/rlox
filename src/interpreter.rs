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
                        LiteralValue::True | LiteralValue::False => right,
                        _ => LiteralValue::False,
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
                    TokenType::Plus => {
                        match (left, right) {
                            (LiteralValue::Number(left), LiteralValue::Number(right)) => {
                                LiteralValue::Number(left - right)
                            }
                            (LiteralValue::String(left), LiteralValue::String(right)) => {
                                // LiteralValue::String(&format!("{left}{right}"))
                                LiteralValue::String(left)
                            }
                            _ => LiteralValue::None,
                        }
                    }

                    TokenType::Greater => {
                        if left.to_number().unwrap() > right.to_number().unwrap() {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::GreaterEqual => {
                        if left.to_number().unwrap() >= right.to_number().unwrap() {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::Less => {
                        if left.to_number().unwrap() < right.to_number().unwrap() {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::LessEqual => {
                        if left.to_number().unwrap() <= right.to_number().unwrap() {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }

                    TokenType::BangEqual => Interpreter::negate(Interpreter::is_equal(left, right)),
                    TokenType::EqualEqual => Interpreter::is_equal(left, right),
                    // Unreachable.
                    _ => LiteralValue::None,
                }
            }
        }
    }
    fn is_equal<'a>(a: LiteralValue<'a>, b: LiteralValue<'a>) -> LiteralValue<'a> {
        if a == LiteralValue::None && b == LiteralValue::None {
            LiteralValue::True
        } else if a == LiteralValue::None {
            LiteralValue::False
        } else if a == b {
            LiteralValue::True
        } else {
            LiteralValue::False
        }
    }

    fn negate(a: LiteralValue) -> LiteralValue {
        if a == LiteralValue::False {
            LiteralValue::True
        } else {
            LiteralValue::False
        }
    }
}
