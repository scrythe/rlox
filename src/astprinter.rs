use crate::{parser::Expr, scanner::LiteralValue};

pub struct AstPrinter {}

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

#[cfg(test)]
mod test {
    use crate::scanner::{Token, TokenType};

    use super::*;
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
