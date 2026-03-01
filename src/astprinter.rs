use crate::{
    parser::{Expr, Stmt},
    scanner::LiteralValue,
};

pub struct _AstPrinter {}

impl _AstPrinter {
    pub fn _print_statements(statements: &Vec<Stmt>) -> String {
        statements
            .iter()
            .map(_AstPrinter::_print_statement)
            .collect()
    }

    pub fn _print_statement(statement: &Stmt) -> String {
        match statement {
            Stmt::If(stmt) => {
                let condition = _AstPrinter::_print_expression(&stmt.condition);
                let then_branch = _AstPrinter::_print_statement(&stmt.then_branch);
                let else_branch = match &stmt.else_branch {
                    Some(else_branch) => _AstPrinter::_print_statement(else_branch),
                    None => "".to_string(),
                };
                format!(
                    "if ({}) then {} else {}",
                    condition, then_branch, else_branch
                )
            }
            Stmt::While(stmt) => {
                let condition = _AstPrinter::_print_expression(&stmt.condition);
                let body = _AstPrinter::_print_statement(&stmt.body);
                format!("while ({}) {}", condition, body)
            }
            Stmt::Block(stmt) => stmt
                .statements
                .iter()
                .map(|stmt| _AstPrinter::_print_statement(stmt) + "\n")
                .collect(),
            Stmt::Expression(stmt) => _AstPrinter::_print_expression(&stmt.expression),
            Stmt::Pritn(stmt) => {
                "print".to_string() + &_AstPrinter::_print_expression(&stmt.expression)
            }
            Stmt::Var(stmt) => {
                format!(
                    "var {} = {};\n",
                    stmt.name.lexeme,
                    _AstPrinter::_print_expression(&stmt.initializer)
                )
            }
            Stmt::Function(stmt) => {
                todo!()
            }
        }
    }

    pub fn _print_expression(expr: &Expr) -> String {
        match expr {
            Expr::Assign(assign_expr) => {
                format!(
                    "{} {}",
                    assign_expr.name.lexeme,
                    _AstPrinter::_print_expression(&assign_expr.value)
                )
            }
            Expr::Binary(binary_expr) => {
                format!(
                    "({} {} {})",
                    binary_expr.operator.lexeme,
                    _AstPrinter::_print_expression(&binary_expr.left),
                    _AstPrinter::_print_expression(&binary_expr.right)
                )
            }
            Expr::Grouping(group_expr) => {
                format!(
                    "(group {})",
                    _AstPrinter::_print_expression(&group_expr.expression)
                )
            }
            Expr::Literal(literal_expr) => match &literal_expr.value {
                LiteralValue::None => String::from(""),
                LiteralValue::String(text) => text.to_string(),
                LiteralValue::Number(val) => val.to_string(),
                LiteralValue::Bool(bool) => bool.to_string(),
            },
            Expr::Unary(unary_expr) => format!(
                "({} {})",
                unary_expr.operator.lexeme,
                _AstPrinter::_print_expression(&unary_expr.right),
            ),
            Expr::Variable(var_expr) => var_expr.name.lexeme.to_string(),
            Expr::Logical(logcal_expr) => format!(
                "({} {} {})",
                logcal_expr.operator.lexeme,
                _AstPrinter::_print_expression(&logcal_expr.left),
                _AstPrinter::_print_expression(&logcal_expr.right)
            ),
            Expr::Call(_) => todo!(),
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
        let out = _AstPrinter::_print_expression(&expr);
        assert_eq!(out, "(+ 5 4.3)")
    }
}
