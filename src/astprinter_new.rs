use crate::parser_new::{Expr, Object, Stmt};

pub struct _AstPrinter<'stmt_lt, 'expr_lt, 'string_lt> {
    statements: Vec<Stmt<'stmt_lt, 'expr_lt, 'string_lt>>,
    expressions: Vec<Expr<'expr_lt, 'string_lt>>,
    strings: Vec<String>,
}

impl<'stmt_lt, 'expr_lt, 'string_lt> _AstPrinter<'stmt_lt, 'expr_lt, 'string_lt> {
    pub fn new(
        statements: Vec<Stmt<'stmt_lt, 'expr_lt, 'string_lt>>,
        expressions: Vec<Expr<'expr_lt, 'string_lt>>,
        strings: Vec<String>,
    ) -> _AstPrinter<'stmt_lt, 'expr_lt, 'string_lt> {
        _AstPrinter {
            statements,
            expressions,
            strings,
        }
    }

    pub fn _print_statements(&self, statements: &Vec<Stmt>) -> String {
        statements
            .iter()
            .map(|stmt| self._print_statement(stmt))
            .collect()
    }

    pub fn _print_statement(&self, statement: &Stmt) -> String {
        match statement {
            Stmt::If(stmt) => {
                let condition = self.get_expression(stmt.condition);
                let condition = self._print_expression(condition);
                let then_branch = self.get_statement(stmt.then_branch);
                let then_branch = self._print_statement(then_branch);
                let else_branch = match &stmt.else_branch {
                    Some(else_branch) => {
                        let else_branch = self.get_statement(*else_branch);
                        self._print_statement(else_branch)
                    }
                    None => "".to_string(),
                };
                format!(
                    "(if ({}) then {} else {})",
                    condition, then_branch, else_branch
                )
            }
            Stmt::While(stmt) => {
                let condition = self.get_expression(stmt.condition);
                let condition = self._print_expression(condition);
                let body = self.get_statement(stmt.body);
                let body = self._print_statement(body);
                format!("(while {}\n{})", condition, body)
            }
            Stmt::Block(stmt) => {
                let stmt_start = stmt.statements_start as usize;
                let stmt_end = stmt.statements_end as usize;
                self.statements[stmt_start..=stmt_end]
                    .iter()
                    .map(|stmt| self._print_statement(stmt) + "\n")
                    .collect()
            }
            Stmt::Expression(stmt) => self._print_expression(self.get_expression(stmt.expression)),
            Stmt::Pritn(stmt) => {
                format!(
                    "(print {})",
                    self._print_expression(self.get_expression(stmt.expression))
                )
            }
            Stmt::Var(stmt) => {
                let initializer = self.get_expression(stmt.initializer);
                format!(
                    "var {} = {};\n",
                    self.get_string(stmt.name),
                    self._print_expression(initializer)
                )
            }
        }
    }

    pub fn _print_expression(&self, expr: &Expr) -> String {
        match expr {
            Expr::Assign(expr) => {
                let name = self.get_string(expr.name);
                let value = self.get_expression(expr.value);
                let value = self._print_expression(value);
                format!("(= {} {})", name, value)
            }
            Expr::Binary(expr) => {
                let left = self.get_expression(expr.left);
                let right = self.get_expression(expr.right);
                format!(
                    "({:?} {} {})",
                    expr.operator,
                    self._print_expression(left),
                    self._print_expression(right)
                )
            }
            Expr::Grouping(expr) => {
                let expression = self.get_expression(expr.expression);
                format!("(group {})", self._print_expression(expression))
            }
            Expr::Literal(literal_expr) => match &literal_expr.value {
                Object::None => String::from("nil"),
                Object::String(val, _) => self.get_string(*val).to_string(),
                Object::Number(val) => val.to_string(),
                Object::Bool(val) => val.to_string(),
            },
            Expr::Unary(expr) => format!(
                "({:?} {})",
                expr.operator,
                self._print_expression(self.get_expression(expr.right)),
            ),
            Expr::Variable(expr) => self.get_string(expr.name).to_string(),
            Expr::Logical(expr) => format!(
                "({:?} {} {})",
                expr.operator,
                self._print_expression(self.get_expression(expr.left)),
                self._print_expression(self.get_expression(expr.right))
            ),
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
}
