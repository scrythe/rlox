pub struct AstPrinter {}

macro_rules! define_single_ast {
    // Binary          <'a>:                Expr: left, Expr: right
    ($class_type:ident $(<$lt:lifetime>)? -> $($field_names:ident: $field_class_types:ty),+;) => {
        struct $class_type $(<$lt>)? {
            $($field_names: $field_class_types,)+
        }

        impl $(<$lt>)?  $class_type $(<$lt>)? {
            fn new($($field_names: $field_class_types,)+)->$class_type $(<$lt>)? {
                $class_type {$($field_names),*}
            }
        }
    };
}

macro_rules! define_ast {
     ($expr_class:ident<$expr_lt:lifetime>;
     $($class_types:ident $(<$lt:lifetime>)? -> $($field_names:ident: $field_class_types:ty),+;)+) => {
        enum $expr_class<$expr_lt> {
            $($class_types(Box<$class_types $(<$lt>)? >)),+
        }
        $(
            define_single_ast!($class_types $(<$lt>)? -> $($field_names: $field_class_types),+;);
        )+
    };
}

use super::scanner::Literal;
use super::scanner::Token;

define_ast!(
    Expr<'a>;
    Binary<'a> -> left:Expr<'a> , operator:Token<'a> , right:Expr<'a> ;
    Grouping<'a> -> expression:Expr<'a> ;
    LiteralExpr<'a> -> value: Literal<'a> ;
    Unary<'a> -> operator:Token<'a> , right:Expr<'a> ;
);

impl AstPrinter {
    // pub fn print_stdout(expr: Expr) {
    //     let result = AstPrinter::print(expr);
    //     println!("{result}");
    // }
    pub fn print(expr: Expr) -> String {
        match expr {
            Expr::Binary(binary_expr) => {
                format!(
                    "{} {} {}",
                    str::from_utf8(binary_expr.operator.lexeme).unwrap(),
                    AstPrinter::print(binary_expr.left),
                    AstPrinter::print(binary_expr.right)
                )
            }
            Expr::Grouping(group_expr) => {
                format!("group {}", AstPrinter::print(group_expr.expression))
            }
            Expr::LiteralExpr(literal_expr) => match literal_expr.value {
                Literal::None => String::from(""),
                Literal::String(text) => text.to_string(),
                Literal::Number(val) => val.to_string(),
            },
            Expr::Unary(unary_expr) => format!(
                "{} {}",
                str::from_utf8(unary_expr.operator.lexeme).unwrap(),
                AstPrinter::print(unary_expr.right),
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::TokenType;
    #[test]
    fn test_ast() {
        let token = Token::new(TokenType::Plus, "+".as_bytes(), Literal::None, 1);
        let expr = Binary::new(
            Expr::LiteralExpr(Box::new(LiteralExpr::new(Literal::Number(5.0)))),
            token,
            Expr::LiteralExpr(Box::new(LiteralExpr::new(Literal::Number(4.3)))),
        );
        let expr = Expr::Binary(Box::new(expr));
        let out = AstPrinter::print(expr);
        assert_eq!(out, "+ 5 4.3")
    }
}
