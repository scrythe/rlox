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

struct Token<'a> {
    test: &'a str,
}
// struct Token {}
struct Literal {}

define_ast!(
    Expr<'a>;
    Binary<'a> -> left:Expr<'a> , operator:Token<'a> , right:Expr<'a> ;
    Grouping<'a> -> expression:Expr<'a> ;
    LiteralExpr -> value: Literal ;
    Unary<'a> -> operator:Token<'a> , right:Expr<'a> ;
);

fn main() {
    Binary::new(
        Expr::LiteralExpr(Box::new(LiteralExpr::new(Literal {}))),
        Token { test: "hm" },
        Expr::LiteralExpr(Box::new(LiteralExpr::new(Literal {}))),
    );
}
