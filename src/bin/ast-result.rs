struct Token<'a> {
    test: &'a str,
}
struct Literal {}
enum Expr<'a> {
    Binary(Box<Binary<'a>>),
    Grouping(Box<Grouping<'a>>),
    LiteralExpr(Box<LiteralExpr>),
    Unary(Box<Unary<'a>>),
}
struct Binary<'a> {
    left: Expr<'a>,
    operator: Token<'a>,
    right: Expr<'a>,
}
impl<'a> Binary<'a> {
    fn new(left: Expr<'a>, operator: Token<'a>, right: Expr<'a>) -> Binary<'a> {
        Binary {
            left,
            operator,
            right,
        }
    }
}
struct Grouping<'a> {
    expression: Expr<'a>,
}
impl<'a> Grouping<'a> {
    fn new(expression: Expr<'a>) -> Grouping<'a> {
        Grouping { expression }
    }
}
struct LiteralExpr {
    value: Literal,
}
impl LiteralExpr {
    fn new(value: Literal) -> LiteralExpr {
        LiteralExpr { value }
    }
}
struct Unary<'a> {
    operator: Token<'a>,
    right: Expr<'a>,
}
impl<'a> Unary<'a> {
    fn new(operator: Token<'a>, right: Expr<'a>) -> Unary<'a> {
        Unary { operator, right }
    }
}

fn main() {}
