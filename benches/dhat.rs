// use rlox::{parser, scanner};
use rlox::{parser_new as parser, scanner_new as scanner};
use std::{fs, hint::black_box};

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn scan_tokens(source: &str) -> (Vec<scanner::Token>, bool) {
    let mut scanner = scanner::Scanner::new(source);
    black_box(scanner.scan_tokens())
}

fn bench_scanner() {
    let source = fs::read_to_string("equality.lox").unwrap();
    let _profiler = dhat::Profiler::new_heap();

    let mut scanner = scanner::Scanner::new(&source);
    black_box(scanner.scan_tokens());
}

fn bench_parser() {
    let source = fs::read_to_string("equality.lox").unwrap();
    let mut scanner = scanner::Scanner::new(&source);
    let (tokens, _) = scanner.scan_tokens();
    let mut parser = parser::Parser::new(tokens, scanner);

    let _profiler = dhat::Profiler::new_heap();

    // let (statements, _) = black_box(parser.parse());
    black_box(parser.parse());
    // dbg!(size_of::<parser::Expr>());
    // dbg!(size_of::<parser::Stmt>() * statements.capacity());
    // dbg!(size_of::<parser::Expr>() * parser.expressions.capacity());
    // dbg!(
    //     size_of::<parser::Expr>() * parser.expressions.capacity()
    //         - 12 * parser.expressions.capacity()
    // );
    // dbg!(size_of::<String>() * parser.strings.capacity());
}

fn main() {
    // bench_scanner();
    bench_parser();
}
