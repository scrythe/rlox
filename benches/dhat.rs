use std::{fs, hint::black_box};

use rlox::{parser, scanner};

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn bench_scanner() {
    let source = fs::read_to_string("equality.lox").unwrap();

    let _profiler = dhat::Profiler::new_heap();

    let scanner = scanner::Scanner::new(&source);
    black_box(scanner.scan_tokens());
}

fn bench_parser() {
    let source = fs::read_to_string("equality.lox").unwrap();

    let scanner = scanner::Scanner::new(&source);
    let (tokens, _) = scanner.scan_tokens();

    let _profiler = dhat::Profiler::new_heap();

    let parser = parser::Parser::new(tokens);
    black_box(parser.parse());
}

fn main() {
    // bench_scanner();
    bench_parser();
}
