use divan::{AllocProfiler, Bencher};
use rlox::{parser, scanner};
use std::{fs, hint::black_box};

// mod scanner;

#[global_allocator]
static ALLOC: AllocProfiler = AllocProfiler::system();

#[divan::bench]
fn scanner(bencher: Bencher) {
    let source = fs::read_to_string("equality.lox").unwrap();
    bencher.bench(|| {
        let scanner = scanner::Scanner::new(&source);
        black_box(scanner.scan_tokens())
    });
}

#[divan::bench]
fn parser(bencher: Bencher) {
    let source = fs::read_to_string("equality.lox").unwrap();
    let scanner = scanner::Scanner::new(&source);
    let (tokens, _) = scanner.scan_tokens();
    bencher
        .with_inputs(|| tokens.clone())
        .bench_values(|tokens| {
            let parser = parser::Parser::new(tokens);
            black_box(parser.parse());
        });
}

fn main() {
    divan::main();
}
