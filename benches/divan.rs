use divan::{AllocProfiler, Bencher};
use rlox::scanner;
use std::{fs, hint::black_box};

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

fn main() {
    divan::main();
}
