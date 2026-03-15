use divan::AllocProfiler;
use rlox::scanner::{self, Token};

#[global_allocator]
static ALLOC: AllocProfiler = AllocProfiler::system();

const SOURCE: &str = include_str!("../equality.lox");
#[divan::bench]
fn scanner<'source>() -> (Vec<Token<'source>>, bool) {
    let scanner = scanner::Scanner::new(SOURCE);
    scanner.scan_tokens()
}

fn main() {
    // Run registered benchmarks.
    divan::main();
}
