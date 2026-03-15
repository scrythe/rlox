use rlox::scanner_new as scanner;
use std::{fs, hint::black_box};

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn scan_tokens(source: &str) -> (Vec<scanner::Token>, bool) {
    let mut scanner = scanner::Scanner::new(source);
    black_box(scanner.scan_tokens())
}

fn main() {
    let source = fs::read_to_string("equality.lox").unwrap();

    let _profiler = dhat::Profiler::new_heap();

    black_box(scan_tokens(&source));
}
