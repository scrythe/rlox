use rlox::scanner;
use std::hint::black_box;
use tango_bench::{IntoBenchmarks, benchmark_fn, tango_benchmarks, tango_main};

const SOURCE: &str = include_str!("../equality.lox");
pub fn scanner<'source>() -> (Vec<scanner::Token<'source>>, bool) {
    let scanner = scanner::Scanner::new(SOURCE);
    scanner.scan_tokens()
}

fn scanner_benchmarks() -> impl IntoBenchmarks {
    [benchmark_fn("scanner", |b| {
        b.iter(|| {
            black_box(scanner());
        })
    })]
}

tango_benchmarks!(scanner_benchmarks());
tango_main!();
