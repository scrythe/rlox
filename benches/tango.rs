use rlox::scanner;
use std::{fs, hint::black_box};
use tango_bench::{IntoBenchmarks, benchmark_fn, tango_benchmarks, tango_main};

fn scanner_benchmarks() -> impl IntoBenchmarks {
    [benchmark_fn("scanner", |b| {
        let source = fs::read_to_string("equality.lox").unwrap();
        b.iter(move || {
            let scanner = scanner::Scanner::new(&source);
            black_box(scanner.scan_tokens());
        })
    })]
}

tango_benchmarks!(scanner_benchmarks());
tango_main!();
