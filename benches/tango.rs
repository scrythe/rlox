// use rlox::scanner_new as scanner;
// use rlox::parser;
// use rlox::scanner;
use rlox::parser_new as parser;
use rlox::scanner_new as scanner;
use std::{fs, hint::black_box};
use tango_bench::{IntoBenchmarks, benchmark_fn, tango_benchmarks, tango_main};

fn scanner_benchmarks() -> impl IntoBenchmarks {
    [benchmark_fn("scanner", |b| {
        let source = fs::read_to_string("equality.lox").unwrap();
        b.iter(move || {
            let mut scanner = scanner::Scanner::new(&source);
            black_box(scanner.scan_tokens());
        })
    })]
}

fn parser_benchmarks() -> impl IntoBenchmarks {
    [benchmark_fn("parser", |b| {
        let source = fs::read_to_string("equality.lox").unwrap();
        let source = Box::leak(Box::new(source));
        let mut scanner = scanner::Scanner::new(source);
        let (tokens, _) = scanner.scan_tokens();
        b.iter(move || {
            let scanner = scanner::Scanner::new(source);
            let mut parser = parser::Parser::new(tokens.clone(), scanner);
            black_box(parser.parse());
        })
    })]
}

tango_benchmarks!(scanner_benchmarks(), parser_benchmarks());
tango_main!();
