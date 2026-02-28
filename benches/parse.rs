use std::hint::black_box;
use tango_bench::{IntoBenchmarks, benchmark_fn, tango_benchmarks, tango_main};

pub fn factorial(mut n: usize) -> usize {
    let mut result = 1usize;
    while n > 0 {
        result = result.wrapping_mul(black_box(n));
        n -= 1;
    }
    result
}

pub fn parse_benchmarks(mut n: usize) -> usize {
    let parser = parser::Parser::new(tokens);
    let (statements, has_parser_error) = parser.parse();
}

fn factorial_benchmarks() -> impl IntoBenchmarks {
    [benchmark_fn("factorial", |b| b.iter(|| factorial(495)))]
}

tango_benchmarks!(factorial_benchmarks());
tango_main!();
