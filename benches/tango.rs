// use rlox::interpreter::Interpreter;
// use rlox::parser;
// use rlox::scanner;
use rlox::interpreter_new_2;
use rlox::parser_new_2;
use rlox::scanner_new;
use std::{fs, hint::black_box};
use tango_bench::{IntoBenchmarks, benchmark_fn, tango_benchmarks, tango_main};

fn interpreter_benchmark() -> impl IntoBenchmarks {
    [benchmark_fn("interpreter", |b| {
        let source = fs::read_to_string("equality.lox").unwrap();
        let source = Box::leak(Box::new(source));
        let mut scanner = scanner_new::Scanner::new(source);
        let (tokens, _) = scanner.scan_tokens();
        let mut parser = parser_new_2::Parser::new(tokens.clone(), scanner);
        let (statements, statements_arena, expressions, strings, has_error) = parser.parse();
        b.iter(move || {
            let mut interpreter = interpreter_new_2::Interpreter::new(
                statements_arena.clone(),
                expressions.clone(),
                strings.clone(),
            );
            interpreter.interpret(statements.clone())
        })
    })]
}

// fn interpreter_benchmark() -> impl IntoBenchmarks {
//     [benchmark_fn("interpreter", |b| {
//         let source = fs::read_to_string("equality.lox").unwrap();
//         let source = Box::leak(Box::new(source));
//         let scanner = scanner::Scanner::new(source);
//         let (tokens, _) = scanner.scan_tokens();
//         let parser = parser::Parser::new(tokens.clone());
//         let (statements, _) = parser.parse();
//         b.iter(move || {
//             let mut interpreter = Interpreter::new();
//             interpreter.interpret(statements.clone())
//         })
//     })]
// }

tango_benchmarks!(interpreter_benchmark());
tango_main!();
