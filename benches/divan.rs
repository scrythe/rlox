use divan::{AllocProfiler, Bencher};
use rlox::{
    interpreter::{self, Interpreter},
    interpreter_new, parser, parser_new, scanner, scanner_new,
};
use std::{fs, hint::black_box};

#[global_allocator]
static ALLOC: AllocProfiler = AllocProfiler::system();

// #[divan::bench]
// fn scanner(bencher: Bencher) {
//     let source = fs::read_to_string("equality.lox").unwrap();
//     bencher.bench(|| {
//         let mut scanner = scanner::Scanner::new(&source);
//         black_box(scanner.scan_tokens())
//     });
// }
//
// #[divan::bench]
// fn parser(bencher: Bencher) {
//     let source = fs::read_to_string("equality.lox").unwrap();
//     bencher
//         .with_inputs(|| {
//             let mut scanner = scanner::Scanner::new(&source);
//             let (tokens, _) = scanner.scan_tokens();
//             (tokens, scanner)
//         })
//         .bench_values(|(tokens, scanner)| {
//             let mut parser = parser::Parser::new(tokens, scanner);
//             black_box(parser.parse());
//         });
//     // bencher.bench(|| {
//     //     let mut scanner = scanner::Scanner::new(&source);
//     //     let (tokens, _) = scanner.scan_tokens();
//     //     let mut parser = parser::Parser::new(tokens, scanner);
//     //     black_box(parser.parse());
//     // });
// }

#[divan::bench]
fn interpreter1(bencher: Bencher) {
    bencher
        .with_inputs(|| {
            let source = fs::read_to_string("equality.lox").unwrap();
            let source = Box::leak(Box::new(source));
            let scanner = scanner::Scanner::new(source);
            let (tokens, _) = scanner.scan_tokens();
            let parser = parser::Parser::new(tokens.clone());
            let (statements, _) = parser.parse();
            statements
        })
        .bench_values(|statements| {
            let mut interpreter = Interpreter::new();
            let _ = interpreter.interpret(statements);
        });
}
#[divan::bench]
fn interpreter2(bencher: Bencher) {
    let source = fs::read_to_string("equality.lox").unwrap();
    bencher
        .with_inputs(|| {
            let mut scanner = scanner_new::Scanner::new(&source);
            let (tokens, _) = scanner.scan_tokens();
            let mut parser = parser_new::Parser::new(tokens, scanner);
            let (statements, statements_arena, expressions, strings, has_error) = parser.parse();
            (
                statements,
                statements_arena,
                expressions,
                strings,
                has_error,
            )
        })
        .bench_values(
            |(statements, statements_arena, expressions, strings, has_error)| {
                let mut interpreter =
                    interpreter_new::Interpreter::new(statements_arena, expressions, strings);
                interpreter.interpret(statements)
            },
        );
}

fn main() {
    divan::main();
}
