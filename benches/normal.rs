// use rlox::{parser, scanner};
use rlox::{
    astprinter_new, interpreter, interpreter_new, parser,
    parser_new::{self, LoxError},
    scanner, scanner_new,
};
use std::{fs, hint::black_box};

// fn scan_tokens(source: &str) -> (Vec<scanner::Token>, bool) {
//     let mut scanner = scanner::Scanner::new(source);
//     black_box(scanner.scan_tokens())
// }
//
// fn bench_scanner() {
//     let source = fs::read_to_string("equality.lox").unwrap();
//
//     let mut scanner = scanner::Scanner::new(&source);
//     black_box(scanner.scan_tokens());
// }
//
// fn bench_parser() {
//     let source = fs::read_to_string("equality.lox").unwrap();
//     let mut scanner = scanner::Scanner::new(&source);
//     let (tokens, _) = scanner.scan_tokens();
//
//     let mut parser = parser::Parser::new(tokens, scanner);
//
//     black_box(parser.parse());
// }

fn bench_interpreter_old() {
    let source = fs::read_to_string("equality.lox").unwrap();
    let scanner = scanner::Scanner::new(&source);
    let (tokens, _) = scanner.scan_tokens();
    let parser = parser::Parser::new(tokens);
    let (statements, _) = parser.parse();
    let mut interpreter = interpreter::Interpreter::new();
    if let Err(err) = black_box(interpreter.interpret(statements)) {
        LoxError::runtime_error(err.line, err.message);
    }
}

fn bench_interpreter_new() {
    let source = fs::read_to_string("equality.lox").unwrap();
    let mut scanner = scanner_new::Scanner::new(&source);
    let (tokens, _) = scanner.scan_tokens();
    let mut parser = parser_new::Parser::new(tokens, scanner);
    let (statements, statements_arena, expressions, strings, has_error) = parser.parse();
    // let astprinter = astprinter_new::_AstPrinter::new(statements_arena, expressions, strings);
    // let res = astprinter._print_statements(&statements);
    // print!("{}", res);
    let mut interpreter = interpreter_new::Interpreter::new(statements_arena, expressions, strings);
    if let Err(err) = black_box(interpreter.interpret(statements)) {
        LoxError::runtime_error(err.line, err.message);
    }
}

fn main() {
    // bench_scanner();
    // bench_parser();
    // bench_interpreter_old();
    bench_interpreter_new();
}
