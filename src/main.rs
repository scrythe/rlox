use std::{
    env, fs,
    io::{self, Write},
    process,
};

use crate::scanner::{Token, TokenType};
mod astprinter;
mod environment;
mod interpreter;
mod parser;
mod scanner;
fn main() {
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    if args.len() > 2 {
        println!("Usage: jlox [script]");
        process::exit(64);
    } else if args.len() == 2 {
        lox.run_file(&args[1]);
    } else {
        lox.run_prompt();
    }
}

struct Lox {
    lox_error: LoxError,
}

impl Lox {
    fn new() -> Lox {
        let lox_error = LoxError::new();
        Lox { lox_error }
    }
    fn run_file(&mut self, file_path: &str) {
        let source = fs::read_to_string(file_path).unwrap();
        self.run(source);
        if self.lox_error.had_error {
            process::exit(65);
        }
        if self.lox_error.had_runtime_error {
            process::exit(70);
        }
    }
    fn run_prompt(&mut self) {
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut line = String::new();
            io::stdin()
                .read_line(&mut line)
                .expect("Failed to read line");
            if line.is_empty() {
                break;
            }
            self.run(line);
            self.lox_error.had_error = false;
        }
    }
    fn run(&mut self, source: String) {
        let scanner = scanner::Scanner::new(&source);
        let (tokens, has_scan_error) = scanner.scan_tokens();

        let parser = parser::Parser::new(tokens, &mut self.lox_error);
        let statements = parser.parse();

        if has_scan_error {
            return;
        }

        // let ast_print_res = astprinter::AstPrinter::print(expression);
        let interpreter = interpreter::Interpreter::new(&mut self.lox_error);
        interpreter.interpret(statements);

        // TODO: complete ig
    }
}

fn error_token(token: &Token, message: &str) {
    if token.token_type == TokenType::Eof {
        report(token.line, " at end", message);
    } else {
        report(token.line, &format!(" at '{}'", token.lexeme), message);
    }
}

fn error_line(line: u32, message: &str) {
    report(line, "", message);
}

fn report(line: u32, err_where: &str, message: &str) {
    println!("[line {line}] Error {err_where}: {message}");
}

struct LoxError {
    had_error: bool,
    had_runtime_error: bool,
}
impl LoxError {
    fn new() -> LoxError {
        let had_error = false;
        let had_runtime_error = false;
        LoxError {
            had_error,
            had_runtime_error,
        }
    }

    fn error_token(&mut self, token: &Token, message: &str) {
        if token.token_type == TokenType::Eof {
            self.report(token.line, " at end", message);
        } else {
            self.report(token.line, &format!(" at '{}'", token.lexeme), message);
        }
    }

    fn report(&mut self, line: u32, err_where: &str, message: &str) {
        println!("[line {line}] Error {err_where}: {message}");
        self.had_error = true;
    }

    pub fn runtime_error(&mut self, token: Token, message: &str) {
        println!("{message}\n[line {}]", token.line);
        self.had_runtime_error = true;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_parser_and_ast_printer() {
        let mut lox = Lox::new();
        let source = "2+5 / 4 * 2 + 4 == -3";
        let scanner = scanner::Scanner::new(source);
        let (tokens, has_scan_error) = scanner.scan_tokens();

        let parser = parser::Parser::new(tokens, &mut lox.lox_error);
        let expression = parser.parse();
        // let ast_print_res = astprinter::AstPrinter::_print(expression);
        // assert_eq!(ast_print_res, "(== (+ (+ 2 (* (/ 5 4) 2)) 4) (- 3))");
    }
}
