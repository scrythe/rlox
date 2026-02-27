use crate::scanner::{Token, TokenType};
use std::{
    env, fs,
    io::{self, Write},
    process,
};

mod astprinter;
mod environment;
mod interpreter;
mod parser;
mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    let lox = Lox::new();
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
    interpreter: interpreter::Interpreter,
}

impl Lox {
    fn new() -> Lox {
        let interpreter = interpreter::Interpreter::new();
        Lox { interpreter }
    }

    fn run_file(mut self, file_path: &str) {
        let source = fs::read_to_string(file_path).unwrap();
        let lox_error = self.run(source);
        if let Err(lox_error) = lox_error {
            match lox_error {
                LoxError::CompileError => process::exit(65),
                LoxError::RuntimeError => process::exit(70),
            }
        }
    }
    fn run_prompt(mut self) {
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
            let _ = self.run(line);
        }
    }
    fn run(&mut self, source: String) -> Result<(), LoxError> {
        let scanner = scanner::Scanner::new(&source);
        let (tokens, has_scan_error) = scanner.scan_tokens();

        let parser = parser::Parser::new(tokens);
        let (statements, has_parser_error) = parser.parse();

        if has_scan_error || has_parser_error {
            return Err(LoxError::CompileError);
        }

        // let ast_print_res = astprinter::AstPrinter::print(expression);
        let res = self.interpreter.interpret(statements);
        if let Err(err) = res {
            LoxError::runtime_error(err.token, err.message);
            return Err(LoxError::RuntimeError);
        }

        // TODO: complete ig
        Ok(())
    }
}

enum LoxError {
    CompileError,
    RuntimeError,
}
impl LoxError {
    pub fn error_token(token: &Token, message: &str) {
        if token.token_type == TokenType::Eof {
            LoxError::report(token.line, " at end", message);
        } else {
            LoxError::report(token.line, &format!(" at '{}'", token.lexeme), message);
        }
    }

    pub fn error_line(line: u32, message: &str) {
        LoxError::report(line, "", message);
    }

    fn report(line: u32, err_where: &str, message: &str) {
        println!("[line {line}] Error {err_where}: {message}");
    }

    pub fn runtime_error(token: Token, message: String) {
        println!("{message}\n[line {}]", token.line);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Stmt;

    #[test]
    fn test_parser_and_ast_printer() {
        let source = "2+5 / 4 * 2 + 4 == -3;";
        let scanner = scanner::Scanner::new(source);
        let (tokens, has_scan_error) = scanner.scan_tokens();

        if has_scan_error {
            panic!("Unexpected scan error");
        }

        let parser = parser::Parser::new(tokens);
        let (mut statements, _) = parser.parse();
        match statements.remove(0) {
            Stmt::Expression(expr) => {
                let ast_print_res = astprinter::_AstPrinter::_print(expr.expression);
                assert_eq!(ast_print_res, "(== (+ (+ 2 (* (/ 5 4) 2)) 4) (- 3))");
            }
            _ => panic!("error"),
        }
    }
}
