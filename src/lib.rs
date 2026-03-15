use std::{
    fs,
    io::{self, Write},
    process,
};

pub mod astprinter;
pub mod environment;
pub mod interpreter;
pub mod parser;
pub mod scanner;

pub mod parser_new;
pub mod parser_test;
pub mod scanner_new;

pub struct Lox {
    interpreter: interpreter::Interpreter,
}

impl Lox {
    pub fn new() -> Lox {
        let interpreter = interpreter::Interpreter::new();
        Lox { interpreter }
    }

    pub fn run_file(mut self, file_path: &str) {
        let source = fs::read_to_string(file_path).unwrap();
        let lox_error = self.run(source);
        if let Err(lox_error) = lox_error {
            match lox_error {
                LoxError::CompileError => process::exit(65),
                LoxError::RuntimeError => process::exit(70),
            }
        }
    }
    pub fn run_prompt(mut self) {
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
        // let res = astprinter::_AstPrinter::_print_statements(&statements);
        // dbg!(res);

        if has_scan_error || has_parser_error {
            return Err(LoxError::CompileError);
        }

        let res = self.interpreter.interpret(statements);
        if let Err(err) = res {
            LoxError::runtime_error(err.line, err.message);
            return Err(LoxError::RuntimeError);
        }
        Ok(())
    }
}

impl Default for Lox {
    fn default() -> Lox {
        Lox::new()
    }
}

pub enum LoxError {
    CompileError,
    RuntimeError,
}

impl LoxError {
    pub fn error_token(token: &scanner::Token, message: &str) {
        if token.token_type == scanner::TokenType::Eof {
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

    pub fn runtime_error(line: u32, message: String) {
        println!("{message}\n[line {}]", line);
    }
}
