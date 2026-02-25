use std::{
    env, fs,
    io::{self, Write},
    process,
};
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
        let scanner = scanner::Scanner::new(&source, &mut self.lox_error);
        let tokens = scanner.scan_tokens();
        // let ast_printer = parser::AstPrinter::print();

        for token in tokens {
            println!("{:?}", token);
        }

        // TODO complete ig
    }
}

struct LoxError {
    had_error: bool,
}
impl LoxError {
    fn new() -> LoxError {
        let had_error = false;
        LoxError { had_error }
    }

    fn error(&mut self, line: u32, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: u32, err_where: &str, message: &str) {
        println!("[line {line}] Error {err_where}: {message}");
        self.had_error = true;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let source = r#"/ *
// aaa *
()
" s "
5.7
"
"#;
        dbg!(source);
        let mut lox = Lox::new();
        lox.run(source.to_string());
    }
}
