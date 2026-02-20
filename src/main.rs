use std::{env, fs, io, path::Path, process};
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
    had_error: bool,
}

impl Lox {
    fn new() -> Lox {
        let had_error = false;
        Lox { had_error }
    }
    fn run_file(&self, file_path: &str) {
        let source = fs::read_to_string(file_path).unwrap();
        self.run(source);
        if self.had_error {
            process::exit(65);
        }
    }
    fn run_prompt(&mut self) {
        loop {
            println!("> ");
            let mut line = String::new();
            io::stdin()
                .read_line(&mut line)
                .expect("Failed to read line");
            self.run(line);
            self.had_error = false;
        }
    }
    fn run(&self, source: String) {
        let scanner = scanner::Scanner::new(source);
        scanner.scan_tokens();
        todo!()
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
    #[test]
    fn name() {
        todo!();
    }
}
