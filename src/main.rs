use rlox::Lox;
use std::{env, process};

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

#[cfg(test)]
mod test {
    use super::*;
    use rlox::astprinter;
    use rlox::parser;
    use rlox::scanner;

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
            parser::Stmt::Expression(expr) => {
                let ast_print_res = astprinter::_AstPrinter::_print_expression(&expr.expression);
                assert_eq!(ast_print_res, "(== (+ (+ 2 (* (/ 5 4) 2)) 4) (- 3))");
            }
            _ => panic!("Not Expression, error"),
        }
    }

    #[test]
    fn test_interpreter_test_lox_file() {
        let lox = Lox::new();
        lox.run_file("test.lox");
    }
}
