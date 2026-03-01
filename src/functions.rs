use crate::{
    environment,
    parser::{Function, Stmt},
    scanner::{LiteralValue, Token},
};

pub struct LoxFunction<'source> {
    params: Vec<Token<'source>>,
    body: Vec<Stmt<'source>>,
    arity: usize,
}

impl<'source> LoxFunction<'source> {
    fn new(function: Function) -> LoxFunction {
        let params = function.params;
        let body = function.body;
        let arity = params.len();
        LoxFunction {
            params,
            body,
            arity,
        }
    }

    fn call(&mut self, arguments: Vec<LiteralValue>) {
        let mut environment = environment::Environment::new();
        for (param, argument) in self.params.iter().zip(arguments.iter()) {
            environment.define(param.lexeme, argument.clone());
        }
    }
}
