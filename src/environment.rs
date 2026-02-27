use std::collections::HashMap;

use crate::scanner::{LiteralValue, Token};

pub struct Environment<'a> {
    values: HashMap<&'a str, LiteralValue>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        let values = HashMap::new();
        Environment { values }
    }

    pub fn define(&mut self, name: &'a str, value: LiteralValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: Token) -> &LiteralValue {
        self.values.get(name.lexeme).unwrap()
    }
}
