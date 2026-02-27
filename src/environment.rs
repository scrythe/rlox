use std::collections::HashMap;

use crate::scanner::{LiteralValue, Token};

pub struct Environment<'var_names> {
    values: HashMap<&'var_names str, LiteralValue>,
}

impl<'var_names> Environment<'var_names> {
    pub fn new() -> Environment<'var_names> {
        let values = HashMap::new();
        Environment { values }
    }

    pub fn define(&mut self, name: &'var_names str, value: LiteralValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: Token) -> &LiteralValue {
        self.values.get(name.lexeme).unwrap()
    }
}
