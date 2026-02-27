use std::collections::HashMap;

use crate::{
    interpreter::RuntimeError,
    scanner::{LiteralValue, Token},
};

pub struct Environment {
    values: HashMap<String, LiteralValue>,
}

impl Environment {
    pub fn new() -> Environment {
        let values = HashMap::new();
        Environment { values }
    }

    pub fn define(&mut self, name: &str, value: LiteralValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get<'token>(&self, name: Token<'token>) -> Result<&LiteralValue, RuntimeError<'token>> {
        let var_name = name.lexeme;
        self.values.get(var_name).ok_or(RuntimeError::new(
            name,
            format!("Undefined variable '{}'", var_name),
        ))
    }

    pub fn assign<'token>(
        &mut self,
        name: Token<'token>,
        value: LiteralValue,
    ) -> Result<(), RuntimeError<'token>> {
        // let var_name = name.lexeme;
        // match self.values.insert(var_name.to_string(), value) {
        //     Some(_) => Ok(()),
        //     None => Err(RuntimeError::new(
        //         name,
        //         format!("Undefined variable '{}'", var_name),
        //     )),
        // }
        let var_name = name.lexeme;
        match self.values.get_mut(var_name) {
            Some(value_ref) => {
                *value_ref = value;
                Ok(())
            }
            None => Err(RuntimeError::new(
                name,
                format!("Undefined variable '{}'", var_name),
            )),
        }
    }
}
