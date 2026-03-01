use std::{collections::HashMap, mem};

use crate::{
    interpreter::{Object, RuntimeError},
    scanner::{LiteralValue, Token},
};

pub struct Environment {
    values: HashMap<String, Object>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        let values = HashMap::new();
        let enclosing = None;
        Environment { values, enclosing }
    }

    pub fn swap_new_scoped_env(&mut self) {
        let new_env = Environment::new();
        let old_env = mem::replace(self, new_env);
        self.enclosing = Some(Box::new(old_env));
    }

    pub fn get_upper_env(&mut self) -> Environment {
        let upper_env = self.enclosing.take().expect(
            "Failed to get environment from upper scope, \
            this is likely a interpreter issue, please report this",
        );
        *upper_env
    }

    pub fn define(&mut self, name: &str, value: Object) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get<'token>(&self, name: &Token<'token>) -> Result<&Object, RuntimeError> {
        let var_name = name.lexeme;
        let value = self.values.get(var_name);
        match value {
            Some(value) => Ok(value),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(name),
                None => Err(RuntimeError::new(
                    name.line,
                    format!("Undefined variable '{}'", var_name),
                )),
            },
        }
    }

    pub fn assign<'token>(
        &mut self,
        name: &Token<'token>,
        value: Object,
    ) -> Result<(), RuntimeError> {
        let var_name = name.lexeme;
        match self.values.get_mut(var_name) {
            Some(value_ref) => {
                *value_ref = value;
                Ok(())
            }
            None => match &mut self.enclosing {
                Some(enclosing) => enclosing.assign(name, value),
                None => Err(RuntimeError::new(
                    name.line,
                    format!("Undefined variable '{}'", var_name),
                )),
            },
        }
    }
}
