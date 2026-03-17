use std::{collections::HashMap, mem};

use crate::{interpreter_new::RuntimeError, parser_new::Object};

pub struct Environment<'strings> {
    values: HashMap<String, Object<'strings>>,
    enclosing: Option<Box<Environment<'strings>>>,
}

impl<'strings> Environment<'strings> {
    pub fn new() -> Environment<'strings> {
        let values = HashMap::new();
        let enclosing = None;
        Environment { values, enclosing }
    }

    pub fn swap_new_scoped_env(&mut self) {
        let new_env = Environment::new();
        let old_env = mem::replace(self, new_env);
        self.enclosing = Some(Box::new(old_env));
    }

    pub fn get_upper_env(&mut self) -> Environment<'strings> {
        let upper_env = self.enclosing.take().expect(
            "Failed to get environment from upper scope, \
            this is likely a interpreter issue, please report this",
        );
        *upper_env
    }

    pub fn define(&mut self, name: &str, value: Object<'strings>) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<&Object<'strings>, RuntimeError> {
        let value = self.values.get(name);
        match value {
            Some(value) => Ok(value),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(name),
                None => Err(RuntimeError::new(
                    1, // TODO:
                    format!("Undefined variable '{}'", name),
                )),
            },
        }
    }

    pub fn assign(&mut self, var_name: &str, value: Object<'strings>) -> Result<(), RuntimeError> {
        match self.values.get_mut(var_name) {
            Some(value_ref) => {
                *value_ref = value;
                Ok(())
            }
            None => match &mut self.enclosing {
                Some(enclosing) => enclosing.assign(var_name, value),
                None => Err(RuntimeError::new(
                    5, // TODO:
                    format!("Undefined variable '{}'", var_name),
                )),
            },
        }
    }
}

impl<'strings> Default for Environment<'strings> {
    fn default() -> Environment<'strings> {
        Environment::new()
    }
}
