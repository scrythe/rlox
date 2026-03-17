use std::{collections::HashMap, marker::PhantomData, mem};

use crate::{
    environment::{self, Environment},
    interpreter::RuntimeError,
    parser_new::Object,
    scanner::{LiteralValue, Token},
};

pub struct Environments {
    environments: Vec<Environment>,
    value_strings: Vec<String>,
}

impl Environments {
    pub fn new() -> Environment {
        let value_strings = Vec::new();
        let environment = Vec::new();
        Environment {
            value_strings,
            environments,
        }
    }
}
