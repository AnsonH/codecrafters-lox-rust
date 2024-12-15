use miette::Result;
use std::collections::HashMap;

use crate::{ast::expression::Identifier, error::RuntimeError};

use super::object::Object;

/// Environment keeps track of identifiers and their values.
#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    /// Defines a variable with a value. If the variable already exists, its value
    /// will be overwritten.
    #[inline]
    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    pub fn get(&self, ident: &Identifier) -> Result<&Object> {
        if let Some(value) = self.values.get(ident.name) {
            Ok(value)
        } else {
            Err(RuntimeError::UndefinedVariable {
                name: ident.name.into(),
                span: ident.span,
            }
            .into())
        }
    }
}
