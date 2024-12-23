use miette::Result;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{ast::expression::Identifier, error::RuntimeError};

use super::object::Object;

/// Environment keeps track of identifiers and their values. It represents a
/// lexical scope.
#[derive(Debug)]
pub struct Environment {
    /// The parent environment that encloses this environment (if any).
    enclosing: Option<Rc<RefCell<Environment>>>,
    /// Variables of this current scope.
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    /// Creates a new environment that has a parent of `enclosing`.
    pub fn from(enclosing: &Rc<RefCell<Environment>>) -> Self {
        Self {
            enclosing: Some(Rc::clone(enclosing)),
            values: HashMap::new(),
        }
    }

    /// Creates a variable with a value.
    ///
    /// - If the variable already exists in the **current** scope, its value will
    ///   be overwritten.
    /// - When the variable has the same name as a variable in an enclosing
    ///   scope, it shadows the outer one.
    #[inline]
    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    /// Assigns a value to an existing variable.
    #[allow(clippy::map_entry)]
    pub fn assign(&mut self, ident: &Identifier, value: Object) -> Result<()> {
        let name = ident.name.to_string();

        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            // Recursively walks up the scope chain
            enclosing.borrow_mut().assign(ident, value)
        } else {
            Err(RuntimeError::UndefinedVariable {
                name,
                span: ident.span,
            }
            .into())
        }
    }

    /// Gets the value of an identifier.
    pub fn get(&self, ident: &Identifier) -> Result<Object> {
        if let Some(value) = self.values.get(&ident.name) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(ident)
        } else {
            Err(RuntimeError::UndefinedVariable {
                name: ident.name.clone(),
                span: ident.span,
            }
            .into())
        }
    }
}
