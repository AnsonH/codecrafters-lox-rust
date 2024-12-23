use std::{cell::RefCell, fmt::Display, rc::Rc};

use miette::Result;

use crate::{
    ast::statement::FunctionDeclaration,
    evaluator::{environment::Environment, object::Object, Evaluator},
};

use super::Function;

/// A user-defined function.
#[derive(Debug, Clone, PartialEq)]
pub struct UserFunction {
    declaration: FunctionDeclaration,
}

impl UserFunction {
    pub fn new(declaration: FunctionDeclaration) -> Self {
        Self { declaration }
    }

    #[inline]
    pub fn new_obj(declaration: FunctionDeclaration) -> Object {
        Object::Function(Function::User(Self::new(declaration)))
    }

    /// Entry point for calling a user-defined function.
    pub(super) fn call(&self, evaluator: &mut Evaluator, args: &[Object]) -> Result<Object> {
        let env = Rc::new(RefCell::new(Environment::from(&evaluator.env)));
        for (param, arg) in self.declaration.parameters.iter().zip(args.iter()) {
            env.borrow_mut().define(param.name.clone(), arg.clone());
        }
        evaluator.execute_block(&self.declaration.body.statements, env)?;
        Ok(Object::Nil) // TODO: Change to returned value of the function
    }

    /// Gets the number of arguments.
    #[inline]
    pub(super) fn arity(&self) -> usize {
        self.declaration.parameters.len()
    }
}

impl Display for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.declaration.name.name)
    }
}
