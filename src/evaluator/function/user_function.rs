use std::{cell::RefCell, fmt::Display, rc::Rc};

use miette::{Diagnostic, Result};
use thiserror::Error;

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

/// The return value of a user-defined function.
///
/// # Implementation Hack
///
/// It is an `Error` because we can abuse the `?` operator to halt function
/// execution and bubble up the return value back to `UserFunction::call()`.
/// See [10.5.1 - Returning from calls](https://craftinginterpreters.com/functions.html#returning-from-calls)
#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
#[error("")]
pub struct FunctionReturn(pub Object);

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

        match evaluator.execute_block(&self.declaration.body.statements, env) {
            Err(report) => match report.downcast_ref::<FunctionReturn>() {
                Some(FunctionReturn(return_value)) => {
                    // dbg!(&return_value);
                    Ok(return_value.clone())
                }
                None => Err(report), // bubble up runtime error
            },
            Ok(()) => Ok(Object::Nil),
        }
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
