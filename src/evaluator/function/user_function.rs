use std::{cell::RefCell, fmt::Display, ops::ControlFlow, rc::Rc};

use miette::Result;

use crate::{
    ast::statement::FunctionDeclaration,
    evaluator::{environment::Environment, object::Object, Evaluator},
};

use super::Function;

/// A user-defined function.
#[derive(Debug, Clone)]
pub struct UserFunction {
    declaration: FunctionDeclaration,
    closure: Rc<RefCell<Environment>>,
}

impl UserFunction {
    pub fn new(declaration: FunctionDeclaration, closure: Rc<RefCell<Environment>>) -> Self {
        Self {
            declaration,
            closure,
        }
    }

    #[inline]
    pub fn new_obj(declaration: FunctionDeclaration, closure: Rc<RefCell<Environment>>) -> Object {
        Object::Function(Function::User(Self::new(declaration, closure)))
    }

    /// Entry point for calling a user-defined function.
    pub(super) fn call(&self, evaluator: &mut Evaluator, args: &[Object]) -> Result<Object> {
        let env = Rc::new(RefCell::new(Environment::from(&self.closure)));

        for (param, arg) in self.declaration.parameters.iter().zip(args.iter()) {
            env.borrow_mut().define(param.name.clone(), arg.clone());
        }
        match evaluator.execute_block(&self.declaration.body.statements, env) {
            Ok(ControlFlow::Continue(())) => Ok(Object::Nil),
            Ok(ControlFlow::Break(value)) => Ok(value),
            Err(err) => Err(err),
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
