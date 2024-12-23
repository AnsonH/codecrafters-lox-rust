use std::{fmt::Display, rc::Rc};

use miette::Result;

use crate::{
    ast::statement::FunctionDeclaration,
    evaluator::{object::Object, Evaluator},
};

/// A user-defined function.
#[derive(Debug, Clone, PartialEq)]
pub struct UserFunction {
    // declaration: Rc<FunctionDeclaration>,
}

impl UserFunction {
    /// Entry point for calling a user-defined function.
    pub(super) fn call(&self, _evaluator: &mut Evaluator, _args: &[Object]) -> Result<Object> {
        todo!()
    }
}

impl Display for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
