use std::fmt::Display;

use miette::Result;

use super::{native_function::NativeFunction, object::Object, Evaluator};

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    Native(NativeFunction),
}

impl Function {
    /// Executes the function.
    pub(super) fn call(&self, evaluator: &mut Evaluator, args: &[Object]) -> Result<Object> {
        match self {
            Function::Native(function) => function.call(evaluator, args),
        }
    }

    /// Gets the number of arguments.
    pub(super) fn arity(&self) -> usize {
        match self {
            Function::Native(function) => function.arity,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(function) => write!(f, "{function}"),
        }
    }
}
