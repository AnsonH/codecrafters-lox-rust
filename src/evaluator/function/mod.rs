pub(super) mod native_function;
pub(super) mod user_function;

pub use self::native_function::register_native_functions;

pub(super) use self::native_function::NativeFunction;
pub(super) use self::user_function::UserFunction;

/////////////////////////////////

use std::fmt::Display;

use miette::Result;

use super::{object::Object, Evaluator};

#[derive(Debug, Clone)]
pub enum Function {
    Native(NativeFunction),
    User(UserFunction),
}

impl Function {
    /// Executes the function.
    pub(super) fn call(&self, evaluator: &mut Evaluator, args: &[Object]) -> Result<Object> {
        match self {
            Function::Native(function) => function.call(evaluator, args),
            Function::User(function) => function.call(evaluator, args),
        }
    }

    /// Gets the number of arguments.
    pub(super) fn arity(&self) -> usize {
        match self {
            Function::Native(function) => function.arity,
            Function::User(function) => function.arity(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(function) => write!(f, "{function}"),
            Function::User(function) => write!(f, "{function}"),
        }
    }
}
