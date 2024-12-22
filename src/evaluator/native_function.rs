use std::{
    cell::RefCell,
    fmt::Display,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use miette::{IntoDiagnostic, Result};

use super::{environment::Environment, function::Function, object::Object, Evaluator};

/// A built-in (native) function.
#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    /// Number of arguments.
    pub arity: usize,
    name: NativeFunctionName,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeFunctionName {
    Clock,
}

/// Register native functions to the environment. To be called when an
/// evaluator is created.
pub(super) fn register_native_functions(env: Rc<RefCell<Environment>>) {
    env.borrow_mut().define(
        "clock".into(),
        NativeFunction::new_obj(0, NativeFunctionName::Clock),
    );
}

impl NativeFunction {
    /// Entry point for calling a native function.
    pub(super) fn call(&self, _evaluator: &mut Evaluator, _args: &[Object]) -> Result<Object> {
        use NativeFunctionName::*;
        match self.name {
            Clock => self.clock(),
        }
    }

    #[inline]
    fn new_obj(arity: usize, name: NativeFunctionName) -> Object {
        Object::Function(Function::Native(NativeFunction { arity, name }))
    }

    /// Gets the number of seconds since Unix Epoch.
    ///
    /// # Example
    /// ```txt, no_run
    /// print clock();  // 1734877960
    /// ```
    fn clock(&self) -> Result<Object> {
        let secs_since_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .into_diagnostic()?
            .as_secs();
        Ok(Object::Number(secs_since_epoch as f64))
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}
