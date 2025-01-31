use std::{cell::RefCell, ops::ControlFlow, rc::Rc};

use miette::Result;

use crate::ast::statement::*;

use super::{
    environment::Environment, function::user_function::UserFunction, object::Object, Evaluator,
};

impl StmtVisitor for Evaluator {
    /// We use [ControlFlow] to handle return statements from function calls
    /// via `Ok(ControlFlow::Break(value))`.
    ///
    /// We didn't use the [original method](https://craftinginterpreters.com/functions.html#returning-from-calls)
    /// of "throwing" the return value with `Err(value)` and bubble it up to
    /// `UserFunction::call()`. This is because this evaluator returns [miette::Result],
    /// so the value must be casted to a [miette::Report]. This has a huge overhead,
    /// especially in recursive function calls.
    type Value = Result<ControlFlow<Object>>;

    fn visit_block_stmt(&mut self, stmt: &BlockStatement) -> Self::Value {
        let env = Rc::new(RefCell::new(Environment::from(&self.env)));
        self.execute_block(&stmt.statements, env)
    }

    fn visit_expression_stmt(&mut self, stmt: &ExpressionStatement) -> Self::Value {
        stmt.expression.accept(self)?;
        Ok(ControlFlow::Continue(()))
    }

    /// The [ForStatement] is de-sugared during evaluation in the following way:
    ///
    /// Before: `for ( <initializer> ; <condition> ; <update> ) <body statement>`
    ///
    /// After:
    /// ```txt, no_run
    /// {
    ///     <initializer>;
    ///     while (<condition>) {
    ///         <body statement>
    ///         <update>;
    ///     }
    /// }
    /// ```
    fn visit_for_stmt(&mut self, stmt: &ForStatement) -> Self::Value {
        let env_previous = Rc::clone(&self.env);

        // Temporarily set to new environment
        let env_new = Environment::from(&self.env);
        self.env = Rc::new(RefCell::new(env_new));

        // Note: We use a closure to ensure the old environment is restored
        // before any error is propagated
        let mut evaluate_for_loop = || -> Result<ControlFlow<Object>> {
            match &stmt.init {
                Some(ForStatementInit::ExpressionStatement(s)) => self.visit_expression_stmt(s)?,
                Some(ForStatementInit::VarStatement(s)) => self.visit_var_stmt(s)?,
                _ => ControlFlow::Continue(()),
            };

            while stmt.condition.as_ref().map_or(Ok(true), |expr| {
                expr.accept(self).map(|obj| obj.is_truthy())
            })? {
                let control_flow = stmt.body.accept(self)?;
                if control_flow.is_break() {
                    return Ok(control_flow);
                }

                if let Some(update) = &stmt.update {
                    update.accept(self)?;
                }
            }

            Ok(ControlFlow::Continue(()))
        };
        let result = evaluate_for_loop();

        self.env = env_previous; // Restore back to old environment
        result
    }

    fn visit_function_declaration(&mut self, stmt: &FunctionDeclaration) -> Self::Value {
        let closure_env = Rc::clone(&self.env);
        let function = UserFunction::new_obj(stmt.clone(), closure_env);
        self.env
            .borrow_mut()
            .define(stmt.name.name.clone(), function);
        Ok(ControlFlow::Continue(()))
    }

    fn visit_if_stmt(&mut self, stmt: &IfStatement) -> Self::Value {
        if stmt.condition.accept(self)?.is_truthy() {
            Ok(stmt.then_branch.accept(self)?)
        } else if let Some(else_branch) = &stmt.else_branch {
            Ok(else_branch.accept(self)?)
        } else {
            Ok(ControlFlow::Continue(()))
        }
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStatement) -> Self::Value {
        println!("{}", stmt.expression.accept(self)?);
        Ok(ControlFlow::Continue(()))
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStatement) -> Self::Value {
        let value = stmt
            .expression
            .as_ref()
            .map_or(Ok(Object::Nil), |expr| expr.accept(self))?;
        Ok(ControlFlow::Break(value))
    }

    fn visit_var_stmt(&mut self, stmt: &VarStatement) -> Self::Value {
        let value = stmt
            .initializer
            .as_ref()
            .map_or(Ok(Object::Nil), |expr| expr.accept(self))?;

        self.env.borrow_mut().define(stmt.ident.name.clone(), value);
        Ok(ControlFlow::Continue(()))
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStatement) -> Self::Value {
        while stmt.condition.accept(self)?.is_truthy() {
            let control_flow = stmt.body.accept(self)?;
            if control_flow.is_break() {
                return Ok(control_flow);
            }
        }
        Ok(ControlFlow::Continue(()))
    }
}
