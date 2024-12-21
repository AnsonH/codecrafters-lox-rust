use std::{cell::RefCell, rc::Rc};

use miette::Result;

use crate::ast::statement::*;

use super::{environment::Environment, object::Object, Evaluator};

impl StmtVisitor for Evaluator {
    type Value = Result<()>;

    fn visit_block_stmt(&mut self, expr: &BlockStatement) -> Self::Value {
        let env = Environment::from(&self.env);
        self.execute_block(&expr.statements, Rc::new(RefCell::new(env)))?;
        Ok(())
    }

    fn visit_expression_stmt(&mut self, expr: &ExpressionStatement) -> Self::Value {
        expr.expression.accept(self)?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, expr: &IfStatement) -> Self::Value {
        todo!()
    }

    fn visit_print_stmt(&mut self, expr: &PrintStatement) -> Self::Value {
        println!("{}", expr.expression.accept(self)?);
        Ok(())
    }

    fn visit_var_stmt(&mut self, expr: &VarStatement) -> Self::Value {
        let value = expr
            .initializer
            .as_ref()
            .map_or(Ok(Object::Nil), |expr| expr.accept(self))?;

        self.env.borrow_mut().define(expr.ident.name.into(), value);
        Ok(())
    }
}

// TODO: Replace unit tests with integration tests
#[cfg(test)]
mod tests {
    use crate::{ast::expression::Identifier, parser::Parser};

    use super::*;
    use pretty_assertions::assert_eq;

    fn eval(input: &'static str) -> Result<Evaluator> {
        let parser = Parser::new(input);
        let program = parser.parse_program().unwrap_or_else(|report| {
            panic!(
                "Encountered error while parsing\n{:?}",
                report.with_source_code(input)
            )
        });
        let mut evaluator = Evaluator::default();
        evaluator.evaluate_program(&program)?;
        Ok(evaluator)
    }

    /// Asserts the value of a variable in the environment.
    fn assert_env_val(result: &Result<Evaluator>, ident: &str, expected: &Object) {
        // Create dummy `Identifier` since `Environment::get()` accepts `&Identifier`
        let ident = Identifier {
            name: ident,
            span: (0, 0).into(),
        };
        let env = result.as_ref().expect("no RuntimeError").env.borrow();
        match env.get(&ident) {
            Ok(actual) => assert_eq!(*expected, actual),
            Err(err) => panic!("Encountered error\n{:?}", err),
        }
    }

    #[test]
    fn test_var_statement() {
        let result = eval("var foo = 10 + 20; var bar;");
        assert_env_val(&result, "foo", &Object::Number(30.0));
        assert_env_val(&result, "bar", &Object::Nil);

        // Assignment
        let result = eval(
            "
            var a = 10;
            var b = 20;
            var c = 30;
            a = b = c + 1;",
        );
        assert_env_val(&result, "a", &Object::Number(31.0));
        assert_env_val(&result, "b", &Object::Number(31.0));
        assert_env_val(&result, "c", &Object::Number(30.0));
    }
}
