use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

trait Evaluable {
    fn eval(self: Box<Self>) -> Object;
}

impl Evaluable for Program {
    fn eval(self: Box<Self>) -> Object {
        eval_statements(self.statements)
    }
}

impl Evaluable for Expression {
    fn eval(self: Box<Self>) -> Object {
        match *self {
            Expression::IntLit(int_lit) => Object::INTEGER(int_lit.value),
            Expression::Identifier(identifier) => todo!(),
            Expression::PreExp(prefix_expression) => todo!(),
            Expression::InExp(infix_expression) => todo!(),
            Expression::BoolLit(boolean) => todo!(),
            Expression::IfExp(if_expression) => todo!(),
            Expression::FncLit(function_literal) => todo!(),
            Expression::CallExp(call_expression) => todo!(),
        }
    }
}

impl Evaluable for Statement {
    fn eval(self: Box<Self>) -> Object {
        match *self {
            Statement::ExpStmt(exp_stmt) => Box::new(exp_stmt.expression).eval(),
            Statement::LetStmt(let_statement) => todo!(),
            Statement::RetStmt(return_statement) => todo!(),
            Statement::BlcStmt(block_statement) => todo!(),
        }
    }
}

pub fn eval(node: Box<dyn Evaluable>) -> Object {
    node.eval()
}

fn eval_statements(statements: Vec<Statement>) -> Object {
    let mut result: Object = Object::NULL;
    for stmt in statements {
        result = Box::new(stmt).eval();
    }
    return result;
}

#[cfg(test)]
mod test;
