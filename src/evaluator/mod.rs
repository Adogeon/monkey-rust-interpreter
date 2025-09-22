use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

pub trait Evaluable {
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
            Expression::PreExp(prefix_expression) => {
                let right = eval(prefix_expression.right);
                eval_prefix_expression(prefix_expression.operator, right)
            }
            Expression::InExp(infix_expression) => todo!(),
            Expression::BoolLit(boolean) => Object::BOOLEAN(boolean.value),
            Expression::IfExp(if_expression) => todo!(),
            Expression::FncLit(function_literal) => todo!(),
            Expression::CallExp(call_expression) => todo!(),
        }
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        _ => Object::NULL,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::NULL => Object::BOOLEAN(true),
        Object::BOOLEAN(s) => Object::BOOLEAN(!s),
        _ => Object::BOOLEAN(false),
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
