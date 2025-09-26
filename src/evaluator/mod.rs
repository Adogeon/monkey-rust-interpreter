use crate::ast::{BlockStatement, Expression, IfExpression, Program, Statement};
use crate::object::Object;

const TRUE: Object = Object::BOOLEAN(true);
const FALSE: Object = Object::BOOLEAN(false);
const NULL: Object = Object::NULL;

pub trait Evaluable {
    fn eval(self: Box<Self>) -> Object;
}

impl Evaluable for Program {
    fn eval(self: Box<Self>) -> Object {
        eval_statements(self.statements)
    }
}

impl Evaluable for BlockStatement {
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
            Expression::InExp(infix_expression) => {
                let right = eval(infix_expression.right);
                let left = eval(infix_expression.left);
                eval_infix_expression(infix_expression.operator, left, right)
            }
            Expression::BoolLit(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::IfExp(if_expression) => eval_if_expression(if_expression),
            Expression::FncLit(function_literal) => todo!(),
            Expression::CallExp(call_expression) => todo!(),
        }
    }
}

fn eval_if_expression(if_expression: IfExpression) -> Object {
    let condition = eval(if_expression.condition);
    if is_truthy(condition) {
        Box::new(if_expression.consequence).eval()
    } else if let Some(alter) = if_expression.alternative {
        Box::new(alter).eval()
    } else {
        NULL
    }
}

fn is_truthy(condition: Object) -> bool {
    match condition {
        NULL | FALSE => false,
        _ => true,
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    if matches!(left, Object::INTEGER(_)) && matches!(right, Object::INTEGER(_)) {
        eval_integer_infix_expression(operator, left, right)
    } else if operator.as_str() == "==" {
        native_bool_to_boolean_object(left == right)
    } else if operator.as_str() == "!=" {
        native_bool_to_boolean_object(left != right)
    } else {
        NULL
    }
}

fn eval_integer_infix_expression(operator: String, left: Object, right: Object) -> Object {
    let left_value = match left {
        Object::INTEGER(s) => s,
        _ => 0,
    };

    let right_value = match right {
        Object::INTEGER(s) => s,
        _ => 0,
    };

    match operator.as_str() {
        "+" => Object::INTEGER(left_value + right_value),
        "-" => Object::INTEGER(left_value - right_value),
        "*" => Object::INTEGER(left_value * right_value),
        "/" => Object::INTEGER(left_value / right_value),
        "<" => native_bool_to_boolean_object(left_value < right_value),
        ">" => native_bool_to_boolean_object(left_value > right_value),
        "==" => native_bool_to_boolean_object(left_value == right_value),
        "!=" => native_bool_to_boolean_object(left_value != right_value),
        _ => NULL,
    }
}

fn native_bool_to_boolean_object(value: bool) -> Object {
    if value {
        TRUE
    } else {
        FALSE
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::NULL,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::INTEGER(s) = right {
        Object::INTEGER(-s)
    } else {
        NULL
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

impl Evaluable for Statement {
    fn eval(self: Box<Self>) -> Object {
        match *self {
            Statement::ExpStmt(exp_stmt) => Box::new(exp_stmt.expression).eval(),
            Statement::LetStmt(let_statement) => todo!(),
            Statement::RetStmt(return_statement) => todo!(),
            Statement::BlcStmt(block_statement) => Box::new(block_statement).eval(),
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
