use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

const TRUE: Object = Object::BOOLEAN(true);
const FALSE: Object = Object::BOOLEAN(false);
const NULL: Object = Object::NULL;

pub trait Evaluable {
    fn eval(self: Box<Self>) -> Object;
}

impl Evaluable for Program {
    fn eval(self: Box<Self>) -> Object {
        let mut result = Object::NULL;
        for stmt in self.statements {
            result = eval(stmt);
            if let Object::RETURN(rv) = result {
                return *rv;
            }
        }
        return result;
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
            Expression::IfExp(if_expression) => {
                let condition = eval(if_expression.condition);
                if is_truthy(condition) {
                    if_expression.consequence.eval()
                } else if let Some(alter) = if_expression.alternative {
                    alter.eval()
                } else {
                    NULL
                }
            }
            Expression::FncLit(function_literal) => todo!(),
            Expression::CallExp(call_expression) => todo!(),
        }
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
            Statement::ExpStmt(exp_stmt) => exp_stmt.expression.eval(),
            Statement::LetStmt(let_statement) => todo!(),
            Statement::RetStmt(return_statement) => {
                let value = return_statement.return_value.eval();
                Object::RETURN(Box::new(value))
            }
            Statement::BlcStmt(block_statement) => {
                let mut result: Object = Object::NULL;
                for stmt in block_statement.statements {
                    result = eval(stmt);
                    if matches!(result, Object::RETURN(_)) && !matches!(result, Object::NULL) {
                        return result;
                    }
                }
                return result;
            }
        }
    }
}

pub fn eval(node: Box<dyn Evaluable>) -> Object {
    node.eval()
}

#[cfg(test)]
mod test;
