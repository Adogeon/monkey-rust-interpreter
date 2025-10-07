use crate::ast::{Expression, Identifier, Program, Statement};
use crate::object::environment::{new_enclosed_environment, Environment};
use crate::object::{Function, Object};
use std::cell::RefCell;
use std::rc::Rc;

const TRUE: Object = Object::BOOLEAN(true);
const FALSE: Object = Object::BOOLEAN(false);
const NULL: Object = Object::NULL;

fn new_error(msg: String) -> Object {
    Object::ERROR(msg)
}

fn is_error(input: &Object) -> bool {
    matches!(input, Object::ERROR(_))
}

pub trait Evaluable {
    fn eval(self: Box<Self>, env: &mut Environment) -> Object;
}

impl Evaluable for Program {
    fn eval(self: Box<Self>, env: &mut Environment) -> Object {
        let mut result = Object::NULL;
        for stmt in self.statements {
            result = eval(stmt, env);
            if let Object::RETURN(rv) = result {
                return *rv;
            } else if matches!(result, Object::ERROR(_)) {
                return result;
            }
        }
        return result;
    }
}

impl Evaluable for Expression {
    fn eval(self: Box<Self>, env: &mut Environment) -> Object {
        match *self {
            Expression::IntLit(int_lit) => Object::INTEGER(int_lit.value),
            Expression::Identifier(identifier) => match env.get(&identifier.value) {
                Some(val) => val,
                None => new_error(format!("identifier not found: {}", identifier.value)),
            },
            Expression::PreExp(prefix_expression) => {
                let right = eval(prefix_expression.right, env);
                if is_error(&right) {
                    return right;
                }
                eval_prefix_expression(prefix_expression.operator, right)
            }
            Expression::InExp(infix_expression) => {
                let right = eval(infix_expression.right, env);
                if is_error(&right) {
                    return right;
                }
                let left = eval(infix_expression.left, env);
                if is_error(&left) {
                    return left;
                }
                eval_infix_expression(infix_expression.operator, left, right)
            }
            Expression::BoolLit(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::IfExp(if_expression) => {
                let condition = eval(if_expression.condition, env);
                if is_error(&condition) {
                    return condition;
                }

                if is_truthy(condition) {
                    if_expression.consequence.eval(env)
                } else if let Some(alter) = if_expression.alternative {
                    alter.eval(env)
                } else {
                    NULL
                }
            }
            Expression::FncLit(function_literal) => {
                let params = function_literal
                    .parameters
                    .into_iter()
                    .map(|p| {
                        if let Expression::Identifier(id) = *p {
                            id
                        } else {
                            panic!("function parameters is not identifiers")
                        }
                    })
                    .collect();

                let body = if let Statement::BlcStmt(block) = *function_literal.body {
                    block
                } else {
                    panic!("Abort, wrong type of statement in function body")
                };

                let outer = Rc::new(RefCell::new(*env));

                Object::FUNCTION(Rc::new(Function {
                    parameters: params,
                    body,
                    env: new_enclosed_environment(outer),
                }))
            }
            Expression::CallExp(call_expression) => {
                let function = call_expression.function.eval(env);
                if is_error(&function) {
                    function
                }
                let args = eval_expressions(call_expression.arguments, env);
                if args.len() == 1 && is_error(&args[0]) {
                    args[0]
                }
            }
        }
    }
}

fn eval_expressions(arguments: Vec<Box<Expression>>, env: &mut Environment) -> Vec<Object> {
    let mut result: Vec<Object> = vec![];

    for exp in arguments {
        let val = eval(exp, env);
        if is_error(&val) {
            result.push(val);
            return result;
        }
        result.push(val);
    }

    result
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
    } else if left.ob_type() != right.ob_type() {
        new_error(format!(
            "type mismatch:{} {} {}",
            left.ob_type(),
            operator,
            right.ob_type()
        ))
    } else {
        new_error(format!(
            "unknown operator:{} {} {}",
            left.ob_type(),
            operator,
            right.ob_type()
        ))
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
        _ => new_error(format!(
            "unknown operator:{} {} {}",
            left.ob_type(),
            operator,
            right.ob_type(),
        )),
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
        _ => new_error(format!("unknown operator:{}{}", operator, right.ob_type())),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::INTEGER(s) = right {
        Object::INTEGER(-s)
    } else {
        new_error(format!("unknown operator:-{}", right.ob_type()))
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
    fn eval(self: Box<Self>, env: &mut Environment) -> Object {
        match *self {
            Statement::ExpStmt(exp_stmt) => exp_stmt.expression.eval(env),
            Statement::LetStmt(let_statement) => {
                let value = let_statement.value.eval(env);
                if is_error(&value) {
                    return value;
                }
                env.set(let_statement.name.value, &value);
                value
            }
            Statement::RetStmt(return_statement) => {
                let value = return_statement.return_value.eval(env);
                if is_error(&value) {
                    return value;
                }
                Object::RETURN(Box::new(value))
            }
            Statement::BlcStmt(block_statement) => {
                let mut result: Object = Object::NULL;
                for stmt in block_statement.statements {
                    result = eval(stmt, env);
                    if !matches!(result, Object::NULL) {
                        if matches!(result, Object::RETURN(_)) || matches!(result, Object::ERROR(_))
                        {
                            return result;
                        }
                    }
                }
                return result;
            }
        }
    }
}

pub fn eval(node: Box<dyn Evaluable>, env: &mut Environment) -> Object {
    node.eval(env)
}

#[cfg(test)]
mod test;
