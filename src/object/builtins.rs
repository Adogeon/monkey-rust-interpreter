use std::rc::Rc;

use crate::object::new_error;

use super::{Builtin, Object};

pub fn builtins_fn(input: &str) -> Object {
    match input {
        "len" => Object::BUILTIN(Rc::new(Builtin {
            function: |parameter| {
                if parameter.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want 1",
                        parameter.len()
                    ));
                };

                if let Object::STRING(str) = parameter[0].clone() {
                    Object::INTEGER(str.len() as i64)
                } else {
                    new_error(format!(
                        "argument to `len` not supported, got={}",
                        parameter[0].ob_type()
                    ))
                }
            },
        })),
        _ => Object::NULL,
    }
}
