use crate::ast::{Identifier, Statement};
use environment::Env;
use std::fmt::{Display, Write};
use std::rc::Rc;

pub mod environment;

pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: Rc<Statement>,
    pub env: Env,
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }

    fn ne(&self, _other: &Self) -> bool {
        true
    }
}

#[derive(PartialEq, Clone)]
pub enum Object {
    INTEGER(i64),
    BOOLEAN(bool),
    RETURN(Box<Object>),
    FUNCTION(Rc<Function>),
    ERROR(String),
    NULL,
}

impl Object {
    fn inspect(&self) -> String {
        match self {
            Self::INTEGER(val) => format!("{}", val),
            Self::BOOLEAN(val) => format!("{}", val),
            Self::RETURN(val) => format!("{}", val.inspect()),
            Self::ERROR(val) => format!("{}", val),
            Self::FUNCTION(val) => {
                let mut buffer = String::new();
                let param_list = val
                    .parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(buffer, "( {} )", param_list).unwrap();
                write!(buffer, "{{/n {} /n}}", val.body.to_string()).unwrap();
                buffer
            }
            Self::NULL => String::from("null"),
        }
    }

    pub fn ob_type(&self) -> &str {
        match self {
            Self::INTEGER(_) => "INTEGER",
            Self::BOOLEAN(_) => "BOOLEAN",
            Self::RETURN(_) => "RETURN_OBJ",
            Self::ERROR(_) => "ERROR",
            Self::FUNCTION(_) => "FUNCTION",
            Self::NULL => "NULL",
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.ob_type(), self.inspect())
    }
}
