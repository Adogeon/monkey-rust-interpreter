use crate::ast::{Identifier, Statement};
use environment::Env;
use std::fmt::{Display, Write};
use std::rc::Rc;

pub mod builtins;
pub mod environment;

pub fn new_error(msg: String) -> Object {
    Object::ERROR(msg)
}

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

type BuiltinFunction = fn(params: Vec<Object>) -> Object;

pub struct Builtin {
    pub function: BuiltinFunction,
}
impl PartialEq for Builtin {
    fn eq(&self, _other: &Self) -> bool {
        false
    }

    fn ne(&self, _other: &Self) -> bool {
        true
    }
}

#[derive(PartialEq, Eq, Hash)]
enum HashKey {
    INTEGER(i64),
    STRING(String),
    BOOLEAN(bool),
}

impl TryFrom<Object> for HashKey {
    type Error = ();
    fn try_from(value: Object) -> Result<Self, Self::Error> {
        match value {
            Object::INTEGER(val) => Ok(HashKey::INTEGER(val)),
            Object::STRING(val) => Ok(HashKey::STRING(val)),
            Object::BOOLEAN(val) => Ok(HashKey::BOOLEAN(val)),
            _ => Err(()),
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Object {
    INTEGER(i64),
    STRING(String),
    BOOLEAN(bool),
    RETURN(Box<Object>),
    FUNCTION(Rc<Function>),
    BUILTIN(Rc<Builtin>),
    ERROR(String),
    ARRAY(Vec<Object>),
    NULL,
}

impl Object {
    fn inspect(&self) -> String {
        match self {
            Self::INTEGER(val) => format!("{}", val),
            Self::STRING(val) => format!("{}", val),
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
            Self::BUILTIN(_) => String::from("Builtin function"),
            Self::ARRAY(val) => {
                let mut buffer = String::new();
                let el_list = val
                    .iter()
                    .map(|v| v.inspect())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(buffer, "[ {} ]", el_list).unwrap();
                buffer
            }
            Self::NULL => String::from("null"),
        }
    }

    pub fn ob_type(&self) -> &str {
        match self {
            Self::INTEGER(_) => "INTEGER",
            Self::STRING(_) => "STRING",
            Self::BOOLEAN(_) => "BOOLEAN",
            Self::RETURN(_) => "RETURN_OBJ",
            Self::ERROR(_) => "ERROR",
            Self::FUNCTION(_) => "FUNCTION",
            Self::BUILTIN(_) => "BUILTIN_FN",
            Self::ARRAY(_) => "ARRAY",
            Self::NULL => "NULL",
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inspect())
    }
}
