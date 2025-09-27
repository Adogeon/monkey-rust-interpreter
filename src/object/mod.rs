use std::fmt::Display;

#[derive(PartialEq)]
pub enum Object {
    INTEGER(i64),
    BOOLEAN(bool),
    RETURN(Box<Object>),
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
            Self::NULL => String::from("null"),
        }
    }

    pub fn ob_type(&self) -> &str {
        match self {
            Self::INTEGER(_) => "INTEGER",
            Self::BOOLEAN(_) => "BOOLEAN",
            Self::RETURN(_) => "RETURN_OBJ",
            Self::ERROR(_) => "ERROR",
            Self::NULL => "NULL",
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.ob_type(), self.inspect())
    }
}
