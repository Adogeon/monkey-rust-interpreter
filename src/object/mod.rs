use std::fmt::Display;

#[derive(PartialEq)]
pub enum Object {
    INTEGER(i64),
    BOOLEAN(bool),
    NULL,
}

impl Object {
    fn inspect(&self) -> String {
        match self {
            Self::INTEGER(val) => format!("{}", val),
            Self::BOOLEAN(val) => format!("{}", val),
            Self::NULL => String::from("null"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inspect())
    }
}
