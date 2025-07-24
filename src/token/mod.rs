use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum T_type {
    ILLEGAL,
    EOF,
    // Identifiers + Literals
    IDENT,
    INT,
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOTEQ,
    //DELIMITERS
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    //KEYWORD
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

pub fn look_up_ident(ident: &String) -> T_type {
    match ident.as_str() {
        "fn" => T_type::FUNCTION,
        "let" => T_type::LET,
        "true" => T_type::TRUE,
        "false" => T_type::FALSE,
        "if" => T_type::IF,
        "else" => T_type::ELSE,
        "return" => T_type::RETURN,
        _ => T_type::IDENT,
    }
}

#[derive(Debug)]
pub struct Token {
    pub Type: T_type,
    pub Literal: String,
}

impl Token {
    pub fn new(ttype: T_type, literal: &str) -> Token {
        Token {
            Type: ttype,
            Literal: literal.to_string(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(Type:{:?}, Literal:{}", self.Type, self.Literal)
    }
}
