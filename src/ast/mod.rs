use crate::token::Token;
use std::boxed::Box;
use std::fmt::Write;

pub trait Node {
    fn token_literal(&self) -> Option<&str>;
    fn write_string(&self) -> String;
}

pub struct EmptyValue;

impl Node for EmptyValue {
    fn token_literal(&self) -> Option<&str> {
        Some("it's empty")
    }

    fn write_string(&self) -> String {
        String::from("<blank>")
    }
}

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> Option<&str> {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            None
        }
    }

    fn write_string(&self) -> String {
        let mut buf = String::new();
        for s in &self.statements {
            write!(&mut buf, "{}", s.write_string()).unwrap();
        }
        buf
    }
}

pub struct Identifier {
    pub idt_token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.idt_token.tok_literal)
    }

    fn write_string(&self) -> String {
        self.value.clone()
    }
}

pub struct LetStatement {
    pub stmt_token: Token,
    pub name: Box<Identifier>,
    pub value: Option<Box<Expression>>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }

    fn write_string(&self) -> String {
        let mut buf = String::new();

        write!(&mut buf, "{} ", self.token_literal().unwrap());
        write!(&mut buf, "{}", self.name.write_string());
        write!(&mut buf, " = ");

        if let Some(stmt_value) = &self.value {
            write!(&mut buf, "{}", stmt_value.write_string());
        }
        write!(&mut buf, ";");
        buf
    }
}

pub struct ReturnStatement {
    pub stmt_token: Token,
    pub return_value: Option<Box<Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }

    fn write_string(&self) -> String {
        let mut buf = String::new();
        write!(&mut buf, "{} ", self.token_literal().unwrap());
        if let Some(stmt_value) = &self.return_value {
            write!(&mut buf, "{}", stmt_value.write_string());
        }
        write!(&mut buf, ";");
        buf
    }
}

pub struct ExpressionStatement {
    pub stmt_token: Token,
    pub expression: Box<Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
    fn write_string(&self) -> String {
        String::from("ExpressionStatement still cooking")
    }
}

pub enum Statement {
    LetStmt(LetStatement),
    RetStmt(ReturnStatement),
    ExpStmt(ExpressionStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> Option<&str> {
        match self {
            Self::LetStmt(lst) => lst.token_literal(),
            Self::RetStmt(rst) => rst.token_literal(),
            Self::ExpStmt(est) => est.token_literal(),
        }
    }

    fn write_string(&self) -> String {
        match self {
            Self::LetStmt(lst) => lst.write_string(),
            Self::RetStmt(rst) => rst.write_string(),
            Self::ExpStmt(est) => est.write_string(),
        }
    }
}

pub enum Expression {
    Identifier(Identifier),
}

impl Node for Expression {
    fn token_literal(&self) -> Option<&str> {
        match self {
            Self::Identifier(ident) => ident.token_literal(),
        }
    }

    fn write_string(&self) -> String {
        match self {
            Self::Identifier(ident) => ident.write_string(),
        }
    }
}

pub enum ParseError {
    UnexpectedToken,
}

#[cfg(test)]
mod test;
