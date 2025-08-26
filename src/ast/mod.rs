use crate::token::Token;
use std::boxed::Box;

pub trait Node {
    fn token_literal(&self) -> Option<&str>;
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct EmptyValue;

impl Node for EmptyValue {
    fn token_literal(&self) -> Option<&str> {
        Some("it's empty")
    }
}

impl Expression for EmptyValue {
    fn expression_node(&self) {}
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<StatementEnum>>,
}

impl Node for Program {
    fn token_literal(&self) -> Option<&str> {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            None
        }
    }
}

pub struct Identifier {
    pub idt_token: Token,
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

impl Node for Identifier {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.idt_token.tok_literal)
    }
}

pub struct LetStatement {
    pub stmt_token: Token,
    pub name: Box<Identifier>,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

pub struct ReturnStatement {
    pub stmt_token: Token,
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

pub enum StatementEnum {
    LetStmt(LetStatement),
    RetStmt(ReturnStatement),
}

impl Node for StatementEnum {
    fn token_literal(&self) -> Option<&str> {
        match self {
            Self::LetStmt(lst) => lst.token_literal(),
            Self::RetStmt(rst) => rst.token_literal(),
        }
    }
}

impl Statement for StatementEnum {
    fn statement_node(&self) {
        match self {
            Self::LetStmt(lst) => lst.statement_node(),
            Self::RetStmt(rst) => rst.statement_node(),
        }
    }
}

pub enum ParseError {
    UnexpectedToken,
}
