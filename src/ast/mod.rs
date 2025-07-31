use crate::token::Token;
use std::boxed::Box;

pub trait Node {
    fn token_literal(&self) -> Option<&str>;
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
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

impl Node for Identifier {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.idt_token.Literal)
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

pub struct LetStatement {
    pub stmt_token: Token,
    pub name: Box<Identifier>,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.Literal)
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}
