use crate::token::Token;
use std::fmt::Display;

pub trait Node: Display {
    fn token_literal(&self) -> Option<&str>;
}

pub struct EmptyValue;

impl Node for EmptyValue {
    fn token_literal(&self) -> Option<&str> {
        Some("it's empty")
    }
}

impl Display for EmptyValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "It's blank")
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
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

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

pub struct LetStatement {
    pub stmt_token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = ", self.token_literal().unwrap(), self.name)?;
        if let Some(stmt_value) = &self.value {
            write!(f, "{stmt_value}")?;
        }
        write!(f, ";")
    }
}

pub struct ReturnStatement {
    pub stmt_token: Token,
    pub return_value: Option<Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.token_literal().unwrap())?;
        if let Some(stmt_value) = &self.return_value {
            write!(f, "{stmt_value}")?;
        }
        write!(f, ";")
    }
}

pub struct ExpressionStatement {
    pub stmt_token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ExpressionStatement still cooking")
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
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LetStmt(lst) => write!(f, "{}", lst),
            Self::RetStmt(rst) => write!(f, "{}", rst),
            Self::ExpStmt(est) => write!(f, "{}", est),
        }
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: u64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.tok_literal)
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.tok_literal)
    }
}

pub enum Expression {
    Identifier(Identifier),
    IntLit(IntegerLiteral),
    PreExp(PrefixExpression),
    InExp(InfixExpression),
    BoolLit(Boolean),
}

impl Node for Expression {
    fn token_literal(&self) -> Option<&str> {
        match self {
            Self::Identifier(ident) => ident.token_literal(),
            Self::IntLit(int_lit) => int_lit.token_literal(),
            Self::PreExp(pe) => pe.token_literal(),
            Self::InExp(ie) => ie.token_literal(),
            Self::BoolLit(bl) => bl.token_literal(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "{id}"),
            Self::IntLit(int_lit) => write!(f, "{int_lit}"),
            Self::PreExp(pe) => write!(f, "{pe}"),
            Self::InExp(ie) => write!(f, "{ie}"),
            Self::BoolLit(bl) => write!(f, "{bl}"),
        }
    }
}

#[cfg(test)]
mod test;
