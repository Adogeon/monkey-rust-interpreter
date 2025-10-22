use crate::token::Token;
use std::fmt::Display;
use std::rc::Rc;

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
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = ", self.token_literal().unwrap(), self.name)?;
        write!(f, "{}", self.value)?;

        write!(f, ";")
    }
}

pub struct ReturnStatement {
    pub stmt_token: Token,
    pub return_value: Box<Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.stmt_token.tok_literal)
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.token_literal().unwrap())?;
        write!(f, "{}", self.return_value)?;
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
        write!(f, "{}", self.expression)
    }
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{stmt}")?;
        }

        Ok(())
    }
}

pub enum Statement {
    LetStmt(LetStatement),
    RetStmt(ReturnStatement),
    ExpStmt(ExpressionStatement),
    BlcStmt(BlockStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> Option<&str> {
        match self {
            Self::LetStmt(lst) => lst.token_literal(),
            Self::RetStmt(rst) => rst.token_literal(),
            Self::ExpStmt(est) => est.token_literal(),
            Self::BlcStmt(bst) => bst.token_literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LetStmt(lst) => write!(f, "{}", lst),
            Self::RetStmt(rst) => write!(f, "{}", rst),
            Self::ExpStmt(est) => write!(f, "{}", est),
            Self::BlcStmt(bst) => write!(f, "{}", bst),
        }
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
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

#[derive(Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.tok_literal)
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Expression,
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
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
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

#[derive(Clone)]
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

pub struct IfExpression {
    pub token: Token,
    pub condition: Expression,
    pub consequence: Rc<Statement>,
    pub alternative: Option<Rc<Statement>>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} ", self.condition)?;
        write!(f, "{}", self.consequence)?;
        if let Some(alternative) = &self.alternative {
            write!(f, "{}", alternative)?;
        }
        Ok(())
    }
}

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Expression>,
    pub body: Rc<Statement>,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (", self.token.tok_literal)?;
        let para_lists = self
            .parameters
            .iter()
            .map(|para| para.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{} )", para_lists)?;
        write!(f, "{}", self.body)?;
        Ok(())
    }
}

pub struct CallExpression {
    pub token: Token,
    pub function: Rc<Expression>,
    pub arguments: Vec<Expression>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> Option<&str> {
        Some(&self.token.tok_literal)
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args_list = self
            .arguments
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({})", self.function, args_list)
    }
}

#[derive(Clone)]
pub enum Expression {
    Identifier(Identifier),
    StringLit(StringLiteral),
    IntLit(IntegerLiteral),
    PreExp(Rc<PrefixExpression>),
    InExp(Rc<InfixExpression>),
    BoolLit(Boolean),
    IfExp(Rc<IfExpression>),
    FncLit(Rc<FunctionLiteral>),
    CallExp(Rc<CallExpression>),
}

impl Node for Expression {
    fn token_literal(&self) -> Option<&str> {
        match self {
            Self::Identifier(ident) => ident.token_literal(),
            Self::IntLit(int_lit) => int_lit.token_literal(),
            Self::StringLit(str_lit) => str_lit.token_literal(),
            Self::PreExp(pe) => pe.token_literal(),
            Self::InExp(ie) => ie.token_literal(),
            Self::BoolLit(bl) => bl.token_literal(),
            Self::IfExp(ife) => ife.token_literal(),
            Self::FncLit(fnl) => fnl.token_literal(),
            Self::CallExp(ce) => ce.token_literal(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "{id}"),
            Self::IntLit(int_lit) => write!(f, "{int_lit}"),
            Self::StringLit(str_lit) => write!(f, "{str_lit}"),
            Self::PreExp(pe) => write!(f, "{pe}"),
            Self::InExp(ie) => write!(f, "{ie}"),
            Self::BoolLit(bl) => write!(f, "{bl}"),
            Self::IfExp(ife) => write!(f, "{ife}"),
            Self::FncLit(fnl) => write!(f, "{fnl}"),
            Self::CallExp(ce) => write!(f, "{ce}"),
        }
    }
}

#[cfg(test)]
mod test;
