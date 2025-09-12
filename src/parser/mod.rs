use crate::ast::{self, Expression, Statement};
use crate::lexer;
use crate::token::{self, TType};
use std::fmt::Display;

#[derive(PartialOrd, PartialEq)]
enum Precedent {
    LOWEST = 0,
    EQUALS = 1,
    LESSGREATER = 2,
    SUM = 3,
    PRODUCT = 4,
    PREFIX = 5,
    CALL = 6,
}

fn tok_preceden_look_up(tok_type: TType) -> Precedent {
    match tok_type {
        TType::EQ => Precedent::EQUALS,
        TType::NOTEQ => Precedent::EQUALS,
        TType::LT => Precedent::LESSGREATER,
        TType::GT => Precedent::LESSGREATER,
        TType::PLUS => Precedent::SUM,
        TType::MINUS => Precedent::SUM,
        TType::SLASH => Precedent::PRODUCT,
        TType::ASTERISK => Precedent::PRODUCT,
        TType::ASSIGN => Precedent::PRODUCT,
        _ => Precedent::LOWEST,
    }
}

pub enum ParseError {
    UnexpectedToken(String),
    ParsingError,
    IntLitParseError(String),
    NoPrefixParseFnError(TType),
    NoInfixParseFnError(TType),
    MissingClosing(TType),
    MissingExpectedToken(TType),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(tok_lit) => {
                write!(f, "Parse Error - UnexpectedToken : {tok_lit}",)
            }
            Self::IntLitParseError(tok_lit) => write!(f, "can't parse {tok_lit} to int"),
            Self::NoPrefixParseFnError(tok_type) => write!(
                f,
                "Parse Error - No prefix parse function for {:?} found",
                tok_type
            ),
            Self::NoInfixParseFnError(tok_type) => write!(
                f,
                "Parse Error - No infix parse function for {:?} found",
                tok_type
            ),
            Self::MissingClosing(tok_type) => write!(f, "Parse Error - Missing {:?}", tok_type),
            Self::MissingExpectedToken(tok_type) => {
                write!(f, "Parse Error - Expecting {:?}", tok_type)
            }
            Self::ParsingError => write!(f, "Program has parsing Error"),
        }
    }
}

pub struct Parser<'a> {
    l: lexer::Lexer<'a>,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn next_tok(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn new(l: lexer::Lexer<'a>) -> Self {
        let mut p = Parser {
            l: l,
            cur_token: token::Token::new(TType::EOF, ""),
            peek_token: token::Token::new(TType::EOF, ""),
            errors: Vec::new(),
        };
        p.next_tok();
        p.next_tok();
        p
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ParseError> {
        let mut program = ast::Program { statements: vec![] };

        while self.cur_token.tok_type != TType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            } else {
                self.append_errors(ParseError::UnexpectedToken(
                    self.cur_token.tok_literal.clone(),
                ));
            }
            self.next_tok()
        }

        if self.errors.len() > 0 {
            println!("parser has {} errors", self.errors.len());
            for err in &self.errors {
                println!("{err}");
            }
            return Err(ParseError::ParsingError);
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.tok_type {
            TType::LET => self.parse_let_stmt(),
            TType::RETURN => self.parse_return_stmt(),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Statement> {
        let stmt_token = self.cur_token.clone();

        self.expect_peek(TType::IDENT)?;

        let stmt_name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.tok_literal.clone(),
        };

        self.expect_peek(TType::ASSIGN)?;

        while !self.cur_tok_is(TType::SEMICOLON) {
            self.next_tok();
        }

        let let_stmt = ast::LetStatement {
            stmt_token,
            name: stmt_name,
            value: None,
        };

        Some(Statement::LetStmt(let_stmt))
    }

    fn parse_return_stmt(&mut self) -> Option<Statement> {
        let mut stmt_value = None;
        let ret_stmt = ast::ReturnStatement {
            stmt_token: self.cur_token.clone(),
            return_value: stmt_value,
        };

        self.next_tok();

        while !self.cur_tok_is(TType::SEMICOLON) {
            self.next_tok();
        }

        Some(Statement::RetStmt(ret_stmt))
    }

    fn parse_expression_stmt(&mut self) -> Option<Statement> {
        let exp_tok = self.cur_token.clone();
        let express = self.parse_expression(Precedent::LOWEST)?;
        if self.peek_tok_is(TType::SEMICOLON).is_some() {
            self.next_tok();
        }

        Some(Statement::ExpStmt(ast::ExpressionStatement {
            stmt_token: exp_tok,
            expression: express,
        }))
    }

    fn append_errors(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    fn cur_tok_is(&self, t: TType) -> bool {
        self.cur_token.tok_type == t
    }

    fn peek_tok_is(&self, t: TType) -> Option<()> {
        if self.peek_token.tok_type == t {
            Some(())
        } else {
            None
        }
    }

    fn expect_peek(&mut self, t: TType) -> Option<()> {
        self.peek_tok_is(t)?;
        self.next_tok();
        Some(())
    }

    fn cur_precedence(&self) -> Precedent {
        tok_preceden_look_up(self.cur_token.tok_type.clone())
    }

    fn peek_precedence(&self) -> Precedent {
        tok_preceden_look_up(self.peek_token.tok_type.clone())
    }

    fn prefix_parse_fn(&mut self, tok_type: TType) -> Option<Expression> {
        match tok_type {
            TType::IDENT => self.parse_identifier().ok(),
            TType::INT => self.parse_integer_literal().ok(),
            TType::BANG | TType::MINUS => self.parse_prefix_expression().ok(),
            TType::TRUE | TType::FALSE => self.parse_boolean().ok(),
            TType::LPAREN => self.parse_grouped_expression().ok(),
            TType::IF => self.parse_if_expression().ok(),
            _ => None,
        }
    }

    fn check_infix_tok(&self, tok_type: TType) -> bool {
        matches!(
            tok_type,
            TType::PLUS
                | TType::MINUS
                | TType::SLASH
                | TType::ASTERISK
                | TType::EQ
                | TType::NOTEQ
                | TType::LT
                | TType::GT
        )
    }

    fn parse_expression(&mut self, precedence: Precedent) -> Option<Expression> {
        let prefix = self.prefix_parse_fn(self.cur_token.tok_type.clone());

        if let Some(result) = prefix {
            let mut left_exp: Option<Box<Expression>> = Some(Box::new(result));
            while self.peek_tok_is(TType::SEMICOLON).is_none()
                && precedence < self.peek_precedence()
            {
                if self.check_infix_tok(self.peek_token.tok_type.clone()) {
                    self.next_tok();
                    if let Some(exp) = self.parse_infix_expression(left_exp.take().unwrap()).ok() {
                        left_exp = Some(Box::new(exp));
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            left_exp.map(|exp| *exp)
        } else {
            None
        }
    }

    fn parse_identifier(&self) -> Result<Expression, ParseError> {
        let value = self.cur_token.tok_literal.clone();
        let token = self.cur_token.clone();
        let ident_exp = ast::Identifier { token, value };
        Ok(Expression::Identifier(ident_exp))
    }

    fn parse_boolean(&self) -> Result<Expression, ParseError> {
        let bool_lit = ast::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_tok_is(TType::TRUE),
        };
        Ok(Expression::BoolLit(bool_lit))
    }

    fn parse_integer_literal(&self) -> Result<Expression, ParseError> {
        let int_token = self.cur_token.clone();
        let value: u64 = int_token
            .tok_literal
            .parse()
            .map_err(|_| ParseError::IntLitParseError(int_token.tok_literal.clone()))?;

        Ok(Expression::IntLit(ast::IntegerLiteral {
            token: int_token,
            value,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();
        let literal = self.cur_token.tok_literal.clone();

        self.next_tok();
        let right = self
            .parse_expression(Precedent::PREFIX)
            .ok_or(ParseError::ParsingError)?;

        Ok(Expression::PreExp(ast::PrefixExpression {
            token,
            operator: literal,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();
        let literal = self.cur_token.tok_literal.clone();
        let precedence = self.cur_precedence();

        self.next_tok();
        let right = self
            .parse_expression(precedence)
            .ok_or(ParseError::ParsingError)?;

        Ok(Expression::InExp(ast::InfixExpression {
            token,
            operator: literal,
            left: left,
            right: Box::new(right),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_tok();
        let exp = self
            .parse_expression(Precedent::LOWEST)
            .ok_or(ParseError::ParsingError)?;

        if self.expect_peek(TType::RPAREN).is_none() {
            return Err(ParseError::MissingClosing(TType::RPAREN));
        }

        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let tok = self.cur_token.clone();
        if self.expect_peek(TType::LPAREN).is_none() {
            return Err(ParseError::MissingExpectedToken(TType::LPAREN));
        }

        self.next_tok();
        let condition = self
            .parse_expression(Precedent::LOWEST)
            .ok_or(ParseError::ParsingError)?;

        if self.expect_peek(TType::RPAREN).is_none() {
            return Err(ParseError::MissingClosing(TType::RPAREN));
        }

        if self.expect_peek(TType::LBRACE).is_none() {
            return Err(ParseError::MissingExpectedToken(TType::LBRACE));
        }

        let consequence = self
            .parse_block_statement()
            .map_err(|_err| ParseError::ParsingError)?;

        let mut alternative = None;

        if self.peek_tok_is(TType::ELSE).is_some() {
            self.next_tok();
            if self.expect_peek(TType::LBRACE).is_none() {
                return Err(ParseError::MissingExpectedToken(TType::LBRACE));
            }
            alternative = Some(
                self.parse_block_statement()
                    .map_err(|_err| ParseError::ParsingError)?,
            );
        }

        Ok(Expression::IfExp(ast::IfExpression {
            token: tok,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<ast::BlockStatement, ParseError> {
        let tok = self.cur_token.clone();
        let mut statements: Vec<Statement> = vec![];

        self.next_tok();
        while !self.cur_tok_is(TType::RBRACE) && !self.cur_tok_is(TType::EOF) {
            let stmt = self.parse_statement().ok_or(ParseError::ParsingError)?;
            statements.push(stmt);
            self.next_tok()
        }

        Ok(ast::BlockStatement {
            token: tok,
            statements,
        })
    }
}

#[cfg(test)]
mod test;
