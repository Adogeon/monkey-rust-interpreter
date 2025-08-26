use crate::ast::{self, StatementEnum};
use crate::lexer;
use crate::token::{self, TType};
use std::boxed::Box;

pub struct Parser<'a> {
    l: lexer::Lexer<'a>,
    cur_token: token::Token,
    peek_token: token::Token,
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
        };
        p.next_tok();
        p.next_tok();
        p
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ast::ParseError> {
        let mut program = ast::Program { statements: vec![] };

        while self.cur_token.tok_type != TType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            } else {
                return Err(ast::ParseError::UnexpectedToken);
            }
            self.next_tok()
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Option<Box<StatementEnum>> {
        match self.cur_token.tok_type {
            TType::LET => self.parse_let_stmt(),
            _ => None,
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Box<StatementEnum>> {
        let stmt_token = self.cur_token.clone();

        if !self.expect_peek(TType::IDENT) {
            return None;
        }

        let stmt_name = Box::new(ast::Identifier {
            idt_token: self.cur_token.clone(),
            value: self.cur_token.tok_literal.clone(),
        });

        if !self.expect_peek(TType::ASSIGN) {
            return None;
        }

        while !self.cur_tok_is(TType::SEMICOLON) {
            self.next_tok();
        }

        let let_stmt = ast::LetStatement {
            stmt_token,
            name: stmt_name,
            value: Box::new(ast::EmptyValue),
        };

        Some(Box::new(StatementEnum::LetStmt(let_stmt)))
    }

    fn cur_tok_is(&self, t: TType) -> bool {
        self.cur_token.tok_type == t
    }

    fn peek_tok_is(&self, t: TType) -> bool {
        self.peek_token.tok_type == t
    }

    fn expect_peek(&mut self, t: TType) -> bool {
        if self.peek_tok_is(t) {
            self.next_tok();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod test;
