use crate::ast::{self, StatementEnum};
use crate::lexer;
use crate::token::{self, TType};
use std::boxed::Box;

enum Precedent {
    LOWEST = 0,
    EQUALS = 1,
    LESSGREATER = 2,
    SUM = 3,
    PRODUCT = 4,
    PREFIX = 5,
    CALL = 6,
}

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
            TType::RETURN => self.parse_return_stmt(),
            _ => None,
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Box<StatementEnum>> {
        let stmt_token = self.cur_token.clone();
        let mut stmt_value = None;
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
            value: stmt_value,
        };

        Some(Box::new(StatementEnum::LetStmt(let_stmt)))
    }

    fn parse_return_stmt(&mut self) -> Option<Box<StatementEnum>> {
        let mut stmt_value = None;
        let ret_stmt = ast::ReturnStatement {
            stmt_token: self.cur_token.clone(),
            return_value: stmt_value,
        };

        self.next_tok();

        while !self.cur_tok_is(TType::SEMICOLON) {
            self.next_tok();
        }

        Some(Box::new(StatementEnum::RetStmt(ret_stmt)))
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

    fn prefix_parse_fn(&self, tok_type: &TType) -> Option<Box<dyn ast::Expression>> {
        match tok_type {
            TType::IDENT => Some(self.parse_identifier()),
            _ => None,
        }
    }

    fn parse_expression(&self, precedence: Precedent) -> Option<Box<dyn ast::Expression>> {
        let prefix = self.prefix_parse_fn(&self.cur_token.tok_type);

        if let Some(result) = prefix {
            let left_exp = result;
            Some(left_exp)
        } else {
            None
        }
    }

    fn parse_identifier(&self) -> Option<Box<ast::Identifier>> {
        let value = self.cur_token.tok_literal.clone();
        let idt_token = self.cur_token.clone();
        Some(Box::new(ast::Identifier { idt_token, value }))
    }
}

#[cfg(test)]
mod test;
