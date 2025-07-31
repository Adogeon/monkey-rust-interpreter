use crate::ast;
use crate::lexer;
use crate::token::{self, T_type};
use std::boxed::Box;

pub struct Parser {
    l: Box<lexer::Lexer>,
    cur_token: token::Token,
    peek_token: token::Token,
}

impl Parser {
    pub fn next_tok(&self) {
        self.cur_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    pub fn new(&self, l: Box<lexer::Lexer>) -> &Parser {
        let first_token = self.l.next_token();
        let p = &Parser {
            l: l,
            cur_token: first_token,
            peek_token: first_token,
        };
        p.next_tok();
    }

    pub fn parse_program(&self) -> &ast::Program {
        let mut program = &mut ast::Program { statements: [] };

        while self.cur_token.Type != T_type::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.append(stmt)
            }
            self.next_tok()
        }

        return program;
    }

    fn parse_statement(&self) -> Option<Box<dyn ast::Statement>> {
        match (self.cur_token.T_type) {
            T_type::LET => self.parse_let_stmt(),
            _ => None,
        }
    }

    fn parse_let_stmt(&self) -> Option<&ast::LetStatement> {
        let stmt_token = self.cur_token;

        if !self.expect_peek(T_type::IDENT) {
            None
        }

        let stmt_name = &ast::Identifier {
            idt_token: self.cur_token,
            value: self.cur_token.Literal,
        };

        if !self.expect_peek(T_type::ASSIGN) {
            None
        }

        while !self.cur_tok_is(T_type::SEMICOLON) {
            self.nextToken();
        }

        &ast::LetStatement {
            stmt_token,
            name: stmt_name,
            value: stmt_token.Literal,
        }
    }

    fn cur_tok_is(&self, t: T_type) -> bool {
        self.cur_token.Type == t
    }

    fn peek_tok_is(&self, t: T_type) -> bool {
        self.peek_token.Type == t
    }

    fn expect_peek(&self, t: T_type) -> bool {
        if self.peek_tok_is(t) {
            self.next_tok();
            true
        } else {
            false
        }
    }
}
