use crate::token::*;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'a> {
    input_iter: Peekable<Chars<'a>>,
    ch: Option<char>,
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_number(ch: char) -> bool {
    ch.is_numeric()
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a String) -> Self {
        let mut iter = input.chars().peekable();
        let current = iter.next();
        Lexer {
            input_iter: iter,
            ch: current,
        }
    }

    fn read_char(&mut self) {
        self.ch = self.input_iter.next();
    }

    fn peek_next(&mut self) -> Option<&char> {
        self.input_iter.peek()
    }

    fn consume_whitespace(&mut self) {
        while let Some(current) = self.ch {
            if current.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn read_mult_char(&mut self, check_fn: fn(char) -> bool) -> String {
        let mut mult_char: Vec<char> = Vec::new();

        while let Some(c) = self.ch {
            if !check_fn(c) {
                break;
            }
            mult_char.push(c);

            if let Some(next) = self.peek_next() {
                if !check_fn(*next) {
                    break;
                }
            } else {
                break;
            }
            self.read_char();
        }
        mult_char.iter().collect()
    }

    pub fn next_token(&mut self) -> Token {
        self.consume_whitespace();
        let tok = match self.ch {
            Some('+') => Token::new(T_type::PLUS, "+"),
            Some('-') => Token::new(T_type::MINUS, "-"),
            Some('*') => Token::new(T_type::ASTERISK, "*"),
            Some('/') => Token::new(T_type::SLASH, "/"),
            Some('<') => Token::new(T_type::LT, "<"),
            Some('>') => Token::new(T_type::GT, ">"),
            Some(',') => Token::new(T_type::COMMA, ","),
            Some(';') => Token::new(T_type::SEMICOLON, ";"),
            Some('(') => Token::new(T_type::LPAREN, "("),
            Some(')') => Token::new(T_type::RPAREN, ")"),
            Some('{') => Token::new(T_type::LBRACE, "{"),
            Some('}') => Token::new(T_type::RBRACE, "}"),
            Some('=') => {
                if self.peek_next() == Some(&'=') {
                    self.read_char();
                    Token::new(T_type::EQ, "==")
                } else {
                    Token::new(T_type::ASSIGN, "=")
                }
            }
            Some('!') => {
                if self.peek_next() == Some(&'=') {
                    self.read_char();
                    Token::new(T_type::NOTEQ, "!=")
                } else {
                    Token::new(T_type::BANG, "!")
                }
            }
            Some(c) => {
                if is_letter(c) {
                    let literal = self.read_mult_char(is_letter);
                    let token = look_up_ident(&literal);
                    Token::new(token, literal.as_str())
                } else if is_number(c) {
                    let literal = self.read_mult_char(is_number);
                    Token::new(T_type::INT, literal.as_str())
                } else {
                    Token::new(T_type::ILLEGAL, &self.ch.unwrap().to_string())
                }
            }
            None => Token::new(T_type::EOF, ""),
        };
        self.read_char();
        tok
    }
}

#[cfg(test)]
mod test;
