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
    pub fn new(input: &'a str) -> Self {
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
            Some('+') => Token::new(TType::PLUS, "+"),
            Some('-') => Token::new(TType::MINUS, "-"),
            Some('*') => Token::new(TType::ASTERISK, "*"),
            Some('/') => Token::new(TType::SLASH, "/"),
            Some('<') => Token::new(TType::LT, "<"),
            Some('>') => Token::new(TType::GT, ">"),
            Some(',') => Token::new(TType::COMMA, ","),
            Some(';') => Token::new(TType::SEMICOLON, ";"),
            Some('(') => Token::new(TType::LPAREN, "("),
            Some(')') => Token::new(TType::RPAREN, ")"),
            Some('{') => Token::new(TType::LBRACE, "{"),
            Some('}') => Token::new(TType::RBRACE, "}"),
            Some('[') => Token::new(TType::LBRACKET, "["),
            Some(']') => Token::new(TType::RBRACKET, "]"),
            Some('=') => {
                if self.peek_next() == Some(&'=') {
                    self.read_char();
                    Token::new(TType::EQ, "==")
                } else {
                    Token::new(TType::ASSIGN, "=")
                }
            }
            Some('!') => {
                if self.peek_next() == Some(&'=') {
                    self.read_char();
                    Token::new(TType::NOTEQ, "!=")
                } else {
                    Token::new(TType::BANG, "!")
                }
            }
            Some(k) if k == '"' || k == '\"' => {
                let literal = self.read_string();
                Token {
                    tok_type: TType::STRING,
                    tok_literal: literal,
                }
            }
            Some(c) => {
                if is_letter(c) {
                    let literal = self.read_mult_char(is_letter);
                    let token = look_up_ident(&literal);
                    Token {
                        tok_type: token,
                        tok_literal: literal,
                    }
                } else if is_number(c) {
                    let literal = self.read_mult_char(is_number);
                    Token {
                        tok_type: TType::INT,
                        tok_literal: literal,
                    }
                } else {
                    Token::new(TType::ILLEGAL, &self.ch.unwrap().to_string())
                }
            }
            None => Token::new(TType::EOF, ""),
        };
        self.read_char();
        tok
    }

    fn read_string(&mut self) -> String {
        self.read_char();

        if let Some(c) = self.ch {
            if c == '"' || c == '\"' {
                return String::from("");
            }
        }

        let mut buf: Vec<char> = vec![];
        while let Some(c) = self.ch {
            buf.push(c);
            if let Some(next) = self.peek_next() {
                if *next == '"' || *next == '\"' {
                    self.input_iter.next();
                    break;
                } else {
                    self.read_char();
                }
            }
        }
        buf.iter().collect()
    }
}

#[cfg(test)]
mod test;
