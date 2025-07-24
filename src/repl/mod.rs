use crate::lexer::Lexer;
use crate::token::T_type;
use std::io::{self, BufRead};

pub fn start(in_handler: io::StdinLock, out_handler: io::StdoutLock) {
    let mut buffer = String::new();

    loop {
        in_handler.read_line(&mut buffer);
        let l = Lexer::new(&buffer);
        while let tok = l.next_token() {
            if tok.Type != T_type::EOF {
                writeln!(out_handler, "Token: {}", tok);
            }
        }
    }
}
