use crate::lexer::Lexer;
use crate::token::TType;
use std::io::{self, BufRead, Write};

pub fn start(mut in_handler: io::StdinLock, mut out_handler: io::StdoutLock) {
    let mut buffer = String::new();

    loop {
        write!(out_handler, ">>").unwrap();
        out_handler.flush().unwrap();

        buffer.clear();
        if let Err(e) = in_handler.read_line(&mut buffer) {
            writeln!(out_handler, "Error reading line: {}", e).unwrap();
            break;
        }

        let mut l = Lexer::new(&buffer);

        loop {
            let tok = l.next_token();
            if tok.tok_type == TType::EOF {
                break;
            }
            writeln!(out_handler, "Token:{:?}", tok).unwrap();
        }
    }
}
