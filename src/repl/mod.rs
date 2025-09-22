use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser};
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

        let l = Lexer::new(&buffer);
        let mut p = Parser::new(l);
        let program = match p.parse_program() {
            Ok(prog) => prog,
            Err(err) => {
                print_parser_errors(&mut out_handler, err);
                continue;
            }
        };

        writeln!(out_handler, "{}", program).unwrap();
    }
}

fn print_parser_errors(out_handler: &mut io::StdoutLock, error: ParseError) {
    writeln!(out_handler, "{error}").unwrap();
}
