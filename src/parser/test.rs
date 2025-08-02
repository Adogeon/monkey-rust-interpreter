use super::*;
use crate::ast::{self};
use crate::lexer::{self, Lexer};

#[test]
fn test_let_statments() -> Result<(), String> {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    if program == nil {
        Err(String::from("parse_program() retuned nil"))
    }
    if program.statements.len() != 3 {
        let err_msg =
            "program.Statements does not contain 3 statement, got" + program.statements.len();
        Err(err_msg)
    }
}
