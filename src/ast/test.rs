use super::*;
use crate::token::{TType, Token};

#[test]
fn test_write_string() {
    let p = Program {
        statements: vec![
            (Statement::LetStmt(LetStatement {
                stmt_token: Token {
                    tok_type: TType::LET,
                    tok_literal: String::from("let"),
                },
                name: Identifier {
                    idt_token: Token {
                        tok_type: TType::IDENT,
                        tok_literal: String::from("myVar"),
                    },
                    value: String::from("myVar"),
                },
                value: Some(Expression::Identifier(Identifier {
                    idt_token: Token {
                        tok_type: TType::IDENT,
                        tok_literal: String::from("anotherVar"),
                    },
                    value: String::from("anotherVar"),
                })),
            })),
        ],
    };

    assert_eq!(
        p.to_string(),
        "let myVar = anotherVar;",
        "program.write_string() wrong, got={}",
        p.to_string()
    )
}
