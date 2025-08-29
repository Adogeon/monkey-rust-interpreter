use std::iter::Product;

use super::*;
use crate::ast::{Node, Program, Statement};
use crate::lexer::Lexer;

fn test_let_statement(stmt: &Statement, name: String) {
    let stmt_literal = stmt.token_literal().unwrap_or("blank");
    assert_eq!(
        "let", stmt_literal,
        "stmt_literal is not 'let', got ={}",
        stmt_literal
    );

    if let Statement::LetStmt(ref let_st) = *stmt {
        assert_eq!(
            name, let_st.name.value,
            "stmt.name.value not {}.got={}",
            name, let_st.name.value
        );
    } else {
        panic!("let stmt is not belong of kind letStmt")
    }
}

#[test]
fn test_let_statments() -> Result<(), String> {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = match p.parse_program() {
        Err(_e) => return Err(String::from("ParseProgram() error")),
        Ok(p) => p,
    };
    assert_eq!(
        3,
        program.statements.len(),
        "program.Statements does not contain 3 statements.got={}",
        program.statements.len()
    );

    let test_cases = vec!["x", "y", "foobar"];
    for (i, tc) in test_cases.iter().enumerate() {
        let stmt = &program.statements[i];
        test_let_statement(stmt, String::from(*tc));
    }

    Ok(())
}

#[test]
fn test_return_statements() -> Result<(), String> {
    let input = "
        return 5;
        return 10;
        return 993322;
    ";

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = match p.parse_program() {
        Err(_e) => return Err(String::from("ParseProgram() error")),
        Ok(p) => p,
    };

    assert_eq!(
        3,
        program.statements.len(),
        "program.statement does not contain 3 statement. got={}",
        program.statements.len()
    );

    for stmt in program.statements {
        if let Statement::RetStmt(return_stmt) = stmt {
            let stmt_literal = return_stmt.token_literal().unwrap_or("blank");
            assert_eq!(
                "return", stmt_literal,
                "return_stmt token literal not 'return', got {}",
                stmt_literal
            );
        } else {
            panic!("stmt is not of StatementEnum::RetStmt")
        }
    }

    Ok(())
}

#[test]
fn test_interget_literal_expression() -> Result<(), String> {
    let input = "5;";

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = match p.parse_program() {
        Err(_e) => return Err(String::from("ParseProgram() error")),
        Ok(p) => p,
    };

    assert_eq!(
        1,
        program.statements.len(),
        "program has not enough statements.got={}",
        program.statements.len()
    );
    for st in program.statements {
        let ex_stmt = if let Statement::ExpStmt(es) = st {
            es
        } else {
            panic!("st is not a ExpressionStatement");
        };

        if let Expression::IntLit(il) = ex_stmt.expression {
            assert_eq!(5, il.value, "literal.value not {}, got {}", 5, il.value);
            let tok_lit = il.token_literal().unwrap_or("blank");
            assert_eq!(
                "5", tok_lit,
                "literal.TokenLiteral not {}, got {}",
                "5", tok_lit
            );
        } else {
            panic!("st.Expression is not a IntergerLiteral");
        }
    }

    Ok(())
}

#[test]
fn test_parsing_prefix_expression() -> Result<(), String> {
    let prefix_tests = vec![("!5", "!", 5), ("-15", "-", 15)];

    for tcase in prefix_tests {
        let l = Lexer::new(tcase.0);
        let mut p = Parser::new(l);
        let program = p
            .parse_program()
            .map_err(|_| -> String { format!("parse error") })?;

        assert_eq!(
            1,
            program.statements.len(),
            "program.statements does not contain 1, got {}",
            program.statements.len()
        );

        let exp_stmt = if let Statement::ExpStmt(ex) = &program.statements[0] {
            ex
        } else {
            panic!("program.statements[0] is not an Expression Statement")
        };

        if let Expression::PreExp(pe) = exp_stmt.expression {
            assert_eq!(
                tcase.1, pe.operator,
                "exp.Opartor is not {}, got={}",
                tcase.1, pe.operator
            );
            assert!(test_integer_literal(pe.right, tcase.2).is_ok());
        } else {
            panic!("exp_stmt is not a PrefixExpression")
        };
    }
    Ok(())
}

fn test_integer_literal(il: Expression, value: u64) -> Result<(), String> {
    if let Expression::IntLit(literal) = il {
        assert_eq!(
            value, literal.value,
            "literal.value not {}, got{}",
            value, literal.value
        );

        let tok_lit = literal.token_literal().unwrap_or("blank");

        assert_eq!(
            value.to_string(),
            tok_lit,
            "literal.token_literal() not {}, got {}",
            value.to_string(),
            tok_lit
        )
    } else {
        panic!("literal is not an Integer Literal")
    }
    Ok(())
}
