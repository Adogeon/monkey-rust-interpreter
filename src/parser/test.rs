use super::*;
use crate::ast::{Node, Statement};
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

    assert_eq!(1, program.statements.len(), "program has not enough statements.got={}", program.statements.len());
    for st in program.statements{
        let ex_stmt = if let Statement::ExpStmt(es) = st {
            es
        } else {
            panic!("st is not a ExpressionStatement");
        };

       let literal = if let Expression::IntLit(il) = ex_stmt.expression {
            assert!(5, il.value,"literal.value not {}, got {}", 5, il.value);
            let tok_lit = il.token_literal.unwrap_or_blank();
            assert!("5", tok_lit, "literal.TokenLiteral not {}, got {}", "5", tok_lit);
        } else {
            panic!("st.Expression is not a IntergerLiteral"):
        }
    }
    
    Ok(())
}
