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
fn test_boolean_literal_expression() -> Result<(), String> {
    let input = "true;";

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

        if let Expression::BoolLit(bl) = ex_stmt.expression {
            assert_eq!(
                true, bl.value,
                "literal.value not {}, got {}",
                true, bl.value
            );
            let tok_lit = bl.token_literal().unwrap_or("blank");
            assert_eq!(
                "true", tok_lit,
                "literal.TokenLiteral not {}, got {}",
                "true", tok_lit
            );
        } else {
            panic!("st.Expression is not a BooleanLiteral");
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
            .map_err(|e| -> String { format!("parse error:{e}") })?;

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

        if let Expression::PreExp(pe) = &exp_stmt.expression {
            assert_eq!(
                tcase.1, pe.operator,
                "exp.Opartor is not {}, got={}",
                tcase.1, pe.operator
            );
            assert!(test_integer_literal(&*pe.right, tcase.2).is_ok());
        } else {
            panic!("exp_stmt is not a PrefixExpression")
        };
    }
    Ok(())
}

fn test_integer_literal(il: &Expression, value: u64) -> Result<(), String> {
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

fn test_identifier(ie: &Expression, value: String) -> Result<(), String> {
    if let Expression::Identifier(ident) = ie {
        assert_eq!(
            value, ident.value,
            "ident.value not {value}. got={}",
            ident.value
        );
        let tok_lit = ident.token_literal().unwrap_or("blank");
        assert_eq!(
            value, tok_lit,
            "ident.TokenLiteral not {value}, got {tok_lit}"
        );
    } else {
        panic!("ie is not Indentifier Expression")
    }
    Ok(())
}

#[derive(PartialEq, Debug)]
enum LiteralVal {
    Int(u64),
    Ident(String),
}

impl From<u64> for LiteralVal {
    fn from(value: u64) -> Self {
        LiteralVal::Int(value)
    }
}

impl From<&str> for LiteralVal {
    fn from(value: &str) -> Self {
        LiteralVal::Ident(value.to_string())
    }
}

fn test_literal_expression(exp: &Expression, value: LiteralVal) -> Result<(), String> {
    match value {
        LiteralVal::Int(val) => test_integer_literal(exp, val),
        LiteralVal::Ident(id) => test_identifier(exp, id),
        _ => panic!("Mismatch literal type"),
    }
}

fn test_infix_expression(
    exp: &Expression,
    left: LiteralVal,
    operator: String,
    right: LiteralVal,
) -> Result<(), String> {
    if let Expression::InExp(infix_exp) = exp {
        assert!(test_literal_expression(infix_exp.left.as_ref(), left).is_ok());
        assert_eq!(operator, infix_exp.operator);
        assert!(test_literal_expression(infix_exp.right.as_ref(), right).is_ok());
        Ok(())
    } else {
        panic!("exp is not Infix Expression")
    }
}
#[test]
fn test_parsing_infix_expression() -> Result<(), String> {
    struct Case<'a> {
        input: &'a str,
        left_val: u64,
        operator: &'a str,
        right_val: u64,
    }

    let test_cases: Vec<Case> = vec![
        Case {
            input: "5+5;",
            left_val: 5,
            operator: "+",
            right_val: 5,
        },
        Case {
            input: "5-5;",
            left_val: 5,
            operator: "-",
            right_val: 5,
        },
        Case {
            input: "5*5;",
            left_val: 5,
            operator: "*",
            right_val: 5,
        },
        Case {
            input: "5/5;",
            left_val: 5,
            operator: "/",
            right_val: 5,
        },
        Case {
            input: "5>5;",
            left_val: 5,
            operator: ">",
            right_val: 5,
        },
        Case {
            input: "5<5;",
            left_val: 5,
            operator: "<",
            right_val: 5,
        },
        Case {
            input: "5==5;",
            left_val: 5,
            operator: "==",
            right_val: 5,
        },
        Case {
            input: "5!=5;",
            left_val: 5,
            operator: "!=",
            right_val: 5,
        },
    ];

    for tcase in test_cases {
        let l = Lexer::new(tcase.input);
        let mut p = Parser::new(l);
        let program = p
            .parse_program()
            .map_err(|err| -> String { format!("parse error{err}") })?;

        assert_eq!(
            1,
            program.statements.len(),
            "program statement does not contain 1 statement. got {}",
            program.statements.len()
        );

        let inf_stmt = if let Statement::ExpStmt(ex) = &program.statements[0] {
            ex
        } else {
            panic!("program.statements[0] is not an Expression Statement")
        };

        assert!(test_infix_expression(
            &inf_stmt.expression,
            tcase.left_val.into(),
            tcase.operator.to_string(),
            tcase.right_val.into()
        )
        .is_ok())
    }

    Ok(())
}
