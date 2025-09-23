use super::*;
use crate::object::Object;
use crate::{lexer::Lexer, parser::Parser};

#[test]
fn test_eval_integer_expression() -> Result<(), String> {
    let test_cases: Vec<(&str, i64)> = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        test_integer_object(evaluated, expected)?;
    }
    Ok(())
}

fn test_eval(input: &str) -> Result<Object, String> {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p
        .parse_program()
        .map_err(|err| format!("Parsing error: {err}"))?;

    Ok(eval(Box::new(program)))
}

fn test_integer_object(obj: Object, expected: i64) -> Result<(), String> {
    if let Object::INTEGER(val) = obj {
        if val == expected {
            Ok(())
        } else {
            Err(format!(
                "Object value is not equal to expected:{expected}, got {}",
                val
            ))
        }
    } else {
        Err(String::from("object is not an Integer Object"))
    }
}

#[test]
fn test_eval_boolean_expression() -> Result<(), String> {
    let test_cases = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
    ];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        test_boolean_object(evaluated, expected)?;
    }

    Ok(())
}

fn test_boolean_object(input: Object, expected: bool) -> Result<(), String> {
    if let Object::BOOLEAN(s) = input {
        if s == expected {
            Ok(())
        } else {
            Err(format!("expected {expected}, got={s}"))
        }
    } else {
        Err(format!("input is not a Boolean Object"))
    }
}

#[test]
fn test_bang_operator() -> Result<(), String> {
    let test_cases = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        test_boolean_object(evaluated, expected)?;
    }
    Ok(())
}

#[test]
fn test_minus_operator() -> Result<(), String> {
    let test_cases: Vec<(&str, i64)> = vec![("5", 5), ("-5", -5), ("11", 11), ("-11", -11)];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        test_integer_object(evaluated, expected)?;
    }
    Ok(())
}
