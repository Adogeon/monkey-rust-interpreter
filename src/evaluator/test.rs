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
        test_integer_object(&evaluated, expected)?;
    }
    Ok(())
}

fn test_eval(input: &str) -> Result<Object, String> {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p
        .parse_program()
        .map_err(|err| format!("Parsing error: {err}"))?;

    let env = Environment::new();
    Ok(eval(&program, env))
}

fn test_integer_object(obj: &Object, expected: i64) -> Result<(), String> {
    if let Object::INTEGER(val) = obj {
        if *val == expected {
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
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        test_boolean_object(&evaluated, expected)?;
    }

    Ok(())
}

fn test_boolean_object(input: &Object, expected: bool) -> Result<(), String> {
    if let Object::BOOLEAN(s) = input {
        if *s == expected {
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
        test_boolean_object(&evaluated, expected)?;
    }
    Ok(())
}

#[test]
fn test_minus_operator() -> Result<(), String> {
    let test_cases: Vec<(&str, i64)> = vec![("5", 5), ("-5", -5), ("11", 11), ("-11", -11)];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        test_integer_object(&evaluated, expected)?;
    }
    Ok(())
}

#[test]
fn test_if_else_expressions() -> Result<(), String> {
    let test_cases = vec![
        ("if(true) {10}", "10"),
        ("if(false) {10}", "null"),
        ("if(1) {10}", "10"),
        ("if(1 < 2) {10}", "10"),
        ("if(1 > 2) {10}", "null"),
        ("if(1 > 2) {10} else {20}", "20"),
        ("if(1 < 2) {10} else {20}", "10"),
    ];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        expected.parse::<i64>().map_or_else(
            |_e| test_null_object(&evaluated),
            |v| test_integer_object(&evaluated, v),
        )?;
    }

    Ok(())
}

fn test_null_object(evaluated: &Object) -> Result<(), String> {
    match evaluated {
        Object::NULL => Ok(()),
        _ => Err(String::from("evalutated is not a Null Object")),
    }
}

#[test]
fn test_return_statement() -> Result<(), String> {
    let test_cases = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2*5; 9;", 10),
        ("if(10 > 1) { if  (10>1) {return 10;} return 1;}", 10),
    ];
    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        test_integer_object(&evaluated, expected)?;
    }
    Ok(())
}

#[test]
fn test_error_handling() -> Result<(), String> {
    let test_cases = vec![
        ("5+true;", "type mismatch:INTEGER + BOOLEAN"),
        ("5+true;5", "type mismatch:INTEGER + BOOLEAN"),
        ("-true;", "unknown operator:-BOOLEAN"),
        ("true + false;", "unknown operator:BOOLEAN + BOOLEAN"),
        ("5;true+false;5", "unknown operator:BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) {true + false;}",
            "unknown operator:BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) { if(10 > 1) { return true + false;} return 1;};",
            "unknown operator:BOOLEAN + BOOLEAN",
        ),
    ];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;

        if let Object::ERROR(msg) = evaluated {
            assert_eq!(expected, msg)
        } else {
            panic!("not an error object for input:{}, got={}", input, evaluated)
        }
    }
    Ok(())
}

#[test]
fn test_let_statements() -> Result<(), String> {
    let test_cases = vec![
        ("let a = 5;a", 5),
        ("let a = 5 * 5;a", 25),
        ("let a = 5; let b = a;b;", 5),
        ("let a = 5;let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in test_cases {
        let val = test_eval(input)?;
        test_integer_object(&val, expected)?;
    }

    Ok(())
}

#[test]
fn test_function_eval() -> Result<(), String> {
    let input = "fn(x) {x + 2;};";
    let val = test_eval(input)?;
    if let Object::FUNCTION(func) = val {
        assert_eq!(
            1,
            func.parameters.len(),
            "function has wrong parameters, expected 1; got={}",
            func.parameters.len()
        );

        assert_eq!(
            "x",
            func.parameters[0].to_string(),
            "parameter is not 'x', got = {}",
            func.parameters[0]
        );

        assert_eq!(
            "(x + 2)",
            func.body.to_string(),
            "body is not \"(x + 2)\". got = {}",
            func.body.to_string()
        )
    } else {
        panic!("Result object is not a function");
    }

    Ok(())
}

#[test]
fn test_functions_application() -> Result<(), String> {
    let test_cases: Vec<(&str, i64)> = vec![
        ("let identify = fn(x) {x;}; identify(5);", 5),
        ("let identify = fn(x) {return x;}; identify(5);", 5),
        ("let double = fn(x) {x * 2;}; double(5);", 10),
        ("let add = fn(x, y) {x + y;}; add(5,5);", 10),
        ("let add = fn(x,y) {x + y;}; add(5 + 5, add(5,5));", 20),
        ("fn(x) {x;}(5)", 5),
    ];

    for (input, expected) in test_cases {
        let val = test_eval(input)?;
        test_integer_object(&val, expected)?;
    }
    Ok(())
}

#[test]
fn test_closures() -> Result<(), String> {
    let input = "
        let newAdder = fn(x) {
            fn(y) {x + y};
        };
        let addTwo = newAdder(2);
        addTwo(2);
    ";
    let val = test_eval(input)?;
    test_integer_object(&val, 4.into())?;
    Ok(())
}

#[test]
fn test_string_literal() -> Result<(), String> {
    let input = "\"Hello World!\"";

    let val = test_eval(input)?;

    if let Object::STRING(str) = val {
        assert_eq!("Hello World!", str);
    } else {
        panic!("Result is not a STRING OBJECT")
    }

    Ok(())
}
