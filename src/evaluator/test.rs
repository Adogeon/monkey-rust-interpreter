use super::*;
use crate::object::Object;
use crate::{lexer::Lexer, parser::Parser};

#[test]
fn test_eval_integer_expression() -> Result<(), String> {
    let test_cases: Vec<(&str, u64)> = vec![("5", 5), ("10", 10)];

    for (input, expected) in test_cases {
        let evaluated = test_eval(input)?;
        assert!(test_integer_object(evaluated, expected).is_ok());
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

fn test_integer_object(obj: Object, expected: u64) -> Result<(), String> {
    if let Object::INTEGER(val) = obj {
        if val == expected {
            Ok(())
        } else {
            Err(format!(
                "Object value is not equal to expected, got {}",
                val
            ))
        }
    } else {
        Err(String::from("object is not an Integer Object"))
    }
}
