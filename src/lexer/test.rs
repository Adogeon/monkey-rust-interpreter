use super::*;
use crate::token::TType;

#[test]
fn it_works() {
    let input = "let five = 5;
    let ten = 10;
    let add = fn(x,y) {
      x+y;
    };
    let result = add(five,ten);
    !-/*5;
    5 < 10 > 5;
    
    if(5 < 10) {
      return true;

    } else {
      return false;
    }
    
    10 == 10;
    10 != 9;
    ";

    let expected_results = vec![
        (TType::LET, "let"),
        (TType::IDENT, "five"),
        (TType::ASSIGN, "="),
        (TType::INT, "5"),
        (TType::SEMICOLON, ";"),
        (TType::LET, "let"),
        (TType::IDENT, "ten"),
        (TType::ASSIGN, "="),
        (TType::INT, "10"),
        (TType::SEMICOLON, ";"),
        (TType::LET, "let"),
        (TType::IDENT, "add"),
        (TType::ASSIGN, "="),
        (TType::FUNCTION, "fn"),
        (TType::LPAREN, "("),
        (TType::IDENT, "x"),
        (TType::COMMA, ","),
        (TType::IDENT, "y"),
        (TType::RPAREN, ")"),
        (TType::LBRACE, "{"),
        (TType::IDENT, "x"),
        (TType::PLUS, "+"),
        (TType::IDENT, "y"),
        (TType::SEMICOLON, ";"),
        (TType::RBRACE, "}"),
        (TType::SEMICOLON, ";"),
        (TType::LET, "let"),
        (TType::IDENT, "result"),
        (TType::ASSIGN, "="),
        (TType::IDENT, "add"),
        (TType::LPAREN, "("),
        (TType::IDENT, "five"),
        (TType::COMMA, ","),
        (TType::IDENT, "ten"),
        (TType::RPAREN, ")"),
        (TType::SEMICOLON, ";"),
        (TType::BANG, "!"),
        (TType::MINUS, "-"),
        (TType::SLASH, "/"),
        (TType::ASTERISK, "*"),
        (TType::INT, "5"),
        (TType::SEMICOLON, ";"),
        (TType::INT, "5"),
        (TType::LT, "<"),
        (TType::INT, "10"),
        (TType::GT, ">"),
        (TType::INT, "5"),
        (TType::SEMICOLON, ";"),
        (TType::IF, "if"),
        (TType::LPAREN, "("),
        (TType::INT, "5"),
        (TType::LT, "<"),
        (TType::INT, "10"),
        (TType::RPAREN, ")"),
        (TType::LBRACE, "{"),
        (TType::RETURN, "return"),
        (TType::TRUE, "true"),
        (TType::SEMICOLON, ";"),
        (TType::RBRACE, "}"),
        (TType::ELSE, "else"),
        (TType::LBRACE, "{"),
        (TType::RETURN, "return"),
        (TType::FALSE, "false"),
        (TType::SEMICOLON, ";"),
        (TType::RBRACE, "}"),
        (TType::INT, "10"),
        (TType::EQ, "=="),
        (TType::INT, "10"),
        (TType::SEMICOLON, ";"),
        (TType::INT, "10"),
        (TType::NOTEQ, "!="),
        (TType::INT, "9"),
        (TType::SEMICOLON, ";"),
        (TType::EOF, ""),
    ];

    let mut l = Lexer::new(input);

    for (i, test_run) in expected_results.iter().enumerate() {
        let result = l.next_token();

        println!(
            "Index {}: got Type = {:?}, Literal = {:?} | expected Type = {:?}, Literal = {:?}",
            i, result.tok_type, result.tok_literal, test_run.0, test_run.1
        );
        assert_eq!(
            &result.tok_type, &test_run.0,
            "Type mismatch at index {}: got{:?}, expected {:?}",
            i, result.tok_type, test_run.0
        );
        assert_eq!(
            &result.tok_literal, &test_run.1,
            "Type mismatch at index {}: got{:?}, expected{:?}",
            i, result.tok_literal, test_run.1
        );
    }
}
