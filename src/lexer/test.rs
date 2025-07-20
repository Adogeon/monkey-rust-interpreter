use super::*;
use crate::token::T_type;

#[test]
fn it_works() {
    let input = r#"let five = 5;
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
    "#
    .to_owned();

    let expected_results = vec![
        (T_type::LET, "let"),
        (T_type::IDENT, "five"),
        (T_type::ASSIGN, "="),
        (T_type::INT, "5"),
        (T_type::SEMICOLON, ";"),
        (T_type::LET, "let"),
        (T_type::IDENT, "ten"),
        (T_type::ASSIGN, "="),
        (T_type::INT, "10"),
        (T_type::SEMICOLON, ";"),
        (T_type::LET, "let"),
        (T_type::IDENT, "add"),
        (T_type::ASSIGN, "="),
        (T_type::FUNCTION, "fn"),
        (T_type::LPAREN, "("),
        (T_type::IDENT, "x"),
        (T_type::COMMA, ","),
        (T_type::IDENT, "y"),
        (T_type::RPAREN, ")"),
        (T_type::LBRACE, "{"),
        (T_type::IDENT, "x"),
        (T_type::PLUS, "+"),
        (T_type::IDENT, "y"),
        (T_type::SEMICOLON, ";"),
        (T_type::RBRACE, "}"),
        (T_type::SEMICOLON, ";"),
        (T_type::LET, "let"),
        (T_type::IDENT, "result"),
        (T_type::ASSIGN, "="),
        (T_type::IDENT, "add"),
        (T_type::LPAREN, "("),
        (T_type::IDENT, "five"),
        (T_type::COMMA, ","),
        (T_type::IDENT, "ten"),
        (T_type::RPAREN, ")"),
        (T_type::SEMICOLON, ";"),
        (T_type::BANG, "!"),
        (T_type::MINUS, "-"),
        (T_type::SLASH, "/"),
        (T_type::ASTERISK, "*"),
        (T_type::INT, "5"),
        (T_type::SEMICOLON, ";"),
        (T_type::INT, "5"),
        (T_type::LT, "<"),
        (T_type::INT, "10"),
        (T_type::GT, ">"),
        (T_type::INT, "5"),
        (T_type::SEMICOLON, ";"),
        (T_type::IF, "if"),
        (T_type::LPAREN, "("),
        (T_type::INT, "5"),
        (T_type::LT, "<"),
        (T_type::INT, "10"),
        (T_type::RPAREN, ")"),
        (T_type::LBRACE, "{"),
        (T_type::RETURN, "return"),
        (T_type::TRUE, "true"),
        (T_type::SEMICOLON, ";"),
        (T_type::RBRACE, "}"),
        (T_type::ELSE, "else"),
        (T_type::LBRACE, "{"),
        (T_type::RETURN, "return"),
        (T_type::FALSE, "false"),
        (T_type::SEMICOLON, ";"),
        (T_type::RBRACE, "}"),
        (T_type::INT, "10"),
        (T_type::EQ, "=="),
        (T_type::INT, "10"),
        (T_type::SEMICOLON, ";"),
        (T_type::INT, "10"),
        (T_type::NOTEQ, "!="),
        (T_type::INT, "9"),
        (T_type::SEMICOLON, ";"),
        (T_type::EOF, ""),
    ];

    let mut l = Lexer::new(&input);

    for (i, test_run) in expected_results.iter().enumerate() {
        let result = l.next_token();

        println!(
            "Index {}: got Type = {:?}, Literal = {:?} | expected Type = {:?}, Literal = {:?}",
            i, result.Type, result.Literal, test_run.0, test_run.1
        );
        assert_eq!(
            &result.Type, &test_run.0,
            "Type mismatch at index {}: got{:?}, expected {:?}",
            i, result.Type, test_run.0
        );
        assert_eq!(
            &result.Literal, &test_run.1,
            "Type mismatch at index {}: got{:?}, expected{:?}",
            i, result.Literal, test_run.1
        );
    }
}
