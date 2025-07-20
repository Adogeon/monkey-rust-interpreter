pub mod lexer;
pub mod token;

fn main() {
    let input = "c;".to_string();
    let mut l = lexer::Lexer::new(&input);
    while let token = l.next_token() {
        println!("{:?}", token);
        if token.Type == token::T_type::EOF {
            break;
        }
    }
}
