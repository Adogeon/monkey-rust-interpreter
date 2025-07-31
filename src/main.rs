pub mod ast;
pub mod lexer;
pub mod repl;
pub mod token;

use std::io::{stdin, stdout};

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    let input = stdin();
    repl::start(input.lock(), stdout().lock())
}
