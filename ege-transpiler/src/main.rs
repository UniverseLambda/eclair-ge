mod lexer;
mod parser;

fn main() {
    println!("Eclair Game Engine Compiler v0.1");

    let file = std::fs::File::open("test.bb").unwrap();

    let mut lexer = lexer::Tokenizer::new(file);

    while let Some(token) = lexer.next_token().unwrap() {
        println!("TOKEN: {token:?}");
    }
}
