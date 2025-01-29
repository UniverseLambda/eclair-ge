mod lexer;
mod parser;

fn main() {
    println!("Eclair Game Engine Compiler v0.1");

    let file = std::fs::File::open("test.bb").unwrap();

    let lexer = lexer::Tokenizer::new(file);
    let mut parser = parser::Parser::new(lexer);

    let program = parser.parse_program().unwrap();

    println!("PROGRAM: {program:?}");
}
