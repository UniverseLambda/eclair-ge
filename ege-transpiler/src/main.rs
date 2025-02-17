mod lexer;
mod parser;

fn main() {
    eprintln!("Eclair Game Engine Compiler v0.1");

    let file = std::fs::File::open("test_parser.bb").unwrap();

    let lexer = lexer::Tokenizer::new(file);
    let mut parser = parser::Parser::new(lexer);

    let program = parser.parse_program().unwrap();

    eprintln!("PROGRAM: {program:#?}");

    println!("{}", serde_json::to_string_pretty(&program).unwrap())
}
