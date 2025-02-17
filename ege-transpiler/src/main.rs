use clap::Parser;

mod lexer;
mod parser;

#[derive(Debug, clap::Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// To output the AST in JSON format
    #[arg(short, long, default_value_t = false)]
    json_ast: bool,
    /// Output path [default: a.out]
    #[arg(short, long)]
    output: Option<std::path::PathBuf>,
    /// Path to the file to compile
    path: std::path::PathBuf,

}

fn main() {
    eprintln!("Eclair Game Engine Compiler v0.1");

    let args: Cli = Cli::parse();


    let file = std::fs::File::open(&args.path).unwrap();

    let lexer = lexer::Tokenizer::new(args.path.to_string_lossy().to_string(), file);
    let mut parser = parser::Parser::new(lexer);

    let program = parser.parse_program().unwrap();

    if args.json_ast {
        let content = serde_json::to_string_pretty(&program).unwrap();

        let mut new_file_name = args.path.file_name().unwrap().to_os_string();

        new_file_name.push(".json");

        std::fs::write(new_file_name, content).expect("Could not write JSON output");
    }

    println!("Done compiling");
}
