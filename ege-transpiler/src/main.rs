use std::time::SystemTime;
use log::{debug, info, Level};

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

    #[arg(short, long, default_value_t = false)]
    verbose: bool,

    /// Path to the file to compile
    path: std::path::PathBuf,
}

fn main() {
    let start_time = SystemTime::now();
    let args: Cli = Cli::parse();


    let level = if args.verbose {
        Level::Trace
    } else {
        Level::Info
    };

    clang_log::init(level, "eclairc");

    debug!("Eclair Game Engine Compiler v0.1");

    info!("Compiling file {}...", args.path.as_os_str().to_string_lossy());

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

    let duration = SystemTime::now().duration_since(start_time).unwrap();

    info!("Done compiling (took {:.3} seconds)", duration.as_secs_f32());
}
