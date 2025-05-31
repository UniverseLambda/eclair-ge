use log::{Level, debug, error, info};
use semantical::analyze_program;
use std::time::SystemTime;

use clap::Parser;

mod codegen;
mod lexer;
mod parser;
mod semantical;

#[derive(Debug, clap::Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// To output the AST (parser and semantical analysis results) in JSON format
    #[arg(short, long, default_value_t = false)]
    json_dump: bool,
    /// To output the LLVM IR
    #[arg(short, long, default_value_t = false)]
    llvm_ir_dump: bool,
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

    info!(
        "Compiling file {}...",
        args.path.as_os_str().to_string_lossy()
    );

    let file = match std::fs::File::open(&args.path) {
        Ok(v) => v,
        Err(err) => {
            error!(
                "{}: could not open file: {err}",
                args.path.to_string_lossy()
            );
            std::process::exit(1);
        }
    };

    let lexer = lexer::Tokenizer::new(args.path.to_string_lossy().to_string(), file);
    let mut parser = parser::Parser::new(lexer);

    let program = match parser.parse_program() {
        Ok(v) => v,
        Err(err) => {
            error!("{}", err.root_cause());
            std::process::exit(1);
        }
    };

    if args.json_dump {
        let content = serde_json::to_string_pretty(&program).unwrap();

        let mut new_file_name = args.path.file_name().unwrap().to_os_string();

        new_file_name.push(".ast.json");

        std::fs::write(new_file_name, content).expect("Could not write JSON output");
    }

    let analyzed_program = match analyze_program(program) {
        Ok(v) => v,
        Err(err) => {
            error!("{}: {}", args.path.to_string_lossy(), err.root_cause());
            debug!("BACKTRACE:");
            debug!("{}", err.backtrace());
            std::process::exit(1);
        }
    };

    if args.json_dump {
        let content = serde_json::to_string_pretty(&analyzed_program).unwrap();

        let mut new_file_name = args.path.file_name().unwrap().to_os_string();

        new_file_name.push(".sem.json");

        std::fs::write(new_file_name, content).expect("Could not write JSON output");
    }

    match codegen::codegen(analyzed_program) {
        Ok(v) => v,
        Err(err) => {
            error!("{}", err.root_cause());
            std::process::exit(1);
        }
    };

    // if args.llvm_ir_dump {

    // }

    let duration = SystemTime::now().duration_since(start_time).unwrap();

    info!(
        "Done compiling (took {:.3} seconds)",
        duration.as_secs_f32()
    );
}
