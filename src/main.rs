use steel_lang::run_file;
use steel_lang::stdlib;
use std::env::args;
use std::fs;

fn main() {
    let source_path = args().nth(1).expect("No source file provided.");
    let mut source = fs::read_to_string(source_path.clone()).expect("Failed to read source file.");
    source += stdlib::get_prelude();
    let mode = args().nth(2).unwrap_or_else(|| "run".to_string());
    let force = args().any(|arg| arg == "-f");
    let debug = args().any(|arg| arg == "-d");

    run_file(&source_path, source.as_str(), debug, mode.as_str(), force);
}
