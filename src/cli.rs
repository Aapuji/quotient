use clap::{Parser, arg};

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// File to execute (or REPL mode from stdin)
    pub file: Option<String>, 

    /// Use release mode
    #[arg(long)]
    pub release: bool,

    /// Compile to bytecode with given name
    #[arg(short, long, value_name = "NAME")]
    pub compile: Option<String>,

    /// Set path to standard library
    #[arg(long, value_name = "PATH")]
    pub stdpath: Option<String>,

    /// Set directory to check for .qir files
    #[arg(short = 'B', long = "build-dir", value_name = "DIR")]
    pub build_dir: Option<String>
}
