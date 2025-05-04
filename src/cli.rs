use std::path::{Path, PathBuf};
use clap::{Parser, arg};


#[derive(Parser)]
#[command(version, about, long_about=None)]
pub struct Cli {
    /// File to execute (or REPL mode from stdin)
    file: Option<PathBuf>, 

    /// Use release mode
    #[arg(long)]
    release: bool,

    /// Compile to bytecode with given name
    #[arg(short, long, value_name="NAME")]
    compile: Option<PathBuf>,

    /// Set path to standard library
    #[arg(long, value_name="PATH")]
    stdpath: Option<PathBuf>
}

impl Cli {
    pub fn filename(&self) -> Option<&Path> {
        self.file.as_deref()
    }

    pub fn is_release_profile(&self) -> bool {
        self.release
    }

    pub fn compile_mode(&self) -> Option<&Path> {
        self.compile.as_deref()
    }

    pub fn stdpath(&self) -> Option<&Path> {
        self.stdpath.as_deref()
    }
}
