mod cli;
mod lexer;
mod num;
mod source;
mod token;

use std::path;

use clap::Parser;
use cli::Cli;

fn main() {
    let cli = Cli::parse();

    println!("file:   {:?}\ncompile: {:?}\nprofile: {:?}\nstdpath: {:?}", 
        if let Some(f) = cli.filename() {f} else {path::Path::new("stdin")}, 
        if let Some(n) = cli.compile_mode() {n} else {path::Path::new("--")},
        if cli.is_release_profile() {"release"} else {"dev"},
        if let Some(p) = cli.stdpath() {p} else {path::Path::new("default")}
    );
}
