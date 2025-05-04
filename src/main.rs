mod cli;
mod lexer;
mod num;
mod session;
mod source;
mod token;

use std::borrow::Cow;
use std::fs::OpenOptions;
use std::io::{self, Read};
use std::path::Path;
use std::process;
use codespan_reporting::files::SimpleFiles;
use clap::Parser;

use cli::Cli;
use lexer::Lexer;
use source::FileId;

fn load_file(path: &Path) -> io::Result<(SimpleFiles<Cow<'_, str>, String>, FileId)> {
    if let Some(ext) = path.extension() {
        match ext.to_str() {
            Some("quo") => Ok(()), // Quotient Text File
            Some("qir") => Ok(()), // Quotient Bytecode File
            _ => Err(io::Error::new(io::ErrorKind::InvalidInput, "expected `.quo` or `qir` file"))
        }?
    }
    
    let mut file = OpenOptions::new()
        .read(true)
        .open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let mut files = SimpleFiles::new();
    let main_id = files.add(path.to_string_lossy(), buf);

    Ok((files, main_id))
}

fn run() -> io::Result<()> {
    let cli = Cli::parse();

    // For debugging purposes
    let filename = cli.file.as_deref().unwrap_or("stdin");
    let compile_target = cli.compile.as_deref().unwrap_or("--");
    let profile = if cli.release { "release" } else { "dev" };
    let stdpath = cli.stdpath.as_deref().unwrap_or("default");
    let build_dir = cli.build_dir.as_deref().unwrap_or("./");

    println!("file:   {:?}\ncompile: {:?}\nprofile: {:?}\nstdpath: {:?}\nbuild:   {:?}", 
        filename, compile_target, profile, stdpath, build_dir);

    // Start REPL if no given file
    if let None = &cli.file {
        todo!()
    }

    // Otherwise, Read from file
    let (file_map, main_id) = load_file(Path::new(filename))?;

    // Lex file
    let mut lexer = Lexer::new(file_map.get(main_id).unwrap().source(), main_id);

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {err}");
        process::exit(1);
    }
}
