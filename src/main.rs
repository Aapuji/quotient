mod cli;
mod lexer;
mod num;
mod source;
mod token;

use std::fs::OpenOptions;
use std::io::Read;
use std::{io, path};
use std::str::{FromStr};
use codespan_reporting::files::SimpleFiles;

use clap::Parser;
use cli::Cli;
use lexer::Lexer;

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    // For debugging purposes
    println!("file:   {:?}\ncompile: {:?}\nprofile: {:?}\nstdpath: {:?}", 
        if let Some(f) = &cli.file {format!("{f:?}")} else {"stdin".to_owned()}, 
        if let Some(n) = &cli.compile {format!("{n:?}")} else {"--".to_owned()},
        if cli.release {"release"} else {"dev"},
        if let Some(p) = &cli.stdpath {format!("{p:?}")} else {"default".to_owned()}
    );

    // Start REPL if no given file
    if let None = &cli.file {
        todo!()
    }

    // Otherwise, read from files and do work
    let mut file_map = SimpleFiles::new();
    let main_file_id;

    // Read from file
    let Some(f) = &cli.file 
        else { unreachable!() };
    let mut file = OpenOptions::new()
        .read(true)
        .open(&f)?;

    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    main_file_id = file_map.add(f.to_string_lossy(), buf);

    // Lex file
    let mut lexer = Lexer::new(file_map.get(main_file_id).unwrap().source(), main_file_id);

    Ok(())
}
