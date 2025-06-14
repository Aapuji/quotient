mod ast;
mod cli;
mod lexer;
mod num;
mod parser;
mod session;
mod source;
mod token;

use std::borrow::Cow;
use std::error::Error;
use std::fs::OpenOptions;
use std::io::{self, Read};
use std::path::Path;
use std::process;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::{self, Chars};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use cli::Cli;
use lexer::Lexer;
use session::Session;
use source::FileId;

use crate::parser::Parser;

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

fn run() -> Result<(), Box<dyn Error>> {
    let cli = { use clap::Parser; Cli::parse() };

    // For debugging purposes
    let filename = cli.file.as_deref().unwrap_or("stdin");
    let compile_target = cli.compile.as_deref().unwrap_or("--");
    let profile = if cli.release { "release" } else { "dev" };
    let stdpath = cli.stdpath.as_deref().unwrap_or("default");
    let build_dir = cli.build_dir.as_deref().unwrap_or("./");

    println!("file:   {:?}\ncompile: {:?}\nprofile: {:?}\nstdpath: {:?}\nbuild:   {:?}\n", 
        filename, compile_target, profile, stdpath, build_dir);

    // Start REPL if no given file
    if let None = &cli.file {
        todo!()
    }

    // Otherwise, Read from file
    let mut session = {
        let (source_map, main_id) = load_file(Path::new(filename))?;

        Session::new(source_map, main_id, vec![])
    };

    // Lex file
    let mut lexer = Lexer::new(&mut session);
    let (tokens, mut diagnostics) = lexer.lex();
    session.append_diagnostics(&mut diagnostics);

    // Parse file
    let mut parser = Parser::new(session.main_src(), *session.main_id(), &tokens);
    let (asts, mut diagnostics) = parser.parse();
    session.append_diagnostics(&mut diagnostics);

    // Result logging
    println!("== Tokens ==\n{:#?}\n== Asts ==\n{:#?}\n== Diagnostics ==", tokens, asts);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let mut config = codespan_reporting::term::Config::default();
    config.chars = Chars::ascii();
    config.chars.source_border_left_break = '·';
    config.chars.snippet_start = String::from("\x08-->");

    for diagnostic in session.diagnostics() {
        term::emit(&mut writer.lock(), &config, session.source_map(), &diagnostic)?;
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {err}");
        process::exit(1);
    }
}
