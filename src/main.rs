mod cli;
mod error;
mod lexer;
mod num;
mod session;
mod source;
mod token;

use std::error::{Error};
use std::fmt::Write;
use std::fs::OpenOptions;
use std::io::{self, BufWriter, Read};
use std::path::Path;
use std::process;
use std::sync::{Arc, RwLock};
use clap::Parser;
use miette::{Diagnostic, GraphicalReportHandler, GraphicalTheme};

use cli::Cli;
use lexer::Lexer;
use source::{FileId, SourceMap};
use token::Token;

fn load_file(path: &Path) -> io::Result<(Arc<RwLock<SourceMap>>, FileId)> {
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

    let mut files = SourceMap::new();
    let main_id = files.add(path.to_string_lossy().into_owned(), &buf);

    Ok((Arc::new(RwLock::new(files)), main_id))
}

fn run() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

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
    let session = load_file(Path::new(filename))?;
    let mut diagnostics = Vec::new();

    // Lex file
    let mut lexer = Lexer::new(session);
    let mut tokens = Vec::<Token>::with_capacity(64);
    
    lexer.lex_token(&mut diagnostics, &mut tokens);

    println!("== Tokens ==\n{:#?}\n==Diagnostics==", tokens);

    // let writer = StandardStream::stderr(ColorChoice::Always);
    // let mut config = codespan_reporting::term::Config::default();
    // config.chars = Chars::ascii();
    // config.chars.source_border_left_break = '·';

    // Set Miette to only use ASCII styles
    // miette::set_hook(Box::new(|_| {
    //     Box::new(GraphicalReportHandler::new()
    //         .with_theme(GraphicalTheme::ascii()))
    // }))?;
    // theme.characters.

    miette::set_hook(Box::new(|_| {
        Box::new(GraphicalReportHandler::new()
            .with_theme({
                let mut theme = GraphicalTheme::ascii();
                theme.characters.vbar_break = '·';
                
                theme
            }))
    }))?;

    for diag in diagnostics {
        let inner = Arc::clone(&diag.0);

        eprintln!("{:?}", miette::Report::new(diag).with_source_code(inner.get_source_text()));
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {err}");
        process::exit(1);
    }
}
