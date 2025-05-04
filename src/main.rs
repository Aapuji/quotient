mod lexer;
mod num;
mod source;
mod token;

use std::env;

fn main() {
    let mut args = env::args().skip(1);
    println!("Hello, world!");
}
