#[macro_use]
extern crate lazy_static;

mod ast;
mod builtins;
mod environment;
mod es;
mod interpreter;
mod lexer;
mod parser;
mod value;

use es::{Config, Es};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let config = Config::build(&args);

    let mut es = Es::new(config);

    if args.len() > 1 {
        es.file(&args[1]);
    } else {
        es.repl();
    }
}
