#![allow(dead_code)]

// lexer -> tokens -> parser -> ast -> interpreter

use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub struct Es {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
    pub config: Config,
}

pub struct Config {
    pub debug: bool,
    pub ast: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            debug: false,
            ast: false,
        }
    }
}

impl Es {
    pub fn new() -> Self {
        Es {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(),
            config: Config::default(),
        }
    }

    pub fn file(&mut self, path: &str) {
        let source = std::fs::read_to_string(path).unwrap();
        self.run(&source);

        if self.had_error {
            std::process::exit(65);
        }

        if self.had_runtime_error {
            std::process::exit(70);
        }
    }

    pub fn repl(&mut self) {
        self.interpreter.config.repl = true;

        use std::io::{self, Write};
        loop {
            print!("[es]> ");
            io::stdout().flush().unwrap();
            let mut buffer = String::new();
            match io::stdin().read_line(&mut buffer) {
                Ok(0) => break,
                Ok(_) => {
                    if buffer.starts_with('.') {
                        self.command(&buffer);
                        continue;
                    }
                    self.run(&buffer);
                    self.had_error = false;
                }
                _ => break,
            }
        }
    }

    fn run(&mut self, source: &str) {
        let tokens = Lexer::new(source).lex().unwrap_or_else(|msg| {
            self.error(msg);
            vec![]
        });

        if self.config.debug {
            println!("--> Tokens {:#?}", tokens);
        }

        if self.had_error {
            return;
        }

        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse();

        if self.config.debug || self.config.ast {
            println!("--> Program {:#?}", program);
        }

        if !errors.is_empty() {
            for error in errors {
                self.error(error);
            }
            return;
        }

        self.interpreter.interpret(&program).unwrap_or_else(|msg| {
            self.runtime_error(msg);
        });
    }

    pub fn error(&mut self, msg: String) {
        eprintln!("{}", msg);
        self.had_error = true;
    }

    pub fn runtime_error(&mut self, msg: String) {
        eprintln!("Runtime error: {}", msg);
        self.had_runtime_error = true;
    }

    fn command(&mut self, buffer: &str) {
        let command = buffer.trim();
        match command {
            ".exit" => std::process::exit(0),
            ".env" => println!("{:#?}", self.interpreter.env.borrow()),
            ".debug" => self.config.debug = !self.config.debug,
            ".ast" => self.config.ast = !self.config.ast,
            _ => println!("Unknown command: {command}"),
        }
    }
}
