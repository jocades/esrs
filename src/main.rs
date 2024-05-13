use std::{cell::RefCell, collections::HashMap};

use es::Es;

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,

    Builtin(fn(Vec<Value>) -> Value),
}

// trying to create the values for my language
// lists, strings and object literals are all objects with attrs and methods
// the methods are just functions that take the object as the first argument
// and return a value
// so i can call them like `list.push(1)`

struct Object {
    attrs: HashMap<String, Value>,
}

impl Object {
    fn new() -> Self {
        Self {
            attrs: HashMap::new(),
        }
    }

    fn get(&self, key: &str) -> Option<&Value> {
        self.attrs.get(key)
    }

    fn set(&mut self, key: String, value: Value) {
        self.attrs.insert(key, value);
    }

    fn to_string(&self) -> String {
        format!("{:?}", self.attrs)
    }
}

struct List {
    items: Vec<Value>,
}

impl List {
    fn new() -> Self {
        Self { items: Vec::new() }
    }

    fn push(&mut self, value: Value) {
        self.items.push(value);
    }

    fn pop(&mut self) -> Option<Value> {
        self.items.pop()
    }

    fn to_string(&self) -> String {
        format!("{:?}", self.items)
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 2 {
        println!("Usage: ene [script]");
        std::process::exit(64);
    }

    let mut es = Es::new();

    if args.len() == 2 {
        es.file(&args[1]);
    } else {
        es.repl()
    }
}
