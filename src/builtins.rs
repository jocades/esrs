use crate::value::Value;

lazy_static! {
    pub static ref BUILTINS: Vec<(&'static str, fn(Vec<Value>) -> Result<Value, String>)> = vec![
        ("echo", |args| {
            for arg in args {
                print!("{} ", arg);
            }
            println!();
            Ok(Value::Nil)
        }),
        ("time", |args| {
            if !args.is_empty() {
                return Err(format!("expected 0 arguments, got {}", args.len()));

            }
            use std::time;
            let now = time::SystemTime::now();
            let since_epoch = now.duration_since(time::UNIX_EPOCH).unwrap();
            Ok(Value::Number(since_epoch.as_millis() as f64))
        }),
        ("type", |args| {
            if args.len() != 1 {
                return Err(format!("expected 1 argument, got {}", args.len()));
            }
            match args[0] {
                Value::Number(_) => Ok(Value::String("number".to_string())),
                Value::String(_) => Ok(Value::String("string".to_string())),
                Value::Bool(_) => Ok(Value::String("bool".to_string())),
                Value::Nil => Ok(Value::String("nil".to_string())),
                Value::Function { .. } | Value::Builtin { .. } => {
                    Ok(Value::String("function".to_string()))
                }
                Value::Object{..} => Ok(Value::String("object".to_string())),
                Value::List(_) => Ok(Value::String("list".to_string())),
            }
        }),
        ("len", |args| {
            if args.len() != 1 {
                return Err(format!("expected 1 argument, got {}", args.len()));
            }
            match &args[0] {
                Value::String(s) => Ok(Value::Number(s.len() as f64)),
                Value::Object(obj) => {
                    let len = obj.borrow().props.borrow().len();
                    Ok(Value::Number(len as f64))
                }
                Value::List(list) => Ok(Value::Number(list.borrow().len() as f64)),
                _ => Err(format!("expected iterable, got {:?}", args[0])),
            }
        }),
        // sleep(seconds)
        ("sleep", |args| {
            if args.len() != 1 {
                return Err(format!("expected 1 argument, got {}", args.len()));
            }
            match args[0] {
                Value::Number(n) => {
                    std::thread::sleep(std::time::Duration::from_secs_f64(n));
                    Ok(Value::Nil)
                }
                _ => Err("expected number".to_string()),
            }
        }),
        ("num", |args| {
            if args.len() != 1 {
                return Err(format!("expected 1 argument, got {}", args.len()));
            }
            match &args[0] {
                Value::String(s) => match s.parse::<f64>() {
                    Ok(n) => Ok(Value::Number(n)),
                    Err(_) => Err(format!("invalid number '{}'", s)),
                },
                _ => Err("expected string".to_string()),
            }
        }),
        ("str", |args| {
            if args.len() != 1 {
                return Err(format!("expected 1 argument, got {}", args.len()));
            }
            Ok(Value::String(format!("{}", args[0])))
        }),
        ("import", |args| {
            if args.len() != 1 {
                return Err(format!("expected 1 argument, got {}", args.len()));
            }
            match &args[0] {
                Value::String(path) => module::load(path),
                _ => Err("expected string".to_string()),


            }
        })

    ];
}

mod module {
    use std::{cell::RefCell, path::Path, rc::Rc};

    use crate::{
        interpreter::Interpreter,
        lexer::Lexer,
        parser::Parser,
        value::{Object, Value},
    };

    pub fn load(path: &str) -> Result<Value, String> {
        let path = std::path::Path::new(path);

        if path.starts_with("std") {
            let key = path.file_stem().unwrap().to_str().unwrap();
            match key {
                "io" => return Ok(load_io()),
                _ => {
                    return Err(format!(
                        "module '{}' not found",
                        path.display()
                    ))
                }
            }
        }

        validate(path)?;

        let source = std::fs::read_to_string(path).unwrap();
        let tokens = Lexer::new(&source).lex()?;
        let (stmts, errors) = Parser::new(tokens).parse();

        if !errors.is_empty() {
            return Err(errors.join("\n"));
        }

        let mut interpreter = Interpreter::new();
        interpreter.interpret(&stmts)?;

        let mut module = Object::default();
        for (name, value) in interpreter.env.borrow().values.borrow().iter() {
            module.set(name, value.clone())?;
        }

        Ok(Value::Object(Rc::new(RefCell::new(module))))
    }

    pub fn validate(path: &Path) -> Result<(), String> {
        if !path.exists() {
            return Err(format!("module '{}' not found", path.display()));
        }
        if !path.is_file() {
            return Err(format!("module '{}' is not a file", path.display()));
        }
        Ok(())
    }

    pub fn load_io() -> Value {
        use std::io::Write;
        let mut io_module = Object::default();
        io_module.name = Some("IO".into());
        io_module
            .set(
                "input".to_string(),
                Value::Builtin {
                    name: "input".to_string(),
                    call: |args: Vec<Value>| {
                        for arg in args {
                            print!("{}", arg);
                        }
                        std::io::stdout().flush().unwrap();
                        let mut input = String::new();
                        std::io::stdin().read_line(&mut input).unwrap();
                        Ok(Value::String(input.trim().to_string()))
                    },
                },
            )
            .unwrap();
        Value::Object(Rc::new(RefCell::new(io_module)))
    }
}
