use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::{Literal, Stmt};
use crate::environment::EnvRef;
use crate::environment::Environment;
use crate::interpreter::{Interpreter, Runtime, RuntimeResult};
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,

    Builtin {
        name: String,
        call: fn(Vec<Value>) -> Result<Value, String>,
    },
    Function(Rc<RefCell<Function>>),
    List(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<Object>>),
}

pub trait Callable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult;
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
    pub closure: EnvRef,
}

pub type FunctionRef = Rc<RefCell<Function>>;

impl Function {
    pub fn new(
        name: Option<String>,
        params: Vec<Token>,
        body: Vec<Stmt>,
        closure: EnvRef,
    ) -> FunctionRef {
        Rc::new(RefCell::new(Function {
            name,
            params,
            body,
            closure,
        }))
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult {
        if self.params.len() != args.len() {
            return Err(Runtime::Error(format!(
                "expected {} arguments, got {}",
                self.params.len(),
                args.len()
            )));
        }

        let env = Environment::new(Some(Rc::clone(&self.closure)));
        for (param, arg) in self.params.iter().zip(args.iter()) {
            env.borrow_mut().define(param.value.clone(), arg.clone());
        }

        match interpreter.exec_block_with_closure(&self.body, env) {
            Ok(value) => Ok(value),
            Err(Runtime::Return(value, _)) => Ok(value),
            Err(e) => Err(e),
        }
    }

    pub fn bind(&self, instance: Object) -> Value {
        let env = Environment::new(Some(Rc::clone(&self.closure)));
        env.borrow_mut().define(
            "this".to_string(),
            Value::Object(Rc::new(RefCell::new(instance))),
        );

        Value::Function(Function::new(
            self.name.clone(),
            self.params.clone(),
            self.body.clone(),
            env,
        ))
    }
}

pub type ObjectRef = Rc<RefCell<Object>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub name: Option<String>,
    pub props: Rc<RefCell<HashMap<String, Value>>>,
    pub proto: Option<ObjectRef>,
}

impl Default for Object {
    fn default() -> Self {
        Object {
            name: None,
            props: Rc::new(RefCell::new(HashMap::new())),
            proto: None,
        }
    }
}

impl Object {
    pub fn new(
        name: Option<String>,
        props: HashMap<String, Value>,
        proto: Option<Object>,
    ) -> ObjectRef {
        Rc::new(RefCell::new(Object {
            name,
            props: Rc::new(RefCell::new(props)),
            proto: proto.map(|proto| Rc::new(RefCell::new(proto))),
        }))
    }

    pub fn get(&self, key: impl Into<String>) -> Option<Value> {
        let key = key.into();

        if key == "__proto" {
            return self
                .proto
                .as_ref()
                .map(|proto| Value::Object(proto.clone()));
        }

        self.props
            .borrow()
            .get(&key)
            .map(|value| match value {
                Value::Function(func) => func.borrow().bind(self.clone()),
                _ => value.clone(),
            })
            .or_else(|| {
                self.proto
                    .as_ref()
                    .and_then(|proto| proto.borrow().get(key))
            })
    }

    pub fn set(
        &mut self,
        key: impl Into<String>,
        value: Value,
    ) -> Result<(), String> {
        let key = key.into();

        if key == "__proto" {
            self.proto = match value {
                Value::Object(obj) => Some(obj),
                _ => return Err("__proto must be an object".into()),
            };
        } else {
            self.props.borrow_mut().insert(key, value);
        }
        Ok(())
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult {
        match self.get("__call") {
            Some(Value::Function(func)) => {
                func.borrow().call(interpreter, args)
            }
            _ => Err(Runtime::Error("object does not implement __call".into())),
        }
    }

    #[allow(dead_code)]
    pub fn div(
        &self,
        interpreter: &mut Interpreter,
        other: Value,
    ) -> RuntimeResult {
        match self.get("__div") {
            Some(Value::Function(func)) => {
                func.borrow().call(interpreter, vec![other])
            }
            _ => Err(Runtime::Error("object does not implement __div".into())),
        }
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Nil => Value::Nil,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Builtin { name, .. } => write!(f, "<builtin {}>", name),
            Value::Function(func) => {
                write!(
                    f,
                    "<function {}>",
                    func.borrow()
                        .name
                        .as_ref()
                        .unwrap_or(&"lambda".to_string())
                )
            }
            Value::List(values) => {
                write!(f, "[")?;
                for (i, value) in values.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Value::Object(obj) => {
                write!(f, "{{ ")?;
                for (i, (key, value)) in
                    obj.borrow().props.borrow().iter().enumerate()
                {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, " }}")
            }
        }
    }
}
