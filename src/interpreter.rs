use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Expr, Stmt};
use crate::builtins::BUILTINS;
use crate::environment::{EnvRef, Environment};
use crate::lexer::{Token, TokenKind};
use crate::value::Value;
use crate::value::{Function, Object};

pub struct Interpreter {
    pub env: EnvRef,
    pub config: Config,
}

pub struct Config {
    pub repl: bool,
}

pub enum Runtime {
    Error(String),
    Break(String),
    Return(Value, String),
}

pub type RuntimeResult = Result<Value, Runtime>;

impl Interpreter {
    pub fn new() -> Interpreter {
        let globals = Environment::new(None);
        for (name, func) in BUILTINS.iter() {
            globals.borrow_mut().define(
                name.to_string(),
                Value::Builtin {
                    name: name.to_string(),
                    call: *func,
                },
            );
        }
        globals
            .borrow_mut()
            .define("PI".to_string(), Value::Number(std::f64::consts::PI));

        let locals = Environment::new(Some(Rc::clone(&globals)));

        Interpreter {
            env: Rc::clone(&locals),
            config: Config { repl: false },
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), String> {
        let mut result = Value::Nil;
        for stmt in stmts {
            match self.execute(stmt) {
                Ok(value) => result = value,
                Err(Runtime::Error(msg)) => return Err(msg),
                Err(Runtime::Break(msg)) => return Err(msg),
                Err(Runtime::Return(_, msg)) => return Err(msg),
            }
        }
        if self.config.repl {
            println!("{}", result);
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> RuntimeResult {
        match stmt {
            Stmt::Expr(expr) => self.evaluate(expr),

            Stmt::Block(stmts) => {
                let env = Environment::new(Some(Rc::clone(&self.env)));
                self.exec_block_with_closure(stmts, env)
            }

            Stmt::Break(_token) => Err(Runtime::Break(
                "break statement used outside of loop".into(),
            )),

            Stmt::For {
                variable,
                iterable,
                body,
            } => {
                let iterable = self.evaluate(iterable)?;
                self.exec_for(variable, iterable, body)
            }

            Stmt::Function { name, body } => {
                match body.as_ref() {
                    Expr::Function { params, body } => {
                        self.env.borrow_mut().define(
                            name.value.clone(),
                            Value::Function(Rc::new(RefCell::new(Function {
                                name: Some(name.value.clone()),
                                params: params.clone(),
                                body: body.clone(),
                                closure: Rc::clone(&self.env),
                            }))),
                        );
                    }
                    _ => unreachable!("expected function body expression"),
                }
                Ok(Value::Nil)
            }

            Stmt::If {
                branches,
                else_branch,
            } => {
                for (condition, body) in branches {
                    if is_truthy(&self.evaluate(condition)?) {
                        return self.exec_block(body);
                    }
                }
                match else_branch {
                    Some(body) => self.exec_block(body),
                    None => Ok(Value::Nil),
                }
            }

            Stmt::Let {
                variables,
                initializers,
            } => {
                for (var, init) in variables.iter().zip(initializers) {
                    let value = match init {
                        Some(expr) => self.evaluate(expr)?,
                        None => Value::Nil,
                    };
                    self.env.borrow_mut().define(var.clone(), value);
                }
                Ok(Value::Nil)
            }

            Stmt::Loop(body) => {
                loop {
                    match self.exec_block(body) {
                        Ok(_) => {}
                        Err(Runtime::Break(_)) => break,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Nil)
            }

            Stmt::Return(_, value) => {
                let value = match value {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };
                Err(Runtime::Return(
                    value,
                    "return statement used outside of function".into(),
                ))
            }

            Stmt::While { condition, body } => {
                while is_truthy(&self.evaluate(condition)?) {
                    for stmt in body {
                        match self.execute(stmt) {
                            Ok(_) => {}
                            Err(Runtime::Break(_)) => break,
                            Err(e) => return Err(e),
                        }
                    }
                }
                Ok(Value::Nil)
            }
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> RuntimeResult {
        match expr {
            Expr::Assign { name, op, value } => {
                self.eval_assignment(name, op, value)
            }

            Expr::Call { callee, args } => {
                if let Expr::Get { object, name } = callee.as_ref() {
                    let mut list_methods: HashMap<
                        &str,
                        fn(
                            Rc<RefCell<Vec<Value>>>,
                            Vec<Value>,
                            interpreter: &mut Interpreter,
                        ) -> RuntimeResult,
                    > = HashMap::new();

                    list_methods.insert("push", |items, args, _| {
                        if args.len() != 1 {
                            return Err(Runtime::Error(format!(
                                "expected 1 argument, got {}",
                                args.len()
                            )));
                        }
                        items.borrow_mut().push(args[0].clone());
                        Ok(Value::Nil)
                    });

                    list_methods.insert("pop", |items, args, _| {
                        if !args.is_empty() {
                            return Err(Runtime::Error(format!(
                                "expected 0 arguments, got {}",
                                args.len()
                            )));
                        }
                        Ok(items.borrow_mut().pop().unwrap_or(Value::Nil))
                    });

                    list_methods.insert("map", |items, args, interpreter| {
                        if args.len() != 1 {
                            return Err(Runtime::Error(format!(
                                "expected 1 argument, got {}",
                                args.len()
                            )));
                        }

                        let Value::Function(func) = &args[0] else {
                            return Err(Runtime::Error(
                                "expected function argument".into(),
                            ));
                        };

                        let mut new_items = Vec::new();
                        for (i, item) in items.borrow().iter().enumerate() {
                            let value = func.borrow().call(
                                interpreter,
                                vec![item.clone(), Value::Number(i as f64)],
                            )?;
                            new_items.push(value);
                        }
                        Ok(Value::List(Rc::new(RefCell::new(new_items))))
                    });

                    list_methods.insert(
                        "for_each",
                        |items, args, interpreter| {
                            if args.len() != 1 {
                                return Err(Runtime::Error(format!(
                                    "expected 1 argument, got {}",
                                    args.len()
                                )));
                            }

                            let Value::Function(func) = &args[0] else {
                                return Err(Runtime::Error(
                                    "expected function argument".into(),
                                ));
                            };

                            for (i, item) in items.borrow().iter().enumerate() {
                                func.borrow().call(
                                    interpreter,
                                    vec![item.clone(), Value::Number(i as f64)],
                                )?;
                            }
                            Ok(Value::Nil)
                        },
                    );

                    match self.evaluate(object)? {
                        Value::List(items) => {
                            if let Some(method) =
                                list_methods.get(name.value.as_str())
                            {
                                return method(
                                    items,
                                    args.iter()
                                        .map(|arg| self.evaluate(arg))
                                        .collect::<Result<Vec<_>, _>>()?,
                                    self,
                                );
                            } else {
                                return Err(Runtime::Error(format!(
                                    "List has no method '{}'",
                                    name.value
                                )));
                            }
                        }
                        Value::String(s) => match name.value.as_str() {
                            "upper" => {
                                return Ok(Value::String(s.to_uppercase()));
                            }
                            "split" => {
                                if args.len() > 1 {
                                    return Err(Runtime::Error(format!(
                                        "expected 1 or 0 arguments, got {}",
                                        args.len()
                                    )));
                                }

                                let sep = if args.is_empty() {
                                    Ok(" ".to_string())
                                } else {
                                    match self.evaluate(&args[0])? {
                                        Value::String(s) => Ok(s.clone()),
                                        _ => Err(Runtime::Error(
                                            "expected string argument".into(),
                                        )),
                                    }
                                }?;
                                let parts = s
                                    .split(&sep)
                                    .map(|s| Value::String(s.into()));

                                return Ok(Value::List(Rc::new(RefCell::new(
                                    parts.collect(),
                                ))));
                            }
                            "is_number" => {
                                if args.len() != 0 {
                                    return Err(Runtime::Error(format!(
                                        "expected 0 arguments, got {}",
                                        args.len()
                                    )));
                                }
                                // accept 10 or 10.20
                                let is_number = s.parse::<f64>().is_ok();
                                return Ok(Value::Bool(is_number));
                            }
                            other => {
                                return Err(Runtime::Error(format!(
                                    "String has no method '{other}'",
                                )));
                            }
                        },
                        _ => (),
                    }
                }

                let callee = self.evaluate(callee)?;
                let args = args
                    .iter()
                    .map(|arg| self.evaluate(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                match callee {
                    Value::Builtin { call, .. } => {
                        call(args).map_err(Runtime::Error)
                    }
                    Value::Function(func) => func.borrow().call(self, args),
                    Value::Object(obj) => obj.borrow().call(self, args),
                    other => Err(Runtime::Error(format!(
                        "cannot call value: {other}"
                    ))),
                }
            }

            Expr::Function { params, body } => {
                Ok(Value::Function(Function::new(
                    None,
                    params.clone(),
                    body.clone(),
                    Rc::clone(&self.env),
                )))
            }

            Expr::Get { object, name } => match self.evaluate(object)? {
                Value::Object(obj) => {
                    Ok(obj.borrow().get(&name.value).unwrap_or(Value::Nil))
                }
                other => Err(Runtime::Error(format!(
                    "can only access properties of objects, found: {other}",
                ))),
            },

            Expr::GetComputed { object, prop } => {
                let object = self.evaluate(object)?;
                let prop = self.evaluate(prop)?;
                match object {
                    Value::List(values) => match prop {
                        Value::Number(n) => {
                            let index = n as usize;
                            if index < values.borrow().len() {
                                Ok(values.borrow()[index].clone())
                            } else {
                                Err(Runtime::Error(
                                    "index out of bounds".into(),
                                ))
                            }
                        }
                        _ => Err(Runtime::Error(
                            "invalid index, expected number".into(),
                        )),
                    },
                    Value::Object(obj) => match prop {
                        Value::String(key) => {
                            Ok(obj.borrow().get(&key).unwrap_or(Value::Nil))
                        }
                        _ => Err(Runtime::Error(
                            "property key must be a string".into(),
                        )),
                    },
                    other => Err(Runtime::Error(format!(
                        "can only access properties of objects, found: {other}",
                    ))),
                }
            }

            Expr::Literal(value) => Ok(value.into()),

            Expr::List(values) => Ok(Value::List(Rc::new(RefCell::new(
                values
                    .iter()
                    .map(|expr| self.evaluate(expr))
                    .collect::<Result<Vec<_>, _>>()?,
            )))),

            Expr::Logical { op, lhs, rhs } => {
                let lhs = self.evaluate(lhs)?;
                match op.kind {
                    TokenKind::Or => {
                        if is_truthy(&lhs) {
                            return Ok(lhs);
                        }
                    }
                    TokenKind::And => {
                        if !is_truthy(&lhs) {
                            return Ok(lhs);
                        }
                    }
                    _ => unreachable!("invalid logical operator"),
                }
                self.evaluate(rhs)
            }

            Expr::Object { props } => {
                let mut properties = HashMap::new();
                for (key, value) in props {
                    match value {
                        Some(value) => {
                            // { key: value }
                            properties.insert(
                                key.value.clone(),
                                self.evaluate(value)?,
                            );
                        }
                        None => {
                            // { key }
                            properties.insert(
                                key.value.clone(),
                                self.env.borrow_mut().get(&key)?,
                            );
                        }
                    }
                }
                Ok(Value::Object(Object::new(
                    None,
                    properties,
                    Some(Object::default()),
                )))
            }

            Expr::Range(start, end) => {
                let start = self.evaluate(start)?;
                let end = self.evaluate(end)?;
                self.eval_range(start, end)
            }

            Expr::Set {
                object,
                name,
                value,
            } => {
                let object = self.evaluate(object)?;
                let value = self.evaluate(value)?;
                match object {
                    Value::Object(obj) => {
                        obj.borrow_mut()
                            .set(&name.value, value.clone())
                            .map_err(Runtime::Error)?;
                        Ok(value)
                    }
                    _ => Err(Runtime::Error("invalid property access".into())),
                }
            }

            Expr::SetComputed {
                object,
                prop,
                value,
            } => {
                let object = self.evaluate(object)?;
                let prop = self.evaluate(prop)?;
                let value = self.evaluate(value)?;
                match object {
                    Value::List(values) => match prop {
                        Value::Number(n) => {
                            let index = n as usize;
                            if index < values.borrow().len() {
                                values.borrow_mut()[index] = value.clone();
                                Ok(value)
                            } else {
                                Err(Runtime::Error(
                                    "index out of bounds".into(),
                                ))
                            }
                        }
                        _ => Err(Runtime::Error(
                            "invalid index, expected number".into(),
                        )),
                    },
                    Value::Object(obj) => match prop {
                        Value::String(key) => {
                            obj.borrow_mut()
                                .set(&key, value.clone())
                                .map_err(Runtime::Error)?;
                            Ok(value)
                        }
                        _ => Err(Runtime::Error(
                            "property key must be a string".into(),
                        )),
                    },
                    other => Err(Runtime::Error(format!(
                        "can only access properties of objects, found: {other}",
                    ))),
                }
            }

            Expr::Variable(name) => self.env.borrow_mut().get(name),

            Expr::BinaryOp { op, lhs, rhs } => {
                let lhs = self.evaluate(lhs)?;
                let rhs = self.evaluate(rhs)?;
                self.eval_binop(op, lhs, rhs)
            }

            Expr::UnaryOp { op, rhs } => {
                let rhs = self.evaluate(rhs)?;
                self.eval_unary(op, rhs)
            }
            Expr::Noop => panic!("noop expression"),
        }
    }

    // -- STMT --
    fn exec_block(&mut self, stmts: &Vec<Stmt>) -> RuntimeResult {
        let mut result = Value::Nil;
        for stmt in stmts {
            match self.execute(stmt) {
                Ok(value) => result = value,
                Err(e) => return Err(e),
            }
        }
        Ok(result)
    }

    pub fn exec_block_with_closure(
        &mut self,
        stmts: &Vec<Stmt>,
        env: EnvRef,
    ) -> RuntimeResult {
        let prev_env = std::mem::replace(&mut self.env, env);
        let mut result = Value::Nil;
        for stmt in stmts {
            match self.execute(stmt) {
                Ok(value) => result = value,
                Err(e) => {
                    self.env = prev_env;
                    return Err(e);
                }
            }
        }
        self.env = prev_env;
        Ok(result)
    }

    fn exec_for(
        &mut self,
        variable: &Token,
        iterable: Value,
        body: &Vec<Stmt>,
    ) -> RuntimeResult {
        match iterable {
            Value::List(values) => {
                for value in values.borrow().iter() {
                    self.env
                        .borrow_mut()
                        .define(variable.value.clone(), value.clone());
                    self.exec_block(body)?;
                }
            }
            Value::String(s) => {
                for c in s.chars() {
                    self.env.borrow_mut().define(
                        variable.value.clone(),
                        Value::String(c.to_string()),
                    );
                    self.exec_block(body)?;
                }
            }
            Value::Object(obj) => {
                for (key, _) in obj.borrow().props.borrow().iter() {
                    self.env.borrow_mut().define(
                        variable.value.clone(),
                        Value::String(key.clone()),
                    );
                    self.exec_block(body)?;
                }
            }
            _ => return Err(Runtime::Error("expected iterable".to_string())),
        };
        Ok(Value::Nil)
    }

    // -- EXPR --
    fn eval_assignment(
        &mut self,
        name: &Token,
        op: &Token,
        value: &Expr,
    ) -> RuntimeResult {
        let value = self.evaluate(value)?;
        println!("eval_assignment: {op:?} -> {:?}", value);
        match op.kind {
            TokenKind::Equal => {
                self.env.borrow_mut().assign(name, value.clone())?;
                Ok(value)
            }
            TokenKind::PlusEqual => {
                let lhs = self.env.borrow_mut().get(name)?;

                let result = match (lhs, value) {
                    (Value::Number(x), Value::Number(y)) => {
                        Value::Number(x + y)
                    }
                    (Value::String(x), Value::String(y)) => {
                        Value::String(format!("{}{}", x, y))
                    }
                    _ => {
                        return Err(Runtime::Error(
                            "invalid operands for '+='".into(),
                        ));
                    }
                };

                self.env.borrow_mut().assign(name, result.clone())?;
                Ok(result)
            }
            TokenKind::MinusEqual => {
                let lhs = self.env.borrow_mut().get(name)?;

                let result = match (lhs, value) {
                    (Value::Number(x), Value::Number(y)) => {
                        Value::Number(x - y)
                    }
                    _ => {
                        return Err(Runtime::Error(
                            "invalid operands for '-='".into(),
                        ));
                    }
                };

                self.env.borrow_mut().assign(name, result.clone())?;
                Ok(result)
            }
            TokenKind::StarEqual => {
                let lhs = self.env.borrow_mut().get(name)?;

                let result = match (lhs, value) {
                    (Value::Number(x), Value::Number(y)) => {
                        Value::Number(x * y)
                    }
                    _ => {
                        return Err(Runtime::Error(
                            "invalid operands for '*='".into(),
                        ));
                    }
                };

                self.env.borrow_mut().assign(name, result.clone())?;
                Ok(result)
            }
            TokenKind::SlashEqual => {
                let lhs = self.env.borrow_mut().get(name)?;

                let result = match (lhs, value) {
                    (Value::Number(x), Value::Number(y)) => {
                        Value::Number(x / y)
                    }
                    _ => {
                        return Err(Runtime::Error(
                            "invalid operands for '/='".into(),
                        ));
                    }
                };

                self.env.borrow_mut().assign(name, result.clone())?;
                Ok(result)
            }
            _ => Err(Runtime::Error(format!(
                "unknown assignment operator: {:?}",
                op
            ))),
        }
    }

    fn eval_range(&self, start: Value, end: Value) -> RuntimeResult {
        match (start, end) {
            (Value::Number(start), Value::Number(end)) => {
                let elements = (start as i64..end as i64)
                    .map(|n| Value::Number(n as f64))
                    .collect();

                Ok(Value::List(Rc::new(RefCell::new(elements))))
            }
            _ => Err(Runtime::Error("invalid range".to_string())),
        }
    }

    fn eval_unary(&self, op: &Token, rhs: Value) -> RuntimeResult {
        use TokenKind::*;
        match op.kind {
            Bang => Ok(Value::Bool(!is_truthy(&rhs))),
            Minus => match rhs {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(Runtime::Error("invalid operand for '-'".to_string())),
            },
            _ => Err(Runtime::Error(format!("unknown operator: {:?}", op))),
        }
    }

    fn eval_binop(&self, op: &Token, lhs: Value, rhs: Value) -> RuntimeResult {
        use TokenKind::*;
        match op.kind {
            BangEqual => Ok(Value::Bool(!is_eq(&lhs, &rhs))),
            EqualEqual => Ok(Value::Bool(is_eq(&lhs, &rhs))),
            Greater => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x > y)),
                _ => {
                    Err(Runtime::Error("invalid operands for '>'".to_string()))
                }
            },
            GreaterEqual => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x >= y)),
                _ => {
                    Err(Runtime::Error("invalid operands for '>='".to_string()))
                }
            },
            Less => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x < y)),
                _ => {
                    Err(Runtime::Error("invalid operands for '<'".to_string()))
                }
            },
            LessEqual => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x <= y)),
                _ => {
                    Err(Runtime::Error("invalid operands for '<='".to_string()))
                }
            },
            Plus => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => {
                    Ok(Value::Number(x + y))
                }
                (Value::String(x), Value::String(y)) => {
                    Ok(Value::String(format!("{}{}", x, y)))
                }
                _ => {
                    Err(Runtime::Error("invalid operands for '+'".to_string()))
                }
            },
            Minus => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => {
                    Ok(Value::Number(x - y))
                }
                _ => {
                    Err(Runtime::Error("invalid operands for '-'".to_string()))
                }
            },
            Star => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => {
                    Ok(Value::Number(x * y))
                }
                _ => {
                    Err(Runtime::Error("invalid operands for '*'".to_string()))
                }
            },
            Slash => match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => {
                    Ok(Value::Number(x / y))
                }
                _ => {
                    Err(Runtime::Error("invalid operands for '/'".to_string()))
                }
            },

            _ => Err(Runtime::Error(format!("unknown operator: {:?}", op))),
        }
    }
}

fn is_eq(x: &Value, y: &Value) -> bool {
    match (x, y) {
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Number(x), Value::Number(y)) => {
            (x - y).abs() < std::f64::EPSILON
        }
        _ => false,
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Bool(b) => *b,
        _ => true,
    }
}
