use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::Runtime;
use crate::lexer::Token;
use crate::value::Value;

pub type EnvRef = Rc<RefCell<Environment>>;

#[derive(Debug, PartialEq)]
pub struct Environment {
    enclosing: Option<EnvRef>,
    pub values: RefCell<HashMap<String, Value>>,
}

impl Environment {
    pub fn new(enclosing: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Environment {
            enclosing,
            values: RefCell::new(HashMap::new()),
        }))
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.borrow_mut().insert(name, value);
    }

    pub fn get(&mut self, name: &Token) -> Result<Value, Runtime> {
        match self.values.borrow().get(&name.value) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(parent) => parent.borrow_mut().get(name),
                None => Err(Runtime::Error(format!(
                    "undefined variable '{}' at {:?}",
                    name.value, name
                ))),
            },
        }
    }

    pub fn assign(
        &mut self,
        name: &Token,
        value: Value,
    ) -> Result<(), Runtime> {
        if self.values.borrow().contains_key(&name.value) {
            self.values.borrow_mut().insert(name.value.clone(), value);
            Ok(())
        } else {
            match &self.enclosing {
                Some(parent) => parent.borrow_mut().assign(name, value),
                None => Err(Runtime::Error(format!(
                    "undefined variable '{}' at {:?}",
                    name.value, name
                ))),
            }
        }
    }
}
