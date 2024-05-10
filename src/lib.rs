#[macro_use]
extern crate lazy_static;

mod ast {
    use std::rc::Rc;

    use super::lexer::Token;

    #[derive(Debug, PartialEq)]
    pub enum Stmt {
        Expr(Expr),
        Block(Vec<Stmt>),
        Let {
            names: Vec<String>,
            initializers: Vec<Option<Expr>>,
        },
        While {
            condition: Rc<Expr>,
            body: Rc<Vec<Stmt>>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum Expr {
        Literal(Literal),
        Variable(String),

        BinaryOp {
            op: Token,
            lhs: Rc<Expr>,
            rhs: Rc<Expr>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum Literal {
        Number(f64),
        String(String),
        Bool(bool),
        Nil,
    }
}

// lexer -> tokens -> parser -> ast -> interpreter
mod lexer {
    use std::collections::HashMap;

    #[rustfmt::skip]
    #[derive(Debug, PartialEq, Clone)]
    pub enum TokenKind {
        // Single character tokens
        LParen, RParen, LBrace, RBrace, LBracket, RBracket,
        Caret, Colon, Comma, Dot, Minus, Plus,
        Question, Semicolon, Slash, Star,

        // One or two character tokens
        Bang, BangEqual,
        Equal, EqualEqual,
        Greater, GreaterEqual,
        Less, LessEqual,
        DotDot, Arrow,

        // Literals
        Identifier, Number, Str,

        // Keywords
        And, Break, Class, Elif, Else, False,
        Fn, For, If, In, Let, Nil, Or, Return,
        Super, This, True, While,

        EOF,
    }

    lazy_static! {
        static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
            use TokenKind::*;

            let mut m = HashMap::new();
            m.insert("and", And);
            m.insert("break", Break);
            m.insert("class", Class);
            m.insert("else", Else);
            m.insert("elif", Elif);
            m.insert("false", False);
            m.insert("fn", Fn);
            m.insert("for", For);
            m.insert("if", If);
            m.insert("in", In);
            m.insert("let", Let);
            m.insert("nil", Nil);
            m.insert("or", Or);
            m.insert("return", Return);
            m.insert("super", Super);
            m.insert("this", This);
            m.insert("true", True);
            m.insert("while", While);
            m
        };
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Token {
        pub kind: TokenKind,
        pub value: String,
        pub line: usize,
        pub column: usize,
    }

    pub struct Lexer {
        source: String,
        tokens: Vec<Token>,
        start: usize,
        index: usize,
        line: usize,
        column: usize,
    }

    impl Lexer {
        pub fn new(source: String) -> Lexer {
            Lexer {
                source,
                tokens: Vec::new(),
                start: 0,
                index: 0,
                line: 1,
                column: 1,
            }
        }

        pub fn lex(&mut self) -> Result<Vec<Token>, String> {
            while !self.eof() {
                self.start = self.index;
                self.scan()?;
            }
            self.token(TokenKind::EOF);

            Ok(self.tokens.to_vec())
        }

        fn scan(&mut self) -> Result<(), String> {
            use TokenKind::*;

            let c = self.eat();
            match c {
                // literals
                '"' => self.string()?,
                '0'..='9' => self.number(),
                'a'..='z' | 'A'..='Z' | '_' => self.identifier(),

                // operators
                '+' => self.token(Plus),
                '*' => self.token(Star),
                '/' => {
                    if self.matchn('/') {
                        while self.at() != '\n' && !self.eof() {
                            self.eat();
                        }
                    } else {
                        self.token(Slash);
                    }
                }
                '-' => {
                    if self.matchn('>') {
                        self.token(Arrow)
                    } else {
                        self.token(Minus)
                    }
                }
                // single-character tokens
                '(' => self.token(LParen),
                ')' => self.token(RParen),
                '{' => self.token(LBrace),
                '}' => self.token(RBrace),
                '[' => self.token(LBracket),
                ']' => self.token(RBracket),
                '.' => self.token(Dot),
                ',' => self.token(Comma),
                ':' => self.token(Colon),
                ';' => self.token(Semicolon),
                '^' => self.token(Caret),
                // two-character tokens
                '!' => {
                    if self.matchn('=') {
                        self.token(BangEqual)
                    } else {
                        self.token(Bang)
                    }
                }
                '=' => {
                    if self.matchn('=') {
                        self.token(EqualEqual)
                    } else {
                        self.token(Equal)
                    }
                }
                '<' => {
                    if self.matchn('=') {
                        self.token(LessEqual)
                    } else {
                        self.token(Less)
                    }
                }
                '>' => {
                    if self.matchn('=') {
                        self.token(GreaterEqual)
                    } else {
                        self.token(Greater)
                    }
                }
                // ignore whitespace
                ' ' | '\t' | '\r' => {}
                // ignore newline
                '\n' => {
                    self.line += 1;
                }
                _ => {
                    return Err(format!("unexpected character '{}'", c));
                }
            }

            Ok(())
        }

        fn number(&mut self) {
            while self.at().is_digit(10) {
                self.eat();
            }

            if self.at() == '.' && self.peek().is_digit(10) {
                self.eat();
                while self.at().is_digit(10) {
                    self.eat();
                }
            }

            self.token(TokenKind::Number);
        }

        fn string(&mut self) -> Result<(), String> {
            while self.at() != '"' && !self.eof() {
                if self.at() == '\n' {
                    self.line += 1;
                }
                self.eat();
            }

            if self.eof() {
                return Err("unterminated string".to_string());
            }

            self.eat();

            let value = &self.source[self.start + 1..self.index - 1];
            self.token_value(TokenKind::Str, value.to_string());
            Ok(())
        }

        fn identifier(&mut self) {
            while self.at().is_alphanumeric() || self.at() == '_' {
                self.eat();
            }

            let value = &self.source[self.start..self.index];
            let kind = KEYWORDS
                .get(value)
                .cloned()
                .unwrap_or(TokenKind::Identifier);

            self.token(kind);
        }

        fn at(&self) -> char {
            if self.eof() {
                return '\0';
            }
            self.source.chars().nth(self.index).unwrap()
        }

        fn eat(&mut self) -> char {
            self.index += 1;
            self.source.chars().nth(self.index - 1).unwrap()
        }

        fn peek(&self) -> char {
            self.source.chars().nth(self.index + 1).unwrap()
        }

        fn matchn(&mut self, expected: char) -> bool {
            if self.eof() {
                return false;
            } else if self.peek() != expected {
                return false;
            }
            self.eat();
            true
        }

        fn expect(&mut self, expected: char) {
            let c = self.source.chars().nth(self.index).unwrap();
            if c != expected {
                panic!("expected '{}', but got '{}'", expected, c);
            }
            self.eat();
        }

        fn token(&mut self, kind: TokenKind) {
            let value = &self.source[self.start..self.index];
            self.tokens.push(Token {
                kind,
                value: value.to_string(),
                line: self.line,
                column: self.column,
            });
        }

        fn token_value(&mut self, kind: TokenKind, value: String) {
            self.tokens.push(Token {
                kind,
                value,
                line: self.line,
                column: self.column,
            })
        }

        fn eof(&self) -> bool {
            self.index >= self.source.len()
        }
    }
}

mod parser {
    use std::rc::Rc;

    use super::ast::{Expr, Literal, Stmt};
    use super::lexer::{Token, TokenKind};

    pub struct Parser {
        tokens: Vec<Token>,
        index: usize,
    }

    type ParseResult<T> = Result<T, &'static str>;

    use TokenKind::*;

    impl Parser {
        pub fn new(tokens: Vec<Token>) -> Parser {
            Parser { tokens, index: 0 }
        }

        pub fn parse(&mut self) -> (Vec<Stmt>, Vec<String>) {
            let mut program = Vec::new();
            let mut errors = Vec::new();

            loop {
                if self.eof() {
                    break;
                }

                match self.declaration() {
                    Ok(stmt) => program.push(stmt),
                    Err(msg) => {
                        errors.push(format!("Parse error: {}", msg));
                        self.synchronyze();
                    }
                }
            }

            (program, errors)
        }

        fn declaration(&mut self) -> ParseResult<Stmt> {
            match self.at().kind {
                TokenKind::Let => {
                    self.eat();
                    self.let_declaration()
                }
                _ => self.statement(),
            }
        }

        fn let_declaration(&mut self) -> ParseResult<Stmt> {
            let mut names = Vec::new();
            loop {
                names.push(
                    self.expect(&Identifier, "expected identifier")
                        .value
                        .clone(),
                );
                if !self.matches(&[Comma]) {
                    break;
                }
            }

            let mut initializers = Vec::new();
            if self.matches(&[Equal]) {
                loop {
                    let expr = self.expression()?;
                    initializers.push(Some(expr));

                    if !self.matches(&[Comma]) {
                        break;
                    }
                }

                if names.len() != initializers.len() {
                    panic!(
                        "Expected {} initializers, got {}",
                        names.len(),
                        initializers.len()
                    );
                }
            } else {
                for _ in 0..names.len() {
                    initializers.push(None);
                }
            }

            Ok(Stmt::Let {
                names,
                initializers,
            })
        }

        fn block(&mut self) -> ParseResult<Vec<Stmt>> {
            let mut statements = Vec::new();
            loop {
                if self.check(&RBrace) || self.eof() {
                    break;
                }
                statements.push(self.declaration()?);
            }
            self.expect(&RBrace, "expected '}' after block");

            Ok(statements)
        }

        fn statement(&mut self) -> ParseResult<Stmt> {
            match self.at().kind {
                While => {
                    self.eat();
                    self.while_statement()
                }
                LBrace => {
                    self.eat();
                    Ok(Stmt::Block(self.block()?))
                }

                _ => self.expression_statement(),
            }
        }

        fn while_statement(&mut self) -> ParseResult<Stmt> {
            let condition = self.expression()?;
            self.expect(&LBrace, "Expected '{' after while condition.");
            let body = self.block()?;
            Ok(Stmt::While {
                condition: Rc::new(condition),
                body: Rc::new(body),
            })
        }

        fn expression_statement(&mut self) -> ParseResult<Stmt> {
            let expr = self.expression()?;
            if self.matches(&[Semicolon]) {}
            Ok(Stmt::Expr(expr))
        }

        fn expression(&mut self) -> ParseResult<Expr> {
            self.assignment()
        }

        fn assignment(&mut self) -> ParseResult<Expr> {
            self.term()
        }

        fn term(&mut self) -> ParseResult<Expr> {
            let mut expr = self.factor()?;

            while self.matches(&[Plus, Minus]) {
                let op = self.prev().clone();
                let rhs = self.factor()?;
                expr = Expr::BinaryOp {
                    op,
                    lhs: Rc::new(expr),
                    rhs: Rc::new(rhs),
                };
            }

            Ok(expr)
        }

        fn factor(&mut self) -> ParseResult<Expr> {
            let mut expr = self.primary()?;

            while self.matches(&[Star, Slash]) {
                let op = self.prev().clone();
                let rhs = self.primary()?;
                expr = Expr::BinaryOp {
                    op,
                    lhs: Rc::new(expr),
                    rhs: Rc::new(rhs),
                };
            }

            Ok(expr)
        }

        fn primary(&mut self) -> ParseResult<Expr> {
            match self.at().kind {
                True => {
                    self.eat();
                    Ok(Expr::Literal(Literal::Bool(true)))
                }
                False => {
                    self.eat();
                    Ok(Expr::Literal(Literal::Bool(false)))
                }
                Nil => {
                    self.eat();
                    Ok(Expr::Literal(Literal::Nil))
                }
                Number => {
                    let val = self.eat().value.parse::<f64>().unwrap();
                    Ok(Expr::Literal(Literal::Number(val)))
                }
                Str => {
                    let val = self.eat().value.clone();
                    Ok(Expr::Literal(Literal::String(val)))
                }
                Identifier => {
                    let val = self.eat().value.clone();
                    Ok(Expr::Variable(val))
                }
                LParen => {
                    self.eat();
                    let expr = self.expression()?;
                    self.expect(&RParen, "expected ')' after expression");
                    Ok(expr)
                }
                _ => Err("expected primary expression"),
            }
        }

        fn at(&self) -> &Token {
            &self.tokens[self.index]
        }

        fn eat(&mut self) -> &Token {
            if !self.eof() {
                self.index += 1;
            }
            self.prev()
        }

        fn peek(&self) -> &Token {
            &self.tokens[self.index + 1]
        }

        fn prev(&self) -> &Token {
            &self.tokens[self.index - 1]
        }

        fn check(&self, kind: &TokenKind) -> bool {
            self.at().kind == *kind
        }

        fn matches(&mut self, kinds: &[TokenKind]) -> bool {
            for kind in kinds {
                if self.check(&kind) {
                    self.eat();
                    return true;
                }
            }
            false
        }

        fn expect(&mut self, expected: &TokenKind, message: &str) -> &Token {
            if self.check(expected) {
                self.eat()
            } else {
                panic!("{} at {:?}", message, self.at())
            }
        }

        fn eof(&self) -> bool {
            self.at().kind == EOF
        }

        fn synchronyze(&mut self) {
            self.eat();

            while !self.eof() {
                if self.prev().kind == Semicolon {
                    return;
                }

                match self.at().kind {
                    Let | While => return,
                    _ => {}
                }

                self.eat();
            }
        }
    }
}

mod value {
    use std::fmt;

    use super::ast::Literal;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Value {
        Number(f64),
        String(String),
        Bool(bool),
        Nil,
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
            }
        }
    }
}

mod environment {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use super::value::Value;

    pub type EnvRef = Rc<RefCell<Env>>;

    #[derive(Debug)]
    pub struct Env {
        enclosing: Option<EnvRef>,
        values: RefCell<HashMap<String, Value>>,
    }

    impl Env {
        pub fn new(enclosing: Option<EnvRef>) -> EnvRef {
            Rc::new(RefCell::new(Env {
                enclosing,
                values: RefCell::new(HashMap::new()),
            }))
        }

        pub fn define(&self, name: String, value: Value) {
            self.values.borrow_mut().insert(name, value);
        }

        pub fn get(&self, name: &str) -> Result<Value, String> {
            match self.values.borrow().get(name) {
                Some(value) => Ok(value.clone()),
                None => match &self.enclosing {
                    Some(parent) => parent.borrow().get(name),
                    None => Err(format!("undefined variable '{}'", name)),
                },
            }
        }

        pub fn assign(&self, name: &str, value: Value) -> Result<(), String> {
            if self.values.borrow().contains_key(name) {
                self.values.borrow_mut().insert(name.to_string(), value);
                Ok(())
            } else {
                match &self.enclosing {
                    Some(parent) => parent.borrow().assign(name, value),
                    None => Err(format!("undefined variable '{}'", name)),
                }
            }
        }
    }
}

mod interpreter {
    use std::rc::Rc;

    use super::ast::{Expr, Stmt};
    use super::environment::{Env, EnvRef};
    use super::lexer::{Token, TokenKind};
    use super::value::Value;

    pub struct Interpreter {
        pub env: EnvRef,
    }

    impl Interpreter {
        pub fn new() -> Interpreter {
            Interpreter {
                env: Env::new(None),
            }
        }

        pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), String> {
            for stmt in stmts {
                match self.execute(&stmt) {
                    Ok(value) => println!("{:#?}", value),
                    Err(msg) => return Err(format!("Runtime error: {}", msg)),
                }
            }
            Ok(())
        }

        fn execute(&mut self, stmt: &Stmt) -> Result<Value, String> {
            match stmt {
                Stmt::Expr(expr) => self.evaluate(expr),
                Stmt::Block(stmts) => {
                    let env = Env::new(Some(Rc::clone(&self.env)));
                    self.eval_block(&stmts, env)
                }
                Stmt::Let {
                    names,
                    initializers,
                } => {
                    for (name, initializer) in names.iter().zip(initializers) {
                        let value = match initializer {
                            Some(expr) => self.evaluate(expr)?,
                            None => Value::Nil,
                        };
                        self.env.borrow().define(name.clone(), value);
                    }
                    Ok(Value::Nil)
                }
                _ => Err("unknown statement".to_string()),
            }
        }

        fn evaluate(&mut self, expr: &Expr) -> Result<Value, String> {
            match expr {
                Expr::Literal(value) => Ok(value.into()),
                Expr::Variable(name) => self.env.borrow().get(name),
                Expr::BinaryOp { op, lhs, rhs } => {
                    let lhs = self.evaluate(lhs)?;
                    let rhs = self.evaluate(rhs)?;
                    self.eval_binop(op, lhs, rhs)
                }
            }
        }

        // -- STMT --
        fn eval_block(
            &mut self,
            stmts: &Vec<Stmt>,
            env: EnvRef,
        ) -> Result<Value, String> {
            let prev = Rc::clone(&self.env);
            self.env = env;
            let mut result = Value::Nil;
            for stmt in stmts {
                result = self.execute(&stmt)?;
            }
            self.env = prev;
            Ok(result)
        }

        // -- EXPR --

        fn eval_binop(
            &self,
            op: &Token,
            lhs: Value,
            rhs: Value,
        ) -> Result<Value, String> {
            use TokenKind::*;
            match op.kind {
                BangEqual => Ok(Value::Bool(!is_eq(&lhs, &rhs))),
                EqualEqual => Ok(Value::Bool(is_eq(&lhs, &rhs))),
                Greater => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x > y))
                    }
                    _ => Err("invalid operands for >".to_string()),
                },
                GreaterEqual => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x >= y))
                    }
                    _ => Err("invalid operands for >=".to_string()),
                },
                Less => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x < y))
                    }
                    _ => Err("invalid operands for <".to_string()),
                },
                LessEqual => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x <= y))
                    }
                    _ => Err("invalid operands for <=".to_string()),
                },
                Plus => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x + y))
                    }
                    (Value::String(x), Value::String(y)) => {
                        Ok(Value::String(format!("{}{}", x, y)))
                    }
                    _ => Err("invalid operands for +".to_string()),
                },
                Minus => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x - y))
                    }
                    _ => Err("invalid operands for -".to_string()),
                },
                Star => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x * y))
                    }
                    _ => Err("invalid operands for *".to_string()),
                },
                Slash => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x / y))
                    }
                    _ => Err("invalid operands for /".to_string()),
                },

                _ => Err(format!("unknown operator: {:?}", op)),
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
}

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

pub struct Es {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
}

impl Es {
    pub fn new() -> Self {
        Es {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(),
        }
    }

    pub fn repl(&mut self) {
        use std::io::{self, Write};
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut buffer = String::new();
            match io::stdin().read_line(&mut buffer) {
                Ok(0) => break,
                Ok(_) => {
                    if buffer.starts_with('.') {
                        self.command(&buffer);
                        continue;
                    }
                    self.run(buffer.clone());
                    self.had_error = false;
                }
                _ => break,
            }
        }
    }

    pub fn file(&mut self, path: &str) {
        let source = std::fs::read_to_string(path).unwrap();
        self.run(source);

        if self.had_error {
            std::process::exit(65);
        }

        if self.had_runtime_error {
            std::process::exit(70);
        }
    }

    fn run(&mut self, source: String) {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap_or_else(|msg| {
            self.error(msg);
            Vec::new()
        });
        println!("--> Tokens {:#?}", tokens);

        if self.had_error {
            return;
        }

        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse();
        println!("--> Program {:#?}", program);

        for error in errors {
            self.error(error.to_string());
        }

        if self.had_error {
            return;
        }

        self.interpreter.interpret(program).unwrap_or_else(|msg| {
            self.runtime_error(msg);
        });
    }

    pub fn error(&mut self, msg: String) {
        eprintln!("{}", msg);
        self.had_error = true;
    }

    pub fn runtime_error(&mut self, msg: String) {
        eprintln!("{}", msg);
        self.had_runtime_error = true;
    }

    fn command(&mut self, buffer: &str) {
        let command = buffer.trim();
        match command {
            ".exit" => std::process::exit(0),
            ".env" => println!("{:#?}", self.interpreter.env.borrow()),
            _ => println!("Unknown command: {}", command),
        }
    }
}
