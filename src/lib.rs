#[macro_use]
extern crate lazy_static;

mod ast {
    use std::rc::Rc;

    use super::lexer::Token;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Stmt {
        Expr(Expr),
        Block(Vec<Stmt>),
        For {
            variable: Token,
            iterable: Rc<Expr>,
            body: Vec<Stmt>,
        },
        Function {
            name: Token,
            body: Rc<Expr>,
        },
        Let {
            variables: Vec<String>,
            initializers: Vec<Option<Expr>>,
        },
        While {
            condition: Rc<Expr>,
            body: Vec<Stmt>,
        },
        Break,
        Return(Token, Option<Expr>),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Expr {
        Assign {
            name: Token,
            value: Rc<Expr>,
        },
        BinaryOp {
            op: Token,
            lhs: Rc<Expr>,
            rhs: Rc<Expr>,
        },
        Call {
            callee: Rc<Expr>,
            args: Vec<Expr>,
        },
        Function {
            params: Vec<Token>,
            body: Vec<Stmt>,
        },
        Get {
            object: Rc<Expr>,
            name: Token,
        },
        GetComputed {
            object: Rc<Expr>,
            prop: Rc<Expr>,
        },
        Literal(Literal),
        List(Vec<Expr>),
        Object {
            props: Vec<(Token, Option<Expr>)>,
        },
        Range(Rc<Expr>, Rc<Expr>),
        Set {
            object: Rc<Expr>,
            name: Token,
            value: Rc<Expr>,
        },
        SetComputed {
            object: Rc<Expr>,
            prop: Rc<Expr>,
            value: Rc<Expr>,
        },
        UnaryOp {
            op: Token,
            rhs: Rc<Expr>,
        },
        Variable(Token),

        Noop,
    }

    #[derive(Debug, PartialEq, Clone)]
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

    pub struct Lexer<'a> {
        source: &'a str,
        tokens: Vec<Token>,
        start: usize,
        index: usize,
        line: usize,
        column: usize,
    }

    impl Lexer<'_> {
        pub fn new(source: &'_ str) -> Lexer {
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
            self.add_token(TokenKind::EOF);

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
                '+' => self.add_token(Plus),
                '*' => self.add_token(Star),
                '/' => {
                    if self.matches('/') {
                        while self.at() != '\n' && !self.eof() {
                            self.eat();
                        }
                    } else {
                        self.add_token(Slash);
                    }
                }
                '-' => {
                    if self.matches('>') {
                        self.add_token(Arrow)
                    } else {
                        self.add_token(Minus)
                    }
                }
                // single-character tokens
                '(' => self.add_token(LParen),
                ')' => self.add_token(RParen),
                '{' => self.add_token(LBrace),
                '}' => self.add_token(RBrace),
                '[' => self.add_token(LBracket),
                ']' => self.add_token(RBracket),
                '.' => {
                    if self.matches('.') {
                        self.add_token(DotDot)
                    } else {
                        self.add_token(Dot)
                    }
                }
                ',' => self.add_token(Comma),
                ':' => self.add_token(Colon),
                ';' => self.add_token(Semicolon),
                '^' => self.add_token(Caret),
                '?' => self.add_token(Question),
                // two-character tokens
                '!' => {
                    if self.matches('=') {
                        self.add_token(BangEqual)
                    } else {
                        self.add_token(Bang)
                    }
                }
                '=' => {
                    if self.matches('=') {
                        self.add_token(EqualEqual)
                    } else {
                        self.add_token(Equal)
                    }
                }
                '<' => {
                    if self.matches('=') {
                        self.add_token(LessEqual)
                    } else {
                        self.add_token(Less)
                    }
                }
                '>' => {
                    if self.matches('=') {
                        self.add_token(GreaterEqual)
                    } else {
                        self.add_token(Greater)
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

            self.add_token(TokenKind::Number);
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
            self.add_token_value(TokenKind::Str, value.to_string());
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

            self.add_token(kind);
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

        fn matches(&mut self, expected: char) -> bool {
            if self.eof() {
                return false;
            } else if self.at() != expected {
                return false;
            }
            self.eat();
            true
        }

        fn add_token(&mut self, kind: TokenKind) {
            let value = &self.source[self.start..self.index];
            self.tokens.push(Token {
                kind,
                value: value.to_string(),
                line: self.line,
                column: self.column,
            });
        }

        fn add_token_value(&mut self, kind: TokenKind, value: String) {
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

    type ParseResult<T> = Result<T, String>;

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
                        errors.push(msg);
                        self.synchronyze();
                    }
                }
            }

            (program, errors)
        }

        fn declaration(&mut self) -> ParseResult<Stmt> {
            use TokenKind::*;
            match self.at().kind {
                Let => {
                    self.eat();
                    self.let_declaration()
                }
                Fn => {
                    if self.match_next(&Identifier) {
                        self.function_declaration("function")
                    } else {
                        Err(self.error("expected function declaration"))
                    }
                }
                _ => self.statement(),
            }
        }

        fn let_declaration(&mut self) -> ParseResult<Stmt> {
            let mut variables = Vec::new();
            loop {
                variables.push(
                    self.expect(&Identifier, "expected identifier")?
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

                if variables.len() != initializers.len() {
                    return Err(self.error(&format!(
                        "expected {} initializers, got {}",
                        variables.len(),
                        initializers.len()
                    )));
                }
            } else {
                for _ in 0..variables.len() {
                    initializers.push(None);
                }
            }

            Ok(Stmt::Let {
                variables,
                initializers,
            })
        }

        fn function_declaration(&mut self, kind: &str) -> ParseResult<Stmt> {
            let name = self.expect(
                &Identifier,
                &format!("expected {} after name", kind),
            )?;

            Ok(Stmt::Function {
                name: name.clone(),
                body: Rc::new(self.function_body(kind)?),
            })
        }

        fn function_body(&mut self, kind: &str) -> ParseResult<Expr> {
            self.expect(&LParen, &format!("expected '(' ater {} name", kind))?;
            let params = self.function_params()?;
            match self.eat().kind {
                LBrace => Ok(Expr::Function {
                    params,
                    body: self.block()?,
                }),
                // Arrow => Ok(Expr::Function {
                //     params,
                //     body: vec![self.expression()?],
                // }),
                _ => {
                    Err(format!("expected '{{' or '->'  before {} body", kind))
                }
            }
        }

        fn function_params(&mut self) -> ParseResult<Vec<Token>> {
            let mut params = Vec::new();
            if !self.check(&RParen) {
                loop {
                    let token = self
                        .expect(&Identifier, "expected parameter name")?
                        .clone();
                    params.push(token);

                    if !self.matches(&[Comma]) {
                        break;
                    }
                }
            }
            self.expect(&RParen, "Expected ')' afer parameters")?;

            Ok(params)
        }

        fn block(&mut self) -> ParseResult<Vec<Stmt>> {
            let mut statements = Vec::new();
            while !self.check(&RBrace) && !self.eof() {
                statements.push(self.declaration()?);
            }
            self.expect(&RBrace, "expected '}' after block")?;

            Ok(statements)
        }

        fn statement(&mut self) -> ParseResult<Stmt> {
            match self.at().kind {
                For => {
                    self.eat();
                    self.for_statement()
                }
                If => {
                    todo!()
                }
                While => {
                    self.eat();
                    self.while_statement()
                }
                Break => {
                    self.eat();
                    Ok(Stmt::Break)
                }
                Return => {
                    self.eat();
                    self.return_statement()
                }
                LBrace => {
                    self.eat();
                    Ok(Stmt::Block(self.block()?))
                }

                _ => self.expression_statement(),
            }
        }

        fn for_statement(&mut self) -> ParseResult<Stmt> {
            // for x in 1..10 {}
            let variable = self
                .expect(&Identifier, "expected identifier after 'for'")?
                .clone();
            self.expect(&In, "expected 'in' after identifier")?;

            let mut iterable = self.expression()?;

            println!("for_statement: {:?}", iterable);
            println!("for_statement: {:?}", self.at());

            if self.check(&DotDot) {
                println!("range");
                if self.is_noop(&iterable) {
                    iterable = Expr::Literal(Literal::Number(0.0))
                }
                self.expect(&DotDot, "expected '..' after start expression")?;
                let end = self.expression()?;
                self.check_noop(&end)?;
                iterable = Expr::Range(Rc::new(iterable), Rc::new(end));
            }

            self.expect(&LBrace, "expected '{' after for loop expression")?;

            Ok(Stmt::For {
                variable,
                iterable: Rc::new(iterable),
                body: self.block()?,
            })
        }

        fn while_statement(&mut self) -> ParseResult<Stmt> {
            let condition = self.expression()?;
            self.expect(&LBrace, "Expected '{' after while condition")?;
            let body = self.block()?;
            Ok(Stmt::While {
                condition: Rc::new(condition),
                body,
            })
        }

        fn return_statement(&mut self) -> ParseResult<Stmt> {
            let keyword = self.prev().clone();
            let value = self.expression()?;

            Ok(Stmt::Return(
                keyword,
                if self.is_noop(&value) {
                    None
                } else {
                    Some(value)
                },
            ))
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
            let expr = self.object()?;

            if self.matches(&[Equal]) {
                let value = self.assignment()?;
                self.check_noop(&value)?;

                match expr {
                    Expr::Variable(name) => {
                        return Ok(Expr::Assign {
                            name,
                            value: Rc::new(value),
                        });
                    }
                    Expr::Get { object, name } => {
                        return Ok(Expr::Set {
                            object,
                            name,
                            value: Rc::new(value),
                        });
                    }
                    Expr::GetComputed { object, prop } => {
                        return Ok(Expr::SetComputed {
                            object,
                            prop,
                            value: Rc::new(value),
                        });
                    }
                    _ => return Err(self.error("invalid assignment target")),
                }
            }

            Ok(expr)
        }

        fn object(&mut self) -> ParseResult<Expr> {
            if !self.matches(&[LBrace]) {
                return self.list();
            }

            let mut props = Vec::new();

            while self.at().kind != RBrace {
                let key = self
                    .expect(&Identifier, "expected identifier key")?
                    .clone();

                if self.matches(&[Comma]) {
                    props.push((key, None));
                    continue;
                } else if self.at().kind == RBrace {
                    // allows shorthand key: pair -> { key }
                    props.push((key, None));
                    break;
                }

                self.expect(&Colon, "expected ':' after object key")?;

                let value = self.expression()?;
                props.push((key, Some(value)));

                if self.at().kind != RBrace {
                    self.expect(&Comma, "expected ',' after object value")?;
                }
            }

            self.expect(&RBrace, "expected '}' after object declaration")?;

            Ok(Expr::Object { props })
        }

        fn list(&mut self) -> ParseResult<Expr> {
            if !self.matches(&[LBracket]) {
                return self.equality();
            }

            let mut values = Vec::new();

            while self.at().kind != RBracket {
                values.push(self.expression()?);
                if !self.matches(&[Comma]) {
                    break;
                }
            }

            self.expect(&RBracket, "Expected ']' after list declaration")?;

            Ok(Expr::List(values))
        }

        fn equality(&mut self) -> ParseResult<Expr> {
            let mut expr = self.comparison()?;

            while self.matches(&[BangEqual, EqualEqual]) {
                let op = self.prev().clone();
                let right = self.comparison()?;
                expr = Expr::BinaryOp {
                    op,
                    lhs: Rc::new(expr),
                    rhs: Rc::new(right),
                };
            }

            Ok(expr)
        }

        fn comparison(&mut self) -> ParseResult<Expr> {
            let mut expr = self.term()?;

            while self.matches(&[Greater, GreaterEqual, Less, LessEqual]) {
                let op = self.prev().clone();
                let right = self.term()?;
                expr = Expr::BinaryOp {
                    op,
                    lhs: Rc::new(expr),
                    rhs: Rc::new(right),
                };
            }

            Ok(expr)
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
            let mut expr = self.unary()?;

            while self.matches(&[Star, Slash]) {
                let op = self.prev().clone();
                let rhs = self.unary()?;
                expr = Expr::BinaryOp {
                    op,
                    lhs: Rc::new(expr),
                    rhs: Rc::new(rhs),
                };
            }

            Ok(expr)
        }

        fn unary(&mut self) -> ParseResult<Expr> {
            if self.matches(&[Bang, Minus]) {
                let op = self.prev().clone();
                let rhs = self.unary()?;
                return Ok(Expr::UnaryOp {
                    op,
                    rhs: Rc::new(rhs),
                });
            }

            self.call()
        }

        fn call(&mut self) -> ParseResult<Expr> {
            let mut expr = self.primary()?;

            loop {
                if self.matches(&[LParen]) {
                    expr = self.finish_call(expr)?;
                } else if self.matches(&[Dot, LBracket]) {
                    let op = self.prev();
                    if op.kind == Dot {
                        let prop = self.expect(
                            &Identifier,
                            "expected property name after '.'",
                        )?;
                        expr = Expr::Get {
                            object: Rc::new(expr),
                            name: prop.clone(),
                        };
                    } else {
                        let prop = self.expression()?;
                        self.expect(
                            &RBracket,
                            "expected ']' after computed property",
                        )?;
                        expr = Expr::GetComputed {
                            object: Rc::new(expr),
                            prop: Rc::new(prop),
                        };
                    }
                } else {
                    break;
                }
            }

            Ok(expr)
        }

        fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
            let mut args: Vec<Expr> = Vec::new();
            if !self.check(&RParen) {
                loop {
                    args.push(self.expression()?);
                    if !self.matches(&[Comma]) {
                        break;
                    }
                }
            }
            self.expect(&RParen, "expected ')' after arguments")?;

            Ok(Expr::Call {
                callee: Rc::new(callee),
                args,
            })
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
                    let token = self.eat().clone();
                    Ok(Expr::Variable(token))
                }
                Fn => {
                    self.eat();
                    self.function_body("lambda")
                }
                LParen => {
                    self.eat();
                    let expr = self.expression()?;
                    self.expect(&RParen, "expected ')' after expression")?;
                    Ok(expr)
                }
                // _ => Err(self.error("expected expression")),
                _ => Ok(Expr::Noop),
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

        fn check_next(&self, kind: &TokenKind) -> bool {
            self.peek().kind == *kind
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

        fn match_next(&mut self, kind: &TokenKind) -> bool {
            if self.check_next(kind) {
                self.eat();
                return true;
            }
            false
        }

        fn expect(
            &mut self,
            expected: &TokenKind,
            message: &str,
        ) -> ParseResult<&Token> {
            if self.check(expected) {
                return Ok(self.eat());
            }
            Err(self.error(message))
        }

        fn is_noop(&self, expr: &Expr) -> bool {
            &Expr::Noop == expr
        }

        fn check_noop(&self, expr: &Expr) -> Result<(), String> {
            if self.is_noop(expr) {
                return Err(self.error("expected expression"));
            }
            Ok(())
        }

        fn error(&self, message: &str) -> String {
            format!("Parse error: {} at {:?}.", message, self.at())
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
                    Let | Fn | Break | If | While | For | Return => return,
                    _ => {}
                }

                self.eat();
            }
        }
    }
}

mod value {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::fmt;
    use std::rc::Rc;

    use super::ast::{Literal, Stmt};
    use super::environment::EnvRef;
    use super::lexer::Token;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Value {
        Number(f64),
        String(String),
        Bool(bool),
        Nil,

        Builtin {
            name: String,
            func: fn(Vec<Value>) -> Result<Value, String>,
        },
        Function {
            name: Option<String>,
            params: Vec<Token>,
            body: Vec<Stmt>,
            closure: EnvRef,
        },
        List(Rc<RefCell<Vec<Value>>>),
        Object(Rc<RefCell<Object>>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Object {
        pub name: Option<String>,
        pub props: Rc<RefCell<HashMap<String, Value>>>,
        pub proto: Option<Rc<RefCell<Object>>>,
    }

    impl Object {
        pub fn new(
            name: Option<String>,
            props: HashMap<String, Value>,
            proto: Option<Object>,
        ) -> Self {
            Object {
                name,
                props: Rc::new(RefCell::new(props)),
                proto: proto.map(|proto| Rc::new(RefCell::new(proto))),
            }
        }

        pub fn get(&self, key: &str) -> Option<Value> {
            if let Some(value) = self.props.borrow().get(key) {
                return Some(value.clone());
            }

            if let Some(proto) = self.proto.as_ref() {
                return proto.borrow().get(key);
            }

            None
        }

        pub fn set(&self, key: String, value: Value) {
            self.props.borrow_mut().insert(key, value);
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
                Value::Function { name, .. } => write!(
                    f,
                    "<fn {}>",
                    name.as_ref().unwrap_or(&"anonymous".to_string())
                ),
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
}

mod environment {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use super::interpreter::Runtime;
    use super::lexer::Token;
    use super::value::Value;

    pub type EnvRef = Rc<RefCell<Environment>>;

    #[derive(Debug, PartialEq)]
    pub struct Environment {
        enclosing: Option<EnvRef>,
        values: RefCell<HashMap<String, Value>>,
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
}

mod interpreter {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use crate::value::Object;
    use crate::BUILTINS;

    use super::ast::{Expr, Stmt};
    use super::environment::{EnvRef, Environment};
    use super::lexer::{Token, TokenKind};
    use super::value::Value;

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

    impl Interpreter {
        pub fn new() -> Interpreter {
            let globals = Environment::new(None);
            for (name, func) in BUILTINS.iter() {
                globals.borrow_mut().define(
                    name.to_string(),
                    Value::Builtin {
                        name: name.to_string(),
                        func: *func,
                    },
                );
            }
            globals
                .borrow_mut()
                .define("PI".to_string(), Value::Number(std::f64::consts::PI));

            Interpreter {
                env: Rc::clone(&globals),
                config: Config { repl: false },
            }
        }

        pub fn interpret(&mut self, stmts: &Vec<Stmt>) -> Result<(), String> {
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

        fn execute(&mut self, stmt: &Stmt) -> Result<Value, Runtime> {
            match stmt {
                Stmt::Expr(expr) => self.evaluate(expr),
                Stmt::Block(stmts) => {
                    let env = Environment::new(Some(Rc::clone(&self.env)));
                    self.exec_block(stmts, env)
                }
                Stmt::Break => Err(Runtime::Break(
                    "break statement used outside of loop".to_string(),
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
                                Value::Function {
                                    name: Some(name.value.clone()),
                                    params: params.clone(),
                                    body: body.clone(),
                                    closure: Rc::clone(&self.env),
                                },
                            );
                        }
                        _ => unreachable!(),
                    }
                    Ok(Value::Nil)
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
                Stmt::Return(_, value) => {
                    let value = match value {
                        Some(expr) => self.evaluate(expr)?,
                        None => Value::Nil,
                    };
                    Err(Runtime::Return(
                        match value {
                            Value::Nil => Value::Nil,
                            _ => value,
                        },
                        "return statement used outside of function".to_string(),
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

        fn evaluate(&mut self, expr: &Expr) -> Result<Value, Runtime> {
            match expr {
                Expr::Assign { name, value } => {
                    let value = self.evaluate(value)?;
                    self.env.borrow_mut().assign(name, value.clone())?;
                    Ok(value)
                }
                Expr::Call { callee, args } => {
                    let callee = self.evaluate(callee)?;
                    let args = args
                        .iter()
                        .map(|arg| self.evaluate(arg))
                        .collect::<Result<Vec<_>, _>>()?;

                    self.eval_call(callee, args)
                }
                Expr::Function { params, body } => Ok(Value::Function {
                    name: None,
                    params: params.clone(),
                    body: body.clone(),
                    closure: Rc::clone(&self.env),
                }),
                Expr::Get { object, name } => match self.evaluate(object)? {
                    Value::Object(obj) => match obj.borrow().get(&name.value) {
                        Some(value) => Ok(value),
                        None => Err(Runtime::Error(format!(
                            "undefined property '{}'",
                            name.value
                        ))),
                    },
                    _ => Err(Runtime::Error(
                        "invalid property access".to_string(),
                    )),
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
                                        "index out of bounds".to_string(),
                                    ))
                                }
                            }
                            _ => Err(Runtime::Error(
                                "invalid index, expected number".to_string(),
                            )),
                        },
                        Value::Object(obj) => match prop {
                            Value::String(key) => {
                                match obj.borrow().get(&key) {
                                    Some(value) => Ok(value),
                                    None => Err(Runtime::Error(format!(
                                        "undefined property '{}'",
                                        key
                                    ))),
                                }
                            }
                            _ => Err(Runtime::Error(
                                "property key must be a string".to_string(),
                            )),
                        },
                        _ => Err(Runtime::Error(
                            "invalid property access".to_string(),
                        )),
                    }
                }
                Expr::Literal(value) => Ok(value.into()),
                Expr::List(values) => Ok(Value::List(Rc::new(RefCell::new(
                    values
                        .iter()
                        .map(|expr| self.evaluate(expr))
                        .collect::<Result<Vec<_>, _>>()?,
                )))),
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
                    let object = Object::new(None, properties, None);
                    Ok(Value::Object(Rc::new(RefCell::new(object))))
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
                                .set(name.value.clone(), value.clone());
                            Ok(value)
                        }
                        _ => Err(Runtime::Error(
                            "invalid property access".to_string(),
                        )),
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
                                        "index out of bounds".to_string(),
                                    ))
                                }
                            }
                            _ => Err(Runtime::Error(
                                "invalid index, expected number".to_string(),
                            )),
                        },
                        Value::Object(obj) => match prop {
                            Value::String(key) => {
                                obj.borrow_mut().set(key, value.clone());
                                Ok(value)
                            }
                            _ => Err(Runtime::Error(
                                "property key must be a string".to_string(),
                            )),
                        },
                        _ => Err(Runtime::Error(
                            "invalid property access".to_string(),
                        )),
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
                Expr::Noop => unreachable!(),
            }
        }

        // -- STMT --

        fn exec_block(
            &mut self,
            stmts: &Vec<Stmt>,
            env: EnvRef,
        ) -> Result<Value, Runtime> {
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
        ) -> Result<Value, Runtime> {
            match iterable {
                Value::List(values) => {
                    for value in values.borrow().iter() {
                        self.env
                            .borrow_mut()
                            .define(variable.value.clone(), value.clone());
                        self.exec_block(body, Rc::clone(&self.env))?;
                    }
                    Ok(Value::Nil)
                }
                _ => Err(Runtime::Error("expected list".to_string())),
            }
        }

        // -- EXPR --

        fn eval_call(
            &mut self,
            callee: Value,
            args: Vec<Value>,
        ) -> Result<Value, Runtime> {
            match callee {
                Value::Builtin { func, .. } => {
                    func(args).map_err(Runtime::Error)
                }
                Value::Function {
                    params,
                    body,
                    closure,
                    ..
                } => {
                    if params.len() != args.len() {
                        return Err(Runtime::Error(format!(
                            "expected {} arguments, got {}",
                            params.len(),
                            args.len()
                        )));
                    }

                    let env = Environment::new(Some(Rc::clone(&closure)));
                    for (param, arg) in params.iter().zip(args) {
                        env.borrow_mut().define(param.value.clone(), arg);
                    }

                    match self.exec_block(&body, env) {
                        Ok(value) => Ok(value),
                        Err(Runtime::Return(value, _)) => Ok(value),
                        Err(e) => Err(e),
                    }
                }

                _ => Err(Runtime::Error("can only call functions".to_string())),
            }
        }

        fn eval_range(
            &self,
            start: Value,
            end: Value,
        ) -> Result<Value, Runtime> {
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

        fn eval_unary(&self, op: &Token, rhs: Value) -> Result<Value, Runtime> {
            use TokenKind::*;
            match op.kind {
                Bang => match rhs {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    _ => {
                        Err(Runtime::Error("invalid operand for !".to_string()))
                    }
                },
                Minus => match rhs {
                    Value::Number(n) => Ok(Value::Number(-n)),
                    _ => {
                        Err(Runtime::Error("invalid operand for -".to_string()))
                    }
                },
                _ => Err(Runtime::Error(format!("unknown operator: {:?}", op))),
            }
        }

        fn eval_binop(
            &self,
            op: &Token,
            lhs: Value,
            rhs: Value,
        ) -> Result<Value, Runtime> {
            use TokenKind::*;
            match op.kind {
                BangEqual => Ok(Value::Bool(!is_eq(&lhs, &rhs))),
                EqualEqual => Ok(Value::Bool(is_eq(&lhs, &rhs))),
                Greater => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x > y))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for >".to_string(),
                    )),
                },
                GreaterEqual => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x >= y))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for >=".to_string(),
                    )),
                },
                Less => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x < y))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for <".to_string(),
                    )),
                },
                LessEqual => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Bool(x <= y))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for <=".to_string(),
                    )),
                },
                Plus => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x + y))
                    }
                    (Value::String(x), Value::String(y)) => {
                        Ok(Value::String(format!("{}{}", x, y)))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for +".to_string(),
                    )),
                },
                Minus => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x - y))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for -".to_string(),
                    )),
                },
                Star => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x * y))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for *".to_string(),
                    )),
                },
                Slash => match (lhs, rhs) {
                    (Value::Number(x), Value::Number(y)) => {
                        Ok(Value::Number(x / y))
                    }
                    _ => Err(Runtime::Error(
                        "invalid operands for /".to_string(),
                    )),
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
}

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

use crate::value::Value;

pub struct Es {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
    config: Config,
}

struct Config {
    debug: bool,
}

impl Es {
    pub fn new() -> Self {
        Es {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(),
            config: Config { debug: false },
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
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap_or_else(|msg| {
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

        if self.config.debug {
            println!("--> Program {:#?}", program);
        }

        for error in errors {
            self.error(error);
        }

        if self.had_error {
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
            ".gc" => {
                std::mem::drop(self.interpreter.env.clone());
                println!("Garbage collected");
            }
            ".debug" => self.config.debug = !self.config.debug,
            _ => println!("Unknown command: {}", command),
        }
    }
}

lazy_static! {
    static ref BUILTINS: Vec<(&'static str, fn(Vec<Value>) -> Result<Value, String>)> = vec![
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
                panic!("Expected 1 argument, got {}", args.len());
            }
            // EneObject::String(format!("{:?}", args[0])) -> enum
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
                // Value::Object(obj) => {
                //     let obj = obj.borrow();
                //     let len = obj.props.borrow().len();
                //     Ok(Value::Number(len as f64))
                // }
                Value::List(list) => Ok(Value::Number(list.borrow().len() as f64)),
                _ => Err(format!("expected iterable, got {:?}", args[0])),
            }
        }),
    ];
}
