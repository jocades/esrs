use std::rc::Rc;

use crate::ast::{Expr, Literal, Stmt};
use crate::lexer::{Token, TokenKind};

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
        match self.at().kind {
            Let => {
                self.eat();
                self.let_declaration()
            }
            Fn if self.check_next(&Identifier) => {
                self.eat();
                self.function_declaration("function")
            }
            _ => self.statement(),
        }
    }

    fn let_declaration(&mut self) -> ParseResult<Stmt> {
        let mut variables = Vec::new();
        loop {
            variables.push(
                self.expect(&Identifier, "expected identifier after 'let'")?
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
                return Err(self.error(format!(
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
        let name =
            self.expect(&Identifier, format!("expected {} after name", kind))?;

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
            _ => Err(format!("expected '{{' or '->'  before {kind} body")),
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
                self.eat();
                self.if_statement()
            }
            Loop => {
                self.eat();
                self.loop_statement()
            }
            While => {
                self.eat();
                self.while_statement()
            }
            Break => {
                let token = self.eat().clone();
                Ok(Stmt::Break(token))
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
        let variable = self
            .expect(&Identifier, "expected identifier after 'for'")?
            .clone();
        self.expect(&In, "expected 'in' after identifier")?;

        let mut iterable = self.expression()?;

        if self.check(&DotDot) {
            iterable = self.range_end(iterable)?;
        }

        self.expect(&LBrace, "expected '{' after for loop expression")?;

        Ok(Stmt::For {
            variable,
            iterable: Rc::new(iterable),
            body: self.block()?,
        })
    }

    fn range_end(&mut self, start: Expr) -> ParseResult<Expr> {
        self.expect(&DotDot, "expected '..' after range start")?;
        let end = self.expression()?;
        Ok(Expr::Range(Rc::new(start), Rc::new(end)))
    }

    fn if_statement(&mut self) -> ParseResult<Stmt> {
        let mut branches = vec![self.branch("if")?];

        while self.matches(&[Elif]) {
            branches.push(self.branch("elif")?);
        }

        let else_branch = if self.matches(&[Else]) {
            self.expect(&LBrace, "expected '{' after 'else'")?;
            Some(self.block()?)
        } else {
            None
        };

        Ok(Stmt::If {
            branches,
            else_branch,
        })
    }

    fn branch(&mut self, kind: &str) -> Result<(Expr, Vec<Stmt>), String> {
        let condition = self.expression()?;
        self.expect(&LBrace, format!("expected '{{' after {kind} condition"))?;
        let body = self.block()?;
        Ok((condition, body))
    }

    fn loop_statement(&mut self) -> ParseResult<Stmt> {
        self.expect(&LBrace, "expected '{' after 'loop'")?;
        Ok(Stmt::Loop(self.block()?))
    }

    fn while_statement(&mut self) -> ParseResult<Stmt> {
        let (condition, body) = self.branch("while")?;
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
        let expr = self.or()?;

        if self.matches(&[Equal, PlusEqual, MinusEqual, StarEqual, SlashEqual])
        {
            let op = self.prev().clone();
            let value = self.assignment()?;

            match expr {
                Expr::Variable(name) => {
                    return Ok(Expr::Assign {
                        name,
                        op,
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

    fn or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.and()?;

        while self.matches(&[Or]) {
            let op = self.prev().clone();
            let rhs = self.and()?;
            expr = Expr::Logical {
                op,
                lhs: Rc::new(expr),
                rhs: Rc::new(rhs),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.equality()?;

        while self.matches(&[And]) {
            let op = self.prev().clone();
            let rhs = self.equality()?;
            expr = Expr::Logical {
                op,
                lhs: Rc::new(expr),
                rhs: Rc::new(rhs),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;

        while self.matches(&[BangEqual, EqualEqual]) {
            let op = self.prev().clone();
            let rhs = self.comparison()?;
            expr = Expr::BinaryOp {
                op,
                lhs: Rc::new(expr),
                rhs: Rc::new(rhs),
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

    fn object(&mut self) -> ParseResult<Expr> {
        let mut props = vec![];

        while self.at().kind != RBrace {
            let key =
                self.expect(&Identifier, "expected identifier key")?.clone();

            if self.matches(&[Comma]) {
                props.push((key, None));
                continue;
            } else if self.at().kind == RBrace {
                // allows shorthand { key }
                props.push((key, None));
                break;
            } else {
                self.expect(&Colon, "expected ':' after object key")?;
                let value = self.expression()?;
                props.push((key, Some(value)));
            }

            if self.at().kind != RBrace {
                self.expect(&Comma, "expected ',' after object value")?;
            }
        }

        self.expect(&RBrace, "expected '}' after object literal")?;

        Ok(Expr::Object { props })
    }

    fn list(&mut self) -> ParseResult<Expr> {
        let mut values = Vec::new();

        while self.at().kind != RBracket {
            values.push(self.expression()?);
            if !self.matches(&[Comma]) {
                break;
            }
        }

        self.expect(&RBracket, "Expected ']' after list")?;

        Ok(Expr::List(values))
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
            LBracket => {
                self.eat();
                self.list()
            }
            LBrace => {
                self.eat();
                self.object()
            }
            LParen => {
                self.eat();
                let expr = self.expression()?;
                if self.check(&DotDot) {
                    return self.range_end(expr);
                }
                self.expect(&RParen, "expected ')' after expression")?;
                Ok(expr)
            }

            _ => Err(self.error("expected expression")),
            // _ => Ok(Expr::Noop),
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

    fn expect(
        &mut self,
        expected: &TokenKind,
        message: impl Into<String>,
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

    fn error(&self, message: impl Into<String>) -> String {
        format!("Parse error: {} at {:?}.", message.into(), self.at())
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
                Let | Fn | If | For | While | Return => return,
                _ => {}
            }

            self.eat();
        }
    }
}
