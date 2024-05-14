use std::rc::Rc;

use crate::lexer::Token;

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
    If {
        branches: Vec<(Expr, Vec<Stmt>)>,
        else_branch: Option<Vec<Stmt>>,
    },
    Let {
        variables: Vec<String>,
        initializers: Vec<Option<Expr>>,
    },
    Loop(Vec<Stmt>),
    While {
        condition: Rc<Expr>,
        body: Vec<Stmt>,
    },
    Break(Token),
    Return(Token, Option<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Assign {
        name: Token,
        op: Token,
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
    Logical {
        op: Token,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
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
