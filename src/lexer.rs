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
        PlusEqual, MinusEqual, 
        StarEqual, SlashEqual,

        // Literals
        Identifier, Number, Str,

        // Keywords
        And, Break, Class, Continue, Elif, Else, False,
        Fn, For, If, /*Import*/ In, Let, Loop, Nil, Or, Return,
        Super, /*This*/ True, While, 

        EOF,
    }

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
        use TokenKind::*;

        let mut m = HashMap::new();
        m.insert("and", And);
        m.insert("break", Break);
        m.insert("class", Class);
        m.insert("continue", Continue);
        m.insert("else", Else);
        m.insert("elif", Elif);
        m.insert("false", False);
        m.insert("fn", Fn);
        m.insert("for", For);
        m.insert("if", If);
        // m.insert("import", Import);
        m.insert("in", In);
        m.insert("let", Let);
        m.insert("loop", Loop);
        m.insert("nil", Nil);
        m.insert("or", Or);
        m.insert("return", Return);
        m.insert("super", Super);
        // m.insert("this", This);
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

        Ok(self.tokens.clone())
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
            '+' => {
                if self.matches('=') {
                    self.add_token(PlusEqual)
                } else {
                    self.add_token(Plus)
                }
            }
            '*' => {
                if self.matches('=') {
                    self.add_token(StarEqual)
                } else {
                    self.add_token(Star)
                }
            }
            '/' => {
                if self.matches('/') {
                    while self.at() != '\n' && !self.eof() {
                        self.eat();
                    }
                } else if self.matches('=') {
                    self.add_token(SlashEqual)
                } else {
                    self.add_token(Slash);
                }
            }
            '-' => {
                if self.matches('>') {
                    self.add_token(Arrow)
                } else if self.matches('=') {
                    self.add_token(MinusEqual)
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
                return Err(format!("unexpected character '{c}'"));
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
            return Err("unterminated string".into());
        }

        self.eat();

        let value = &self.source[self.start + 1..self.index - 1];
        self.add_token_value(TokenKind::Str, value.into());
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
        if self.eof() || self.at() != expected {
            return false;
        }
        self.eat();
        true
    }

    fn add_token(&mut self, kind: TokenKind) {
        let value = &self.source[self.start..self.index];
        self.tokens.push(Token {
            kind,
            value: value.into(),
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
