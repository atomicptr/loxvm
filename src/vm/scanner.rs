#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    NotEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Modulo,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    True,
    False,
    And,
    Class,
    Else,
    If,
    Fun,
    For,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,
    Eof,
    Break,
    Continue,
    QuestionMark,
    Colon,
    Switch,
    Case,
    Default,
    Error,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub start: usize,
    pub length: usize,
}

#[derive(Debug, Clone)]
pub struct Scanner {
    code: Vec<char>,
    current: usize,
    line: usize,
    start: usize,
}

#[derive(Debug)]
pub enum ScannerError {
    UnexpectedCharacter(Token),
    UnterminatedString(Token),
}

impl Scanner {
    pub fn new(code: String) -> Self {
        Self {
            code: code.chars().collect(),
            current: 0,
            line: 1,
            start: 0,
        }
    }

    pub fn scan(&mut self) -> Result<Token, ScannerError> {
        self.skip_whitespaces();

        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenType::Eof));
        }

        let c = self.advance();

        match c {
            '(' => Ok(self.make_token(TokenType::LParen)),
            ')' => Ok(self.make_token(TokenType::RParen)),
            '{' => Ok(self.make_token(TokenType::LBrace)),
            '}' => Ok(self.make_token(TokenType::RBrace)),
            ',' => Ok(self.make_token(TokenType::Comma)),
            '.' => Ok(self.make_token(TokenType::Dot)),
            '-' => Ok(self.make_token(TokenType::Minus)),
            '+' => Ok(self.make_token(TokenType::Plus)),
            ';' => Ok(self.make_token(TokenType::Semicolon)),
            '*' => Ok(self.make_token(TokenType::Star)),
            '/' => Ok(self.make_token(TokenType::Slash)),
            '?' => Ok(self.make_token(TokenType::QuestionMark)),
            ':' => Ok(self.make_token(TokenType::Colon)),
            '!' => {
                if self.matches('=') {
                    Ok(self.make_token(TokenType::NotEqual))
                } else {
                    Ok(self.make_token(TokenType::Bang))
                }
            }
            '=' => {
                if self.matches('=') {
                    Ok(self.make_token(TokenType::EqualEqual))
                } else {
                    Ok(self.make_token(TokenType::Equal))
                }
            }
            '<' => {
                if self.matches('=') {
                    Ok(self.make_token(TokenType::LessEqual))
                } else {
                    Ok(self.make_token(TokenType::Less))
                }
            }
            '>' => {
                if self.matches('=') {
                    Ok(self.make_token(TokenType::GreaterEqual))
                } else {
                    Ok(self.make_token(TokenType::Greater))
                }
            }
            '"' => self.make_string(),
            '0'..='9' => Ok(self.make_number()),
            c if c.is_alphanumeric() || c == '_' => Ok(self.make_identifier()),
            _ => Err(ScannerError::UnexpectedCharacter(
                self.make_token(TokenType::Error),
            )),
        }
    }

    pub fn token_data(&self, token: &Token) -> String {
        self.code[token.start..(token.start + token.length)]
            .iter()
            .collect()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.code.get(self.current - 1).unwrap().clone()
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.code.get(self.current).unwrap().clone() != expected {
            return false;
        }

        self.current += 1;

        true
    }

    fn skip_whitespaces(&mut self) {
        loop {
            match *self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if let Some(next) = self.peek_next() {
                        if *next == '/' {
                            while *self.peek() != '\n' && !self.is_at_end() {
                                self.advance();
                            }
                            // skip the newline too
                            self.advance();
                        }
                    }
                    return;
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> &char {
        self.code.get(self.current).unwrap_or(&'\0')
    }

    fn peek_next(&self) -> Option<&char> {
        self.code.get(self.current + 1)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.code.len()
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
        }
    }

    fn make_string(&mut self) -> Result<Token, ScannerError> {
        while *self.peek() != '"' && !self.is_at_end() {
            if *self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(ScannerError::UnterminatedString(
                self.make_token(TokenType::Error),
            ));
        }

        self.advance();

        Ok(self.make_token(TokenType::String))
    }

    fn make_number(&mut self) -> Token {
        while ('0'..='9').contains(self.peek()) {
            self.advance();
        }

        if *self.peek() == '.' {
            if let Some(next) = self.peek_next() {
                if ('0'..='9').contains(&next.clone()) {
                    self.advance();

                    while ('0'..='9').contains(self.peek()) {
                        self.advance();
                    }
                }
            }
        }

        self.make_token(TokenType::Number)
    }

    fn make_identifier(&mut self) -> Token {
        loop {
            let c = self.peek();

            if c.is_alphanumeric() || *c == '_' {
                self.advance();
                continue;
            }

            let token = self.make_token(TokenType::Identifier);

            return match self.token_data(&token).as_str() {
                "and" => self.make_token(TokenType::And),
                "break" => self.make_token(TokenType::Break),
                "case" => self.make_token(TokenType::Case),
                "class" => self.make_token(TokenType::Class),
                "continue" => self.make_token(TokenType::Continue),
                "default" => self.make_token(TokenType::Default),
                "else" => self.make_token(TokenType::Else),
                "false" => self.make_token(TokenType::False),
                "for" => self.make_token(TokenType::For),
                "fun" => self.make_token(TokenType::Fun),
                "if" => self.make_token(TokenType::If),
                "nil" => self.make_token(TokenType::Nil),
                "or" => self.make_token(TokenType::Or),
                "mod" => self.make_token(TokenType::Modulo),
                "print" => self.make_token(TokenType::Print),
                "return" => self.make_token(TokenType::Return),
                "super" => self.make_token(TokenType::Super),
                "switch" => self.make_token(TokenType::Switch),
                "this" => self.make_token(TokenType::This),
                "true" => self.make_token(TokenType::True),
                "var" => self.make_token(TokenType::Var),
                "while" => self.make_token(TokenType::While),
                _ => token,
            };
        }
    }
}
