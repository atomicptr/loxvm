use crate::vm::{
    chunk::Chunk,
    op::Op,
    scanner::{Scanner, ScannerError, Token, TokenType},
    value::Value,
};

#[derive(Debug)]
struct Compiler {
    // parser
    scanner: Scanner,
    chunk: Option<Chunk>,
    previous: Option<Token>,
    current: Option<Token>,
}

#[derive(Debug)]
pub enum CompileError {
    ScannerError(ScannerError),
    ExpectedChar(char, String, Token),
    ExpectedName(String, Token),
    InvalidAssignmentTarget(Token),
}

type ParseResult = Result<(), CompileError>;

impl From<ScannerError> for CompileError {
    fn from(value: ScannerError) -> Self {
        CompileError::ScannerError(value)
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
enum Precedence {
    #[default]
    NoPrecedence,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next(&self) -> Option<Precedence> {
        use Precedence::*;
        match self {
            NoPrecedence => Some(Assignment),
            Assignment => Some(Or),
            Or => Some(And),
            And => Some(Equality),
            Equality => Some(Comparison),
            Comparison => Some(Term),
            Term => Some(Factor),
            Factor => Some(Unary),
            Unary => Some(Call),
            Call => Some(Primary),
            Primary => None,
        }
    }
}

#[derive(Debug)]
enum ParseFn {
    Literal,
    Grouping,
    Unary,
    Binary,
    Number,
    String,
    Var,
}

#[derive(Debug, Default)]
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    pub fn new(prefix: Option<ParseFn>, infix: Option<ParseFn>, precedence: Precedence) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }

    pub fn prefix(prefix: ParseFn) -> Self {
        Self {
            prefix: Some(prefix),
            ..Default::default()
        }
    }

    pub fn infix_prec(infix: ParseFn, precedence: Precedence) -> Self {
        Self {
            infix: Some(infix),
            precedence,
            ..Default::default()
        }
    }
}

impl Compiler {
    fn new(code: String) -> Self {
        Self {
            scanner: Scanner::new(code),
            chunk: None,
            previous: None,
            current: None,
        }
    }

    fn compile(&mut self) -> Result<Chunk, CompileError> {
        self.chunk = Some(Chunk::default());

        self.advance()?;

        while !self.current_is(TokenType::Eof) {
            self.declaration()?;
        }

        self.emit_byte(Op::Return.into());

        Ok(self.chunk.take().unwrap())
    }

    fn emit_byte(&mut self, byte: u8) {
        if let Some(chunk) = &mut self.chunk {
            chunk.push(
                byte,
                self.previous
                    .as_ref()
                    .and_then(|p| Some(p.line))
                    .unwrap_or_default(),
            );
        }
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8) {
        self.emit_byte(b1);
        self.emit_byte(b2);
    }

    fn emit_constant(&mut self, constant: Value) {
        if let Some(chunk) = &mut self.chunk {
            chunk.push_constant(
                constant,
                self.previous
                    .as_ref()
                    .and_then(|p| Some(p.line))
                    .unwrap_or_default(),
            );
        }
    }

    pub fn advance(&mut self) -> Result<(), ScannerError> {
        self.previous = self.current;
        self.current = Some(self.scanner.scan()?);

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> ParseResult {
        self.advance()?;

        if let Some(prefix) = self.rule(self.previous.unwrap().token_type).prefix {
            let can_assign = precedence <= Precedence::Assignment;
            self.apply_parsefn(prefix, can_assign)?;

            while precedence <= self.rule(self.current.unwrap().token_type).precedence {
                self.advance()?;

                let infix = self.rule(self.previous.unwrap().token_type).infix.unwrap();

                self.apply_parsefn(infix, false)?;
            }

            if can_assign && self.consume_is(TokenType::Equal) {
                return Err(CompileError::InvalidAssignmentTarget(
                    self.previous.unwrap(),
                ));
            }

            return Ok(());
        }

        panic!("expected expression");
    }

    fn grouping(&mut self) -> ParseResult {
        self.expression()?;

        if !self.consume_is(TokenType::RParen) {
            return Err(CompileError::ExpectedChar(
                ')',
                "after expression".to_string(),
                self.previous.unwrap(),
            ));
        }

        Ok(())
    }

    fn unary(&mut self) -> ParseResult {
        let op_type = self.previous.unwrap().token_type;

        self.parse_precedence(Precedence::Unary)?;

        match op_type {
            TokenType::Minus => self.emit_byte(Op::Negate.into()),
            TokenType::Bang => self.emit_byte(Op::Not.into()),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn binary(&mut self) -> ParseResult {
        let op_type = self.previous.unwrap().token_type;

        let rule = self.rule(op_type);
        self.parse_precedence(rule.precedence.next().unwrap_or_default())?;

        match op_type {
            TokenType::Plus => self.emit_byte(Op::Add.into()),
            TokenType::Minus => self.emit_byte(Op::Subtract.into()),
            TokenType::Star => self.emit_byte(Op::Multiply.into()),
            TokenType::Slash => self.emit_byte(Op::Divide.into()),
            TokenType::NotEqual => self.emit_bytes(Op::Equal.into(), Op::Not.into()),
            TokenType::EqualEqual => self.emit_byte(Op::Equal.into()),
            TokenType::Greater => self.emit_byte(Op::Greater.into()),
            TokenType::GreaterEqual => self.emit_bytes(Op::Less.into(), Op::Not.into()),
            TokenType::Less => self.emit_byte(Op::Less.into()),
            TokenType::LessEqual => self.emit_bytes(Op::Greater.into(), Op::Not.into()),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn rule(&mut self, token_type: TokenType) -> ParseRule {
        match token_type {
            TokenType::LParen => ParseRule::prefix(ParseFn::Grouping),
            TokenType::Minus => ParseRule::new(
                Some(ParseFn::Unary),
                Some(ParseFn::Binary),
                Precedence::Term,
            ),
            TokenType::Plus => ParseRule::infix_prec(ParseFn::Binary, Precedence::Term),
            TokenType::Slash => ParseRule::infix_prec(ParseFn::Binary, Precedence::Factor),
            TokenType::Star => ParseRule::infix_prec(ParseFn::Binary, Precedence::Factor),
            TokenType::Number => ParseRule::prefix(ParseFn::Number),
            TokenType::True => ParseRule::prefix(ParseFn::Literal),
            TokenType::False => ParseRule::prefix(ParseFn::Literal),
            TokenType::Nil => ParseRule::prefix(ParseFn::Literal),
            TokenType::Bang => ParseRule::prefix(ParseFn::Unary),
            TokenType::NotEqual => ParseRule::infix_prec(ParseFn::Binary, Precedence::Equality),
            TokenType::EqualEqual => ParseRule::infix_prec(ParseFn::Binary, Precedence::Equality),
            TokenType::Greater => ParseRule::infix_prec(ParseFn::Binary, Precedence::Comparison),
            TokenType::GreaterEqual => {
                ParseRule::infix_prec(ParseFn::Binary, Precedence::Comparison)
            }
            TokenType::Less => ParseRule::infix_prec(ParseFn::Binary, Precedence::Comparison),
            TokenType::LessEqual => ParseRule::infix_prec(ParseFn::Binary, Precedence::Comparison),
            TokenType::String => ParseRule::prefix(ParseFn::String),
            TokenType::Identifier => ParseRule::prefix(ParseFn::Var),
            _ => ParseRule::default(),
        }
    }

    fn apply_parsefn(&mut self, parsefn: ParseFn, can_assign: bool) -> ParseResult {
        match parsefn {
            ParseFn::Literal => self.literal(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::String => self.string(),
            ParseFn::Var => self.variable(can_assign),
        }
    }

    fn declaration(&mut self) -> ParseResult {
        if self.consume_is(TokenType::Var) {
            self.var_decl()
        } else {
            self.statement()
        }
    }

    fn var_decl(&mut self) -> ParseResult {
        let global = self.parse_variable()?;

        if self.consume_is(TokenType::Equal) {
            self.expression()?;
        } else {
            self.emit_byte(Op::Nil.into());
        }

        if !self.consume_is(TokenType::Semicolon) {
            return Err(CompileError::ExpectedChar(
                ';',
                "variable declaration".to_string(),
                self.previous.unwrap(),
            ));
        }

        self.define_var(global);

        Ok(())
    }

    fn define_var(&mut self, global: u8) {
        self.emit_bytes(Op::DefineGlobal.into(), global);
    }

    fn parse_variable(&mut self) -> Result<u8, CompileError> {
        if !self.consume_is(TokenType::Identifier) {
            return Err(CompileError::ExpectedName(
                "variable".to_string(),
                self.previous.unwrap(),
            ));
        }

        let token = self.previous.unwrap();
        Ok(self.identifier_constant(&token))
    }

    fn identifier_constant(&mut self, token: &Token) -> u8 {
        let token_data = self.scanner.token_data(token);

        self.chunk
            .as_mut()
            .unwrap()
            .make_constant(token_data.into())
    }

    fn statement(&mut self) -> ParseResult {
        if self.consume_is(TokenType::Print) {
            return self.print_stmt();
        }

        self.expression_stmt()
    }

    fn expression_stmt(&mut self) -> ParseResult {
        self.expression()?;

        if !self.consume_is(TokenType::Semicolon) {
            return Err(CompileError::ExpectedChar(
                ';',
                "value".to_string(),
                self.current.unwrap(),
            ));
        }

        self.emit_byte(Op::Pop.into());

        Ok(())
    }

    fn print_stmt(&mut self) -> ParseResult {
        self.expression()?;

        if !self.consume_is(TokenType::Semicolon) {
            return Err(CompileError::ExpectedChar(
                ';',
                "value".to_string(),
                self.current.unwrap(),
            ));
        }

        self.emit_byte(Op::Print.into());

        Ok(())
    }

    fn expression(&mut self) -> ParseResult {
        self.parse_precedence(Precedence::Assignment)?;

        Ok(())
    }

    fn number(&mut self) -> ParseResult {
        let value = self
            .scanner
            .token_data(&self.previous.unwrap())
            .parse::<f64>()
            .expect("must convert to number");

        self.emit_constant(Value::Number(value));

        Ok(())
    }

    fn string(&mut self) -> ParseResult {
        let prev = self.previous.unwrap();

        let str = self.scanner.token_data(&prev);
        self.emit_constant(str.trim_matches('"').to_string().into());

        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> ParseResult {
        self.named_variable(&self.previous.unwrap(), can_assign)?;
        Ok(())
    }

    fn named_variable(&mut self, token: &Token, can_assign: bool) -> ParseResult {
        let arg = self.identifier_constant(token);

        if can_assign && self.consume_is(TokenType::Equal) {
            self.expression()?;
            self.emit_bytes(Op::SetGlobal.into(), arg);
            return Ok(());
        }

        self.emit_bytes(Op::GetGlobal.into(), arg);

        Ok(())
    }

    fn literal(&mut self) -> ParseResult {
        match self.previous.unwrap().token_type {
            TokenType::True => self.emit_byte(Op::True.into()),
            TokenType::False => self.emit_byte(Op::False.into()),
            TokenType::Nil => self.emit_byte(Op::Nil.into()),
            _ => unreachable!(),
        };

        Ok(())
    }

    fn sync(&mut self) {
        while self.current.unwrap().token_type != TokenType::Eof {
            if self.previous.unwrap().token_type == TokenType::Semicolon {
                return;
            }

            match self.current.unwrap().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance().unwrap();
        }
    }

    pub fn current_is(&self, token_type: TokenType) -> bool {
        if let Some(current) = &self.current {
            current.token_type == token_type
        } else {
            false
        }
    }

    fn consume(&mut self, token_type: TokenType) -> Option<Token> {
        if self.current_is(token_type) {
            self.advance().unwrap();
            if let Some(prev) = &self.previous {
                Some(prev.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn consume_is(&mut self, token_type: TokenType) -> bool {
        if let Some(_) = self.consume(token_type) {
            true
        } else {
            false
        }
    }
}

pub fn compile(code: String) -> Result<Chunk, CompileError> {
    let mut compiler = Compiler::new(code);
    compiler.compile()
}
