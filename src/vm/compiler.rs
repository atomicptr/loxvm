use crate::vm::{
    chunk::Chunk,
    op,
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
    ExpectedEndOfExpression(Token),
    ExpectedChar(char, String, Token),
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
    Grouping,
    Unary,
    Binary,
    Number,
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

        self.expression()?;

        if !self.consume_is(TokenType::Eof) {
            return Err(CompileError::ExpectedEndOfExpression(self.current.unwrap()));
        }

        self.emit_byte(op::RETURN);

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
            self.apply_parsefn(prefix)?;

            while precedence <= self.rule(self.current.unwrap().token_type).precedence {
                self.advance()?;

                let infix = self.rule(self.previous.unwrap().token_type).infix.unwrap();

                self.apply_parsefn(infix)?;
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
            TokenType::Minus => self.emit_byte(op::NEGATE),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn binary(&mut self) -> ParseResult {
        let op_type = self.previous.unwrap().token_type;

        let rule = self.rule(op_type);
        self.parse_precedence(rule.precedence.next().unwrap_or_default())?;

        match op_type {
            TokenType::Plus => self.emit_byte(op::ADD),
            TokenType::Minus => self.emit_byte(op::SUBTRACT),
            TokenType::Star => self.emit_byte(op::MULTIPLY),
            TokenType::Slash => self.emit_byte(op::DIVIDE),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn rule(&mut self, token_type: TokenType) -> ParseRule {
        match token_type {
            TokenType::LParen => {
                ParseRule::new(Some(ParseFn::Grouping), None, Precedence::NoPrecedence)
            }
            TokenType::Minus => ParseRule::new(
                Some(ParseFn::Unary),
                Some(ParseFn::Binary),
                Precedence::Term,
            ),
            TokenType::Plus => ParseRule::new(None, Some(ParseFn::Binary), Precedence::Term),
            TokenType::Slash => ParseRule::new(None, Some(ParseFn::Binary), Precedence::Factor),
            TokenType::Star => ParseRule::new(None, Some(ParseFn::Binary), Precedence::Factor),
            TokenType::Number => {
                ParseRule::new(Some(ParseFn::Number), None, Precedence::NoPrecedence)
            }
            _ => ParseRule::default(),
        }
    }

    fn apply_parsefn(&mut self, parsefn: ParseFn) -> ParseResult {
        match parsefn {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
        }
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

        self.emit_constant(value);

        Ok(())
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
