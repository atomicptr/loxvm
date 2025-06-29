use std::rc::Rc;

use crate::{
    constants::DEBUG_MODE,
    vm::{
        chunk::Chunk,
        op::Op,
        scanner::{Scanner, ScannerError, Token, TokenType},
        value::{Function, Value},
    },
};

#[derive(Debug)]
struct Compiler {
    // parser
    scanner: Scanner,
    previous: Option<Token>,
    current: Option<Token>,

    // compiler
    ctx: Context,
}

#[derive(Debug)]
struct Context {
    parent: Option<Box<Context>>,
    fun: Option<Function>,
    fun_type: FunctionType,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: usize,
    loop_scopes: Vec<LoopScope>,
}

#[derive(Debug)]
struct Local {
    token: Token,
    depth: i32,
    is_captured: bool,
}

#[derive(Debug)]
struct LoopScope {
    start: usize,
    exit_jumps: Vec<usize>,
}

#[derive(Debug, Clone)]
struct Upvalue {
    index: u8,
    is_local: bool,
}

#[derive(Debug, PartialEq, Eq)]
enum FunctionType {
    Script,
    Function,
}

#[derive(Debug)]
pub enum CompileError {
    ScannerError(ScannerError),
    ExpectedChar(char, String, Token),
    ExpectedName(String, Token),
    InvalidAssignmentTarget(Token),
    TooManyLocalVars(Token),
    VarAlreadyExists(Token),
    ExpectedExpression(Token),
    CantUseLocalVarInItsOwnInitializer(Token),
    IllegalStatement(String, Token),
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
    And,
    Or,
    Call,
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
            // parse
            scanner: Scanner::new(code),
            previous: None,
            current: None,

            // compiler
            ctx: Context {
                parent: None,

                locals: vec![Local {
                    token: Token {
                        token_type: TokenType::Default,
                        line: 0,
                        start: 0,
                        length: 0,
                    },
                    depth: 0,
                    is_captured: false,
                }],
                upvalues: Vec::new(),
                scope_depth: 0,
                loop_scopes: Vec::new(),

                fun_type: FunctionType::Script,
                fun: Some(Function::new("<script>".to_string(), 0)),
            },
        }
    }

    fn compile(&mut self) -> Result<Function, CompileError> {
        self.advance()?;

        while !self.current_is(TokenType::Eof) {
            self.declaration()?;
        }

        self.emit_return();

        let fun = self.ctx.fun.take().unwrap();

        if DEBUG_MODE {
            fun.chunk
                .debug(fun.name.as_ref().unwrap_or(&"???".to_string()).as_str());
        }

        Ok(fun)
    }

    fn emit_byte(&mut self, byte: u8) {
        let prev = self.previous.clone();
        self.chunk_mut()
            .push(byte, prev.and_then(|p| Some(p.line)).unwrap_or_default());
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8) {
        self.emit_byte(b1);
        self.emit_byte(b2);
    }

    fn emit_constant(&mut self, constant: Value) {
        let line = self
            .previous
            .clone()
            .and_then(|p| Some(p.line))
            .unwrap_or_default();
        self.chunk_mut().push_constant(constant, line);
    }

    fn emit_jump(&mut self, op: Op) -> usize {
        self.emit_byte(op.into());
        self.emit_bytes(0xFF, 0xFF);
        self.current_pos() - 2
    }

    fn patch_jump(&mut self, jump_op_pos: usize) {
        let jump = self.current_pos() - jump_op_pos - 2;
        self.patch_jump_with_pos(jump_op_pos, jump);
    }

    fn patch_jump_with_pos(&mut self, jump_op_pos: usize, target: usize) {
        if target > u16::MAX.into() {
            panic!("cant jump this far");
        }

        self.chunk_mut()
            .patch(jump_op_pos, ((target >> 8) & 0xFF) as u8);
        self.chunk_mut()
            .patch(jump_op_pos + 1, (target & 0xFF) as u8);
    }

    fn pop_loop_scope(&mut self) {
        let loop_scope = self.ctx.loop_scopes.pop().unwrap();

        for exit in loop_scope.exit_jumps {
            self.patch_jump(exit);
        }
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(Op::Loop.into());

        let offset = self.current_pos() - loop_start + 2;

        if offset > u16::MAX.into() {
            panic!("loop body too large");
        }

        self.emit_byte(((offset >> 8) & 0xFF) as u8);
        self.emit_byte((offset & 0xFF) as u8);
    }

    fn emit_return(&mut self) {
        self.emit_byte(Op::Nil.into());
        self.emit_byte(Op::Return.into());
    }

    fn current_pos(&self) -> usize {
        self.chunk().len()
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

                self.apply_parsefn(infix, can_assign)?;
            }

            if can_assign && self.consume_is(TokenType::Equal) {
                return Err(CompileError::InvalidAssignmentTarget(
                    self.previous.unwrap(),
                ));
            }

            return Ok(());
        }

        Err(CompileError::ExpectedExpression(self.previous.unwrap()))
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
            TokenType::Modulo => self.emit_byte(Op::Modulo.into()),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn rule(&mut self, token_type: TokenType) -> ParseRule {
        match token_type {
            TokenType::LParen => ParseRule::new(
                Some(ParseFn::Grouping),
                Some(ParseFn::Call),
                Precedence::Call,
            ),
            TokenType::Minus => ParseRule::new(
                Some(ParseFn::Unary),
                Some(ParseFn::Binary),
                Precedence::Term,
            ),
            TokenType::Plus => ParseRule::infix_prec(ParseFn::Binary, Precedence::Term),
            TokenType::Slash => ParseRule::infix_prec(ParseFn::Binary, Precedence::Factor),
            TokenType::Star => ParseRule::infix_prec(ParseFn::Binary, Precedence::Factor),
            TokenType::Modulo => ParseRule::infix_prec(ParseFn::Binary, Precedence::Factor),
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
            TokenType::And => ParseRule::infix_prec(ParseFn::And, Precedence::And),
            TokenType::Or => ParseRule::infix_prec(ParseFn::Or, Precedence::Or),
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
            ParseFn::And => self.and(),
            ParseFn::Or => self.or(),
            ParseFn::Call => self.call(),
        }
    }

    fn declaration(&mut self) -> ParseResult {
        if self.consume_is(TokenType::Fun) {
            self.fun_decl()
        } else if self.consume_is(TokenType::Var) {
            self.var_decl()
        } else {
            self.statement()
        }
    }

    fn fun_decl(&mut self) -> ParseResult {
        let global = self.parse_variable("function")?;
        self.mark_initialized();
        self.function(FunctionType::Function)?;
        self.define_var(global);

        Ok(())
    }

    fn function(&mut self, fun_type: FunctionType) -> ParseResult {
        let name = self.scanner.token_data(self.previous.as_ref().unwrap());

        let ctx = Context {
            parent: None,
            fun_type,
            fun: Some(Function::new(name, 0)),
            locals: Vec::new(),
            upvalues: Vec::new(),
            loop_scopes: Vec::new(),
            scope_depth: self.ctx.scope_depth + 1,
        };

        self.begin_ctx(ctx);

        self.ctx.locals.push(Local {
            token: Token {
                token_type: TokenType::Default,
                line: 0,
                start: 0,
                length: 0,
            },
            depth: 0,
            is_captured: false,
        });

        if !self.consume_is(TokenType::LParen) {
            return Err(CompileError::ExpectedChar(
                '(',
                "function name".to_string(),
                self.previous.unwrap(),
            ));
        }

        if !self.current_is(TokenType::RParen) {
            let mut arity = 1;
            let constant = self.parse_variable("parameter")?;
            self.define_var(constant);

            while self.consume_is(TokenType::Comma) {
                arity += 1;
                let constant = self.parse_variable("parameter")?;
                self.define_var(constant);
            }

            let fun = self.ctx.fun.as_mut().unwrap();
            fun.arity = arity;
        }

        if !self.consume_is(TokenType::RParen) {
            return Err(CompileError::ExpectedChar(
                ')',
                "parameters".to_string(),
                self.previous.unwrap(),
            ));
        }

        if !self.consume_is(TokenType::LBrace) {
            return Err(CompileError::ExpectedChar(
                '{',
                "function body".to_string(),
                self.previous.unwrap(),
            ));
        }

        self.block()?;

        let (fun, upvalues) = self.end_ctx();

        self.emit_byte(Op::Closure.into());
        self.emit_constant(Value::Function(Rc::new(fun)));

        for upvalue in upvalues {
            let is_local = if upvalue.is_local { 1 } else { 0 };

            self.emit_byte(is_local);
            self.emit_byte(upvalue.index);
        }

        Ok(())
    }

    fn var_decl(&mut self) -> ParseResult {
        let global = self.parse_variable("variable")?;

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

    fn declare_var(&mut self) -> ParseResult {
        if self.ctx.scope_depth == 0 {
            return Ok(());
        }

        let token = self.previous.unwrap().clone();

        for i in (0..self.ctx.locals.len()).rev() {
            let local = self.ctx.locals.get(i).unwrap();

            if local.depth != -1 && local.depth < self.ctx.scope_depth as i32 {
                break;
            }

            if identifier_equals(&self.scanner, &token, &local.token) {
                return Err(CompileError::VarAlreadyExists(token));
            }
        }

        self.add_local(token)
    }

    fn define_var(&mut self, global: u8) {
        if self.ctx.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(Op::DefineGlobal.into(), global);
    }

    fn mark_initialized(&mut self) {
        if self.ctx.scope_depth == 0 {
            return;
        }

        self.ctx.locals.last_mut().unwrap().depth = self.ctx.scope_depth as i32;
    }

    fn add_local(&mut self, token: Token) -> ParseResult {
        if self.ctx.locals.len() >= u8::MAX as usize {
            return Err(CompileError::TooManyLocalVars(token));
        }

        self.ctx.locals.push(Local {
            token,
            depth: -1,
            is_captured: false,
        });

        Ok(())
    }

    fn parse_variable(&mut self, name: &str) -> Result<u8, CompileError> {
        if !self.consume_is(TokenType::Identifier) {
            return Err(CompileError::ExpectedName(
                name.to_string(),
                self.previous.unwrap(),
            ));
        }

        self.declare_var()?;

        if self.ctx.scope_depth > 0 {
            return Ok(0);
        }

        let token = self.previous.unwrap();
        Ok(self.identifier_constant(&token))
    }

    fn identifier_constant(&mut self, token: &Token) -> u8 {
        let token_data = self.scanner.token_data(token);

        self.chunk_mut().make_constant(token_data.into())
    }

    fn statement(&mut self) -> ParseResult {
        if self.consume_is(TokenType::Print) {
            self.print_stmt()
        } else if self.consume_is(TokenType::LBrace) {
            self.begin_scope();
            self.block()?;
            self.end_scope();

            Ok(())
        } else if self.consume_is(TokenType::If) {
            self.if_stmt()
        } else if self.consume_is(TokenType::Switch) {
            self.switch_stmt()
        } else if self.consume_is(TokenType::While) {
            self.while_stmt()
        } else if self.consume_is(TokenType::For) {
            self.for_stmt()
        } else if self.consume_is(TokenType::Continue) {
            self.continue_stmt()
        } else if self.consume_is(TokenType::Break) {
            self.break_stmt()
        } else if self.consume_is(TokenType::Return) {
            self.return_stmt()
        } else {
            self.expression_stmt()
        }
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

    fn block(&mut self) -> ParseResult {
        while !self.current_is(TokenType::RBrace) && !self.current_is(TokenType::Eof) {
            self.declaration()?;
        }

        if !self.consume_is(TokenType::RBrace) {
            return Err(CompileError::ExpectedChar(
                ';',
                "block".to_string(),
                self.previous.unwrap(),
            ));
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.ctx.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.ctx.scope_depth -= 1;

        while self.ctx.locals.len() > 0 && self.last_local().depth > self.ctx.scope_depth as i32 {
            if self.ctx.locals.last().unwrap().is_captured {
                self.emit_byte(Op::CloseUpvalue.into());
            } else {
                self.emit_byte(Op::Pop.into());
            }

            self.ctx.locals.pop();
        }
    }

    fn begin_ctx(&mut self, ctx: Context) {
        let ctx = std::mem::replace(&mut self.ctx, ctx);
        self.ctx.parent = Some(Box::new(ctx));
    }

    fn end_ctx(&mut self) -> (Function, Vec<Upvalue>) {
        self.emit_return();

        if DEBUG_MODE {
            self.chunk()
                .debug(self.ctx.fun.as_ref().unwrap().name.as_ref().unwrap());
        }

        let parent = self.ctx.parent.take().unwrap();
        let mut ctx = std::mem::replace(&mut self.ctx, *parent);
        (ctx.fun.take().unwrap(), ctx.upvalues)
    }

    fn if_stmt(&mut self) -> ParseResult {
        if !self.consume_is(TokenType::LParen) {
            return Err(CompileError::ExpectedChar(
                '(',
                "if".to_string(),
                self.previous.unwrap(),
            ));
        }

        self.expression()?;

        if !self.consume_is(TokenType::RParen) {
            return Err(CompileError::ExpectedChar(
                ')',
                "condition".to_string(),
                self.previous.unwrap(),
            ));
        }

        let then_jump = self.emit_jump(Op::JumpIfFalse);
        self.emit_byte(Op::Pop.into());
        self.statement()?;

        let else_jump = self.emit_jump(Op::Jump);

        self.patch_jump(then_jump);
        self.emit_byte(Op::Pop.into());

        if self.consume_is(TokenType::Else) {
            self.statement()?;
        }

        self.patch_jump(else_jump);

        Ok(())
    }

    fn switch_stmt(&mut self) -> ParseResult {
        if !self.consume_is(TokenType::LParen) {
            return Err(CompileError::ExpectedChar(
                '(',
                "switch".to_string(),
                self.previous.unwrap(),
            ));
        }

        self.expression()?;

        if !self.consume_is(TokenType::RParen) {
            return Err(CompileError::ExpectedChar(
                ')',
                "condition".to_string(),
                self.previous.unwrap(),
            ));
        }

        if !self.consume_is(TokenType::LBrace) {
            return Err(CompileError::ExpectedChar(
                '{',
                "switch condition".to_string(),
                self.previous.unwrap(),
            ));
        }

        let mut end_jumps = vec![];

        while self.consume_is(TokenType::Case) {
            // duplicate the condition result
            self.emit_byte(Op::Dup.into());

            // evaluate the expression
            self.expression()?;

            // check if the condition and expression are equal
            self.emit_byte(Op::Equal.into());

            // add a "jump if false" to skip if its false
            let case_jump = self.emit_jump(Op::JumpIfFalse);
            self.emit_byte(Op::Pop.into()); // pop the comparison result

            if !self.consume_is(TokenType::Colon) {
                return Err(CompileError::ExpectedChar(
                    ':',
                    "case expression".to_string(),
                    self.current.unwrap(),
                ));
            }

            // process the statement
            self.statement()?;

            // we managed to successfully execute the statement, pop the initial result
            self.emit_byte(Op::Pop.into());

            // jump to the end after statement
            let end_jump = self.emit_jump(Op::Jump);

            // collect jump point, we later need to patch them all
            end_jumps.push(end_jump);

            // patch the jump target
            self.patch_jump(case_jump);
            self.emit_byte(Op::Pop.into()); // pop the comparison result
        }

        if self.consume_is(TokenType::Default) {
            if !self.consume_is(TokenType::Colon) {
                return Err(CompileError::ExpectedChar(
                    ':',
                    "default case".to_string(),
                    self.current.unwrap(),
                ));
            }

            self.statement()?;
        }

        if !self.consume_is(TokenType::RBrace) {
            return Err(CompileError::ExpectedChar(
                '}',
                "switch body".to_string(),
                self.previous.unwrap(),
            ));
        }

        // patch all the emitted case end jumps
        for end_jump in end_jumps {
            self.patch_jump(end_jump);
        }

        Ok(())
    }

    fn while_stmt(&mut self) -> ParseResult {
        let loop_start = self.current_pos();
        self.ctx.loop_scopes.push(LoopScope {
            start: loop_start,
            exit_jumps: Vec::new(),
        });

        if !self.consume_is(TokenType::LParen) {
            return Err(CompileError::ExpectedChar(
                '(',
                "while".to_string(),
                self.previous.unwrap(),
            ));
        }

        self.expression()?;

        if !self.consume_is(TokenType::RParen) {
            return Err(CompileError::ExpectedChar(
                ')',
                "condition".to_string(),
                self.previous.unwrap(),
            ));
        }

        let exit_jump = self.emit_jump(Op::JumpIfFalse);
        self.emit_byte(Op::Pop.into());
        self.statement()?;

        self.emit_loop(loop_start);
        self.emit_byte(Op::Pop.into());

        self.patch_jump(exit_jump);
        self.pop_loop_scope();

        Ok(())
    }

    fn for_stmt(&mut self) -> ParseResult {
        self.begin_scope();

        if !self.consume_is(TokenType::LParen) {
            return Err(CompileError::ExpectedChar(
                '(',
                "for".to_string(),
                self.previous.unwrap(),
            ));
        }

        // initializer
        if self.consume_is(TokenType::Semicolon) {
            // no initializer
        } else if self.consume_is(TokenType::Var) {
            self.var_decl()?;
        } else {
            self.expression_stmt()?;
        }

        let pre_condition = self.current_pos();

        // condition
        let exit_jump = if !self.consume_is(TokenType::Semicolon) {
            // evaluate the condition
            self.expression()?;

            // need a ';' after condition
            if !self.consume_is(TokenType::Semicolon) {
                return Err(CompileError::ExpectedChar(
                    ';',
                    "loop condition".to_string(),
                    self.previous.unwrap(),
                ));
            }

            // prepare jump to exit if it was false
            let exit_jump = self.emit_jump(Op::JumpIfFalse);
            self.emit_byte(Op::Pop.into()); // pop the condition result

            Some(exit_jump)
        } else {
            None // no condition, this is an endless loop
        };

        // increment clause
        let loop_start = if !self.consume_is(TokenType::RParen) {
            let body_jump = self.emit_jump(Op::Jump);
            let incr_start = self.current_pos();

            // evaluate increment expression
            self.expression()?;
            self.emit_byte(Op::Pop.into()); // remove result again

            if !self.consume_is(TokenType::RParen) {
                return Err(CompileError::ExpectedChar(
                    ')',
                    "for clause".to_string(),
                    self.previous.unwrap(),
                ));
            }

            // jump to before the condition, to evaluate the condition
            self.emit_loop(pre_condition);

            // patch the body jump
            self.patch_jump(body_jump);

            incr_start
        } else {
            // we dont have an increment step, jump back to the condition
            pre_condition
        };

        self.ctx.loop_scopes.push(LoopScope {
            start: loop_start,
            exit_jumps: Vec::new(),
        });

        self.statement()?;

        self.emit_loop(loop_start);

        // we reached the end of the loop, patch that as the exit
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(Op::Pop.into());
        }

        self.pop_loop_scope();

        self.end_scope();

        Ok(())
    }

    fn continue_stmt(&mut self) -> ParseResult {
        let stmt = self.previous.unwrap();

        if !self.consume_is(TokenType::Semicolon) {
            return Err(CompileError::ExpectedChar(
                ';',
                "continue statement".to_string(),
                self.current.unwrap(),
            ));
        }

        if let Some(loop_start) = self.ctx.loop_scopes.last() {
            self.emit_loop(loop_start.start);
            Ok(())
        } else {
            Err(CompileError::IllegalStatement("continue".to_string(), stmt))
        }
    }

    fn break_stmt(&mut self) -> ParseResult {
        let stmt = self.previous.unwrap();

        if !self.consume_is(TokenType::Semicolon) {
            return Err(CompileError::ExpectedChar(
                ';',
                "break statement".to_string(),
                self.current.unwrap(),
            ));
        }

        if self.ctx.loop_scopes.last().is_none() {
            return Err(CompileError::IllegalStatement("break".to_string(), stmt));
        }

        let jump_exit = self.emit_jump(Op::Jump);
        self.ctx
            .loop_scopes
            .last_mut()
            .unwrap()
            .exit_jumps
            .push(jump_exit);

        Ok(())
    }

    fn return_stmt(&mut self) -> ParseResult {
        if self.ctx.fun_type == FunctionType::Script {
            panic!("cant return from top level code");
        }

        if self.consume_is(TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression()?;

            if !self.consume_is(TokenType::Semicolon) {
                return Err(CompileError::ExpectedChar(
                    ';',
                    "break statement".to_string(),
                    self.current.unwrap(),
                ));
            }

            self.emit_byte(Op::Return.into());
        }

        Ok(())
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
        self.named_var(&self.previous.unwrap(), can_assign)?;
        Ok(())
    }

    fn named_var(&mut self, token: &Token, can_assign: bool) -> ParseResult {
        let (arg, op_get, op_set) =
            if let Some(arg) = self.ctx.resolve_local(&self.scanner, token)? {
                (arg, Op::GetLocal, Op::SetLocal)
            } else if let Some(arg) = self.ctx.resolve_upvalue(&self.scanner, token)? {
                (arg, Op::GetUpvalue, Op::SetUpvalue)
            } else {
                let arg = self.identifier_constant(token);
                (arg, Op::GetGlobal, Op::SetGlobal)
            };

        if can_assign && self.consume_is(TokenType::Equal) {
            self.expression()?;
            self.emit_bytes(op_set.into(), arg);
            return Ok(());
        }

        self.emit_bytes(op_get.into(), arg);

        Ok(())
    }

    fn last_local(&self) -> &Local {
        self.ctx.locals.last().unwrap()
    }

    fn and(&mut self) -> ParseResult {
        let end_jump = self.emit_jump(Op::JumpIfFalse);

        self.emit_byte(Op::Pop.into());
        self.parse_precedence(Precedence::And)?;

        self.patch_jump(end_jump);

        Ok(())
    }

    fn or(&mut self) -> ParseResult {
        let else_jump = self.emit_jump(Op::JumpIfFalse);
        let end_jump = self.emit_jump(Op::Jump);

        self.patch_jump(else_jump);
        self.emit_byte(Op::Pop.into());

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);

        Ok(())
    }

    fn call(&mut self) -> ParseResult {
        let arg_count = self.argument_list()?;
        self.emit_bytes(Op::Call.into(), arg_count);
        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8, CompileError> {
        let mut arg_count = 0;

        if !self.current_is(TokenType::RParen) {
            self.expression()?;
            arg_count += 1;

            while self.consume_is(TokenType::Comma) {
                if arg_count >= 255 {
                    panic!("too many arguments");
                }

                self.expression()?;
                arg_count += 1;
            }
        }

        if !self.consume_is(TokenType::RParen) {
            return Err(CompileError::ExpectedChar(
                ')',
                "arguments".to_string(),
                self.previous.unwrap(),
            ));
        }

        Ok(arg_count)
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

    fn chunk(&self) -> &Chunk {
        &self.ctx.fun.as_ref().unwrap().chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.ctx.fun.as_mut().unwrap().chunk
    }
}

impl Context {
    fn resolve_local(
        &mut self,
        scanner: &Scanner,
        token: &Token,
    ) -> Result<Option<u8>, CompileError> {
        for i in (0..self.locals.len()).rev() {
            let local = self.locals.get(i).unwrap();

            if identifier_equals(scanner, token, &local.token) {
                if local.depth == -1 {
                    return Err(CompileError::CantUseLocalVarInItsOwnInitializer(
                        local.token,
                    ));
                }

                return Ok(Some(i as u8));
            }
        }

        Ok(None)
    }

    fn resolve_upvalue(
        &mut self,
        scanner: &Scanner,
        token: &Token,
    ) -> Result<Option<u8>, CompileError> {
        if self.parent.is_none() {
            return Ok(None);
        }

        if let Some(local) = self
            .parent
            .as_mut()
            .unwrap()
            .resolve_local(scanner, token)?
        {
            self.parent.as_mut().unwrap().locals[local as usize].is_captured = true;
            return Ok(Some(self.add_upvalue(local.clone(), true)));
        }

        if self.parent.is_none() {
            return Ok(None);
        }

        if let Some(upvalue) = self
            .parent
            .as_mut()
            .unwrap()
            .resolve_upvalue(scanner, token)?
        {
            return Ok(Some(self.add_upvalue(upvalue, false)));
        }

        Ok(None)
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> u8 {
        if self.upvalues.len() >= u8::MAX as usize {
            panic!("too many closure variables in function");
        }

        if let Some(index) = self.upvalues.iter().position(|u| u.index == index) {
            return index.try_into().unwrap();
        }

        self.fun.as_mut().unwrap().upvalue_count += 1;
        self.upvalues.push(Upvalue { index, is_local });

        self.upvalues
            .len()
            .checked_sub(1)
            .unwrap()
            .try_into()
            .unwrap()
    }
}

fn identifier_equals(scanner: &Scanner, a: &Token, b: &Token) -> bool {
    a.token_type == b.token_type
        && a.length == b.length
        && scanner.token_data(a) == scanner.token_data(b)
}

pub fn compile(code: String) -> Result<Function, CompileError> {
    let mut compiler = Compiler::new(code);
    compiler.compile()
}
