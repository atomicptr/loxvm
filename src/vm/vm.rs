use crate::vm::{
    chunk::Chunk,
    op,
    scanner::{Scanner, ScannerError, TokenType},
    value::Value,
};

const DEBUG_MODE: bool = true;

#[derive(Debug, Default)]
pub struct VM {
    chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum VMError {
    ScannerError(ScannerError),
    CompileError,
    RuntimeError,
}

impl From<ScannerError> for VMError {
    fn from(value: ScannerError) -> Self {
        VMError::ScannerError(value)
    }
}

impl VM {
    pub fn run(&mut self, code: String) -> Result<(), VMError> {
        self.compile(code)
    }

    fn compile(&mut self, code: String) -> Result<(), VMError> {
        let mut scanner = Scanner::new(code);

        let mut line = 0;

        loop {
            let token = scanner.scan()?;

            if token.line != line {
                print!("{:04} ", token.line);
                line = token.line;
            } else {
                print!("   | ");
            }

            println!(
                "{:<12} {:04} {:04} {}",
                format!("{:?}", token.token_type),
                token.start,
                token.length,
                scanner.token_data(&token)
            );

            if token.token_type == TokenType::Eof {
                break;
            }
        }

        Ok(())
    }

    fn interpret(&mut self, chunk: Chunk) -> Result<(), VMError> {
        self.chunk = Some(chunk);
        self.ip = 0;
        self.stack.clear();

        loop {
            self.debug_ip();

            if let Some(op) = self.read_byte() {
                match op {
                    op::CONSTANT => {
                        let constant = self
                            .read_constant()
                            .expect("could not read constant")
                            .clone();
                        self.stack.push(constant);
                    }
                    op::ADD => {
                        let a = self.stack.pop().expect("could not read from stack");
                        let b = self.stack.pop().expect("could not read from stack");
                        self.stack.push(a + b);
                    }
                    op::SUBTRACT => {
                        let a = self.stack.pop().expect("could not read from stack");
                        let b = self.stack.pop().expect("could not read from stack");
                        self.stack.push(a - b);
                    }
                    op::MULTIPLY => {
                        let a = self.stack.pop().expect("could not read from stack");
                        let b = self.stack.pop().expect("could not read from stack");
                        self.stack.push(a * b);
                    }
                    op::DIVIDE => {
                        let a = self.stack.pop().expect("could not read from stack");
                        let b = self.stack.pop().expect("could not read from stack");
                        self.stack.push(a / b);
                    }
                    op::NEGATE => {
                        let value = self.stack.pop().expect("could not read from stack");
                        self.stack.push(-value);
                    }
                    op::RETURN => {
                        if let Some(value) = self.stack.pop() {
                            println!("Return {value:?}");
                        }
                        return Ok(());
                    }
                    _ => panic!("unknown operation: {op}"),
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn read_byte(&mut self) -> Option<u8> {
        if let Some(chunk) = self.chunk.as_ref() {
            if self.ip >= chunk.len() {
                None
            } else {
                let res = chunk.read_byte(self.ip);
                self.ip += 1;
                res.copied()
            }
        } else {
            None
        }
    }

    fn read_constant(&mut self) -> Option<&Value> {
        if let Some(index) = self.read_byte() {
            let index = index.clone();
            self.chunk
                .as_ref()
                .and_then(|chunk| chunk.read_constant(index))
        } else {
            None
        }
    }

    fn debug_ip(&self) {
        if !DEBUG_MODE {
            return;
        }

        println!("--->      STACK:              {:?}", self.stack);

        if let Some(chunk) = self.chunk.as_ref() {
            chunk.debug_op(self.ip);
        }
    }
}
