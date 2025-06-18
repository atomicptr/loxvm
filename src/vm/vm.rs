use crate::vm::{
    chunk::Chunk,
    compiler::{CompileError, compile},
    op,
    value::{Comp, Value, ValueError},
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
    ValueError(ValueError),
    CompileError(CompileError),
    RuntimeError,
}

impl From<ValueError> for VMError {
    fn from(value: ValueError) -> Self {
        VMError::ValueError(value)
    }
}

impl From<CompileError> for VMError {
    fn from(value: CompileError) -> Self {
        VMError::CompileError(value)
    }
}

impl VM {
    pub fn run(&mut self, code: String) -> Result<(), VMError> {
        let chunk = compile(code)?;

        self.chunk = Some(chunk);
        self.ip = 0;
        self.stack.clear();

        self.interpret()
    }

    fn interpret(&mut self) -> Result<(), VMError> {
        loop {
            self.debug_ip();

            if let Some(op) = self.read_byte() {
                match op {
                    op::CONSTANT => {
                        let constant = self
                            .read_constant()
                            .expect("could not read constant")
                            .clone();
                        self.stack.push(constant.clone());
                    }
                    op::ADD => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack.push(a.add(&b)?.into());
                    }
                    op::SUBTRACT => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack.push(a.sub(&b)?.into());
                    }
                    op::MULTIPLY => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack.push(a.mul(&b)?.into());
                    }
                    op::DIVIDE => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack.push(a.div(&b)?.into());
                    }
                    op::NEGATE => {
                        let value = self.stack.pop().unwrap();
                        self.stack.push(value.negate()?);
                    }
                    op::NIL => {
                        self.stack.push(Value::Nil);
                    }
                    op::TRUE => {
                        self.stack.push(Value::Bool(true));
                    }
                    op::FALSE => {
                        self.stack.push(Value::Bool(false));
                    }
                    op::NOT => {
                        let value = self.stack.pop().unwrap();
                        self.stack.push((!value.is_truthy()).into());
                    }
                    op::EQUAL => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack.push(a.equals(&b).into());
                    }
                    op::GREATER => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack.push((a.compare(&b)? == Comp::Greater).into());
                    }
                    op::LESS => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack.push((a.compare(&b)? == Comp::Lesser).into());
                    }
                    op::RETURN => {
                        if let Some(value) = self.stack.pop() {
                            println!("{value:?}");
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
