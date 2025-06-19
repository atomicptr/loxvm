use std::collections::HashMap;

use crate::vm::{
    chunk::Chunk,
    compiler::{CompileError, compile},
    op::{self, Op},
    value::{Comp, Value, ValueError},
};

const DEBUG_MODE: bool = true;

#[derive(Debug, Default)]
pub struct VM {
    chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

#[derive(Debug)]
pub enum VMError {
    ValueError(ValueError),
    CompileError(CompileError),
    UndefinedVariable(String),
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

            if let Some(op) = self.read_u8() {
                let op = Op::from(op);

                match op {
                    Op::Constant => {
                        let constant = self
                            .read_constant()
                            .expect("could not read constant")
                            .clone();
                        self.stack.push(constant.clone());
                    }
                    Op::Nil => self.stack.push(Value::Nil),
                    Op::True => self.stack.push(Value::Bool(true)),
                    Op::False => self.stack.push(Value::Bool(false)),
                    Op::Pop => {
                        self.pop();
                    }
                    Op::GetGlobal => {
                        let name = self.read_constant().unwrap().clone();

                        if let Value::String(name) = name {
                            if let Some(value) = self.globals.get(&name) {
                                self.stack.push(value.clone());
                            } else {
                                return Err(VMError::UndefinedVariable(name));
                            }
                        } else {
                            unreachable!();
                        }
                    }
                    Op::SetGlobal => {
                        let name = self.read_constant().unwrap().clone();

                        if let Value::String(name) = name {
                            if let Some(_) = self.globals.get(&name) {
                                let value = self.peek().clone();
                                self.globals.insert(name, value);
                            } else {
                                return Err(VMError::UndefinedVariable(name));
                            }
                        } else {
                            unreachable!();
                        }
                    }
                    Op::DefineGlobal => {
                        let name = self.read_constant().unwrap().clone();

                        if let Value::String(name) = name {
                            let data = self.pop();
                            self.globals.insert(name.clone(), data);
                        } else {
                            unreachable!("global name: {name:?}");
                        }
                    }
                    Op::GetLocal => {
                        let slot = self.read_u8().unwrap();
                        let value = self.stack.get(slot as usize).unwrap().clone();
                        self.stack.push(value);
                    }
                    Op::SetLocal => {
                        let slot = self.read_u8().unwrap();
                        self.stack.insert(slot as usize, self.peek().clone());
                    }

                    // unary operations
                    Op::Negate => {
                        let value = self.pop();
                        self.stack.push(value.negate()?);
                    }
                    Op::Not => {
                        let value = self.pop();
                        self.stack.push((!value.is_truthy()).into());
                    }

                    // binary operations
                    Op::Add => {
                        let (a, b) = self.pop2();
                        self.stack.push(a.add(&b)?.into());
                    }
                    Op::Subtract => {
                        let (a, b) = self.pop2();
                        self.stack.push(a.sub(&b)?.into());
                    }
                    Op::Multiply => {
                        let (a, b) = self.pop2();
                        self.stack.push(a.mul(&b)?.into());
                    }
                    Op::Divide => {
                        let (a, b) = self.pop2();
                        self.stack.push(a.div(&b)?.into());
                    }
                    Op::Equal => {
                        let (a, b) = self.pop2();
                        self.stack.push(a.equals(&b).into());
                    }
                    Op::Greater => {
                        let (a, b) = self.pop2();
                        self.stack.push((a.compare(&b)? == Comp::Greater).into());
                    }
                    Op::Less => {
                        let (a, b) = self.pop2();
                        self.stack.push((a.compare(&b)? == Comp::Lesser).into());
                    }

                    Op::Print => {
                        let value = self.pop();
                        println!("{value}");
                    }
                    Op::Jump => {
                        let offset = self.read_u16().unwrap();
                        self.ip += offset as usize;
                    }
                    Op::JumpIfFalse => {
                        let offset = self.read_u16().unwrap();

                        if self.peek().is_falsey() {
                            self.ip += offset as usize;
                        }
                    }
                    Op::Loop => {
                        let offset = self.read_u16().unwrap();
                        self.ip -= offset as usize;
                    }

                    Op::Return => {
                        return Ok(());
                    }
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn read_u8(&mut self) -> Option<u8> {
        if let Some(chunk) = self.chunk.as_ref() {
            if self.ip >= chunk.len() {
                None
            } else {
                let res = chunk.read_u8(self.ip);
                self.ip += 1;
                res.copied()
            }
        } else {
            None
        }
    }

    fn read_u16(&mut self) -> Option<u16> {
        let first = self.read_u8()? as u16;
        let second = self.read_u8()? as u16;

        Some(first << 8 | second)
    }

    fn read_constant(&mut self) -> Option<&Value> {
        if let Some(index) = self.read_u8() {
            let index = index.clone();
            self.chunk
                .as_ref()
                .and_then(|chunk| chunk.read_constant(index))
        } else {
            None
        }
    }

    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn pop2(&mut self) -> (Value, Value) {
        let b = self.pop();
        let a = self.pop();
        (a, b)
    }

    fn debug_ip(&self) {
        if !DEBUG_MODE {
            return;
        }

        println!(
            "\x1b[34;2m--->      STACK:              {:?}\x1b[0m",
            self.stack
        );

        if let Some(chunk) = self.chunk.as_ref() {
            chunk.debug_op(self.ip);
        }
    }
}
