use std::collections::HashMap;

use crate::{
    constants::DEBUG_MODE,
    vm::{
        builtins::{lox_assert, lox_panic, lox_time, lox_tostring},
        chunk::Chunk,
        compiler::{CompileError, compile},
        op::Op,
        value::{Comp, Function, NativeFn, Value, ValueError},
    },
};

#[derive(Debug)]
pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

#[derive(Debug)]
struct CallFrame {
    fun: Function,
    ip: usize,
    stack_base_index: usize,
}

#[derive(Debug)]
pub enum VMError {
    ValueError(ValueError),
    CompileError(CompileError),
    RuntimeError(RuntimeError),
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

impl From<RuntimeError> for VMError {
    fn from(value: RuntimeError) -> Self {
        VMError::RuntimeError(value)
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UndefinedVariable(String),
    StackOverflow,
    FunArgumentCountMismatch(usize, usize),
    AssertionFailed(String),
    Panic(String),
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
        };

        vm.define_native("time", lox_time, 0);
        vm.define_native("tostring", lox_tostring, 1);
        vm.define_native("assert", lox_assert, 2);
        vm.define_native("panic", lox_panic, 1);

        vm
    }

    pub fn run(&mut self, code: String) -> Result<Value, VMError> {
        let fun = compile(code)?;

        self.stack.push(Value::Function(fun.clone()));
        self.call_function(fun, 0)?;

        self.interpret()
    }

    fn interpret(&mut self) -> Result<Value, VMError> {
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
                        self.push(constant.clone());
                    }
                    Op::Nil => self.push(Value::Nil),
                    Op::True => self.push(Value::Bool(true)),
                    Op::False => self.push(Value::Bool(false)),
                    Op::Pop => {
                        self.pop();
                    }
                    Op::Dup => {
                        self.push(self.peek().clone());
                    }
                    Op::GetGlobal => {
                        let name = self.read_constant().unwrap().clone();

                        if let Value::String(name) = name {
                            if let Some(value) = self.globals.get(&name) {
                                self.push(value.clone());
                            } else {
                                return Err(RuntimeError::UndefinedVariable(name).into());
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
                                return Err(RuntimeError::UndefinedVariable(name).into());
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
                        let slot = self.read_u8().unwrap() as usize;
                        let value = self
                            .stack
                            .get(self.frame().stack_base_index + slot)
                            .unwrap();
                        self.push(value.clone());
                    }
                    Op::SetLocal => {
                        let slot = self.read_u8().unwrap() as usize;
                        let frame_base_ptr = self.frame().stack_base_index;
                        self.stack[frame_base_ptr + slot] = self.peek().clone();
                    }

                    // unary operations
                    Op::Negate => {
                        let value = self.pop();
                        self.push(value.negate()?);
                    }
                    Op::Not => {
                        let value = self.pop();
                        self.push((!value.is_truthy()).into());
                    }

                    // binary operations
                    Op::Add => {
                        let (a, b) = self.pop2();
                        self.push(a.add(&b)?.into());
                    }
                    Op::Subtract => {
                        let (a, b) = self.pop2();
                        self.push(a.sub(&b)?.into());
                    }
                    Op::Multiply => {
                        let (a, b) = self.pop2();
                        self.push(a.mul(&b)?.into());
                    }
                    Op::Divide => {
                        let (a, b) = self.pop2();
                        self.push(a.div(&b)?.into());
                    }
                    Op::Equal => {
                        let (a, b) = self.pop2();
                        self.push(a.equals(&b).into());
                    }
                    Op::Greater => {
                        let (a, b) = self.pop2();
                        self.push((a.compare(&b)? == Comp::Greater).into());
                    }
                    Op::Less => {
                        let (a, b) = self.pop2();
                        self.push((a.compare(&b)? == Comp::Lesser).into());
                    }
                    Op::Modulo => {
                        let (a, b) = self.pop2();
                        self.push(a.modulo(&b)?);
                    }

                    Op::Print => {
                        let value = self.pop();
                        println!("{value}");
                    }
                    Op::Jump => {
                        let offset = self.read_u16().unwrap();
                        self.frame_mut().ip += offset as usize;
                    }
                    Op::JumpIfFalse => {
                        let offset = self.read_u16().unwrap();

                        if self.peek().is_falsey() {
                            self.frame_mut().ip += offset as usize;
                        }
                    }
                    Op::Loop => {
                        let offset = self.read_u16().unwrap();
                        self.frame_mut().ip -= offset as usize;
                    }
                    Op::Call => {
                        let arity = self.read_u8().unwrap() as usize;
                        let callee = self.peek_at(arity as usize).clone();

                        match callee {
                            Value::Function(fun) => self.call_function(fun, arity)?,
                            Value::NativeFunction(fun, fun_arity) => {
                                if arity != fun_arity {
                                    return Err(RuntimeError::FunArgumentCountMismatch(
                                        arity, fun_arity,
                                    )
                                    .into());
                                }

                                let mut args = vec![];

                                for _ in 0..arity {
                                    args.push(self.pop());
                                }

                                args.reverse();

                                let res = fun(args)?;
                                self.pop();

                                self.stack.push(res);
                            }
                            _ => panic!("can only call functions"),
                        };
                    }

                    Op::Return => {
                        let value = self.pop();

                        // remove items from stack until we're back at the stack base index
                        let base_index = self.frame().stack_base_index;
                        self.stack.truncate(base_index);

                        self.frames.pop();

                        if self.frames.len() == 0 {
                            return Ok(value);
                        }

                        self.push(value);
                    }
                }
            } else {
                break;
            }
        }

        Ok(Value::Nil)
    }

    fn call_function(&mut self, fun: Function, arity: usize) -> Result<(), RuntimeError> {
        if arity != fun.arity {
            self.print_stacktrace();
            return Err(RuntimeError::FunArgumentCountMismatch(arity, fun.arity));
        }

        if self.frames.len() >= 255 {
            self.print_stacktrace();
            return Err(RuntimeError::StackOverflow);
        }

        self.frames.push(CallFrame {
            fun,
            ip: 0,
            stack_base_index: self.stack.len() - 1 - arity,
        });

        Ok(())
    }

    fn define_native(&mut self, name: &str, fun: NativeFn, arity: usize) {
        self.globals
            .insert(name.to_string(), Value::NativeFunction(fun, arity));
    }

    fn print_stacktrace(&self) {
        for frame in self.frames.iter().rev() {
            println!(
                "[line {}] in {}",
                frame.fun.chunk.get_line(frame.ip).unwrap(),
                frame.fun.name.as_ref().unwrap_or(&"???".to_string())
            );
        }
    }

    fn read_u8(&mut self) -> Option<u8> {
        if self.frame().ip >= self.chunk().len() {
            None
        } else {
            let res = self.chunk().read_u8(self.frame().ip).copied();
            self.frame_mut().ip += 1;
            res
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
            self.chunk().read_constant(index)
        } else {
            None
        }
    }

    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn peek_at(&self, num: usize) -> &Value {
        self.stack.get(self.stack.len() - 1 - num).unwrap()
    }

    fn push(&mut self, value: Value) {
        if self.stack.len() > u8::MAX as usize {
            panic!("stack overflow");
        }

        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn pop2(&mut self) -> (Value, Value) {
        let b = self.pop();
        let a = self.pop();
        (a, b)
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn chunk(&self) -> &Chunk {
        &self.frame().fun.chunk
    }

    fn debug_ip(&self) {
        if !DEBUG_MODE {
            return;
        }

        println!(
            "\x1b[34;2m--->      STACK:              {}: [{}]\x1b[0m",
            self.frame().stack_base_index,
            self.stack
                .iter()
                .map(|var| format!("{var}"))
                .collect::<Vec<String>>()
                .join(", ")
        );

        self.frame().fun.chunk.debug_op(self.frame().ip);
    }
}
