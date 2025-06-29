use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    constants::DEBUG_MODE,
    vm::{
        builtins::{lox_assert, lox_panic, lox_time, lox_tostring},
        chunk::Chunk,
        compiler::{CompileError, compile},
        op::Op,
        value::{Closure, Comp, Function, NativeFn, Upvalue, Value, ValueError},
    },
};

#[derive(Debug)]
pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Debug)]
struct CallFrame {
    closure: Closure,
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
            stack: Vec::with_capacity(u8::MAX as usize),
            frames: Vec::with_capacity(u8::MAX as usize),
            globals: HashMap::new(),
            upvalues: Vec::with_capacity(u8::MAX as usize),
        };

        vm.define_native("time", lox_time, 0);
        vm.define_native("tostring", lox_tostring, 1);
        vm.define_native("assert", lox_assert, 2);
        vm.define_native("panic", lox_panic, 1);

        vm
    }

    pub fn run(&mut self, code: String) -> Result<Value, VMError> {
        let fun = compile(code)?;
        let fun = Rc::new(fun);

        self.stack.clear();

        self.stack.push(Value::Function(fun.clone()));
        self.call_function(fun, 0, Vec::new())?;

        let res = self.interpret();

        res
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
                        self.push(constant);
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
                            if let Some(value) = self.globals.get(&*name) {
                                self.push(value.clone());
                            } else {
                                return Err(
                                    RuntimeError::UndefinedVariable(name.to_string()).into()
                                );
                            }
                        } else {
                            unreachable!();
                        }
                    }
                    Op::SetGlobal => {
                        let name = self.read_constant().unwrap().clone();

                        if let Value::String(name) = name {
                            if let Some(_) = self.globals.get(&*name) {
                                let value = self.peek().clone();
                                self.globals.insert(name.to_string(), value);
                            } else {
                                return Err(
                                    RuntimeError::UndefinedVariable(name.to_string()).into()
                                );
                            }
                        } else {
                            unreachable!();
                        }
                    }
                    Op::DefineGlobal => {
                        let name = self.read_constant().unwrap().clone();

                        if let Value::String(name) = name {
                            let data = self.pop();
                            self.globals.insert(name.to_string(), data);
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
                    Op::GetUpvalue => {
                        let slot = self.read_u8().unwrap();

                        let upvalue = self.frame().closure.upvalues.get(slot as usize).unwrap();

                        self.push(if upvalue.borrow().closed.is_some() {
                            upvalue.borrow().closed.as_ref().unwrap().borrow().clone()
                        } else {
                            self.stack.get(upvalue.borrow().index).unwrap().clone()
                        });
                    }
                    Op::SetUpvalue => {
                        let slot = self.read_u8().unwrap();
                        let upvalue = self
                            .frame_mut()
                            .closure
                            .upvalues
                            .get(slot as usize)
                            .unwrap()
                            .clone();

                        let value = self.peek().clone();

                        if upvalue.borrow().closed.is_some() {
                            upvalue
                                .borrow_mut()
                                .closed
                                .replace(Rc::new(RefCell::new(value)));
                        } else {
                            self.stack[upvalue.borrow().index] = value;
                        }
                    }
                    Op::CloseUpvalue => {
                        self.close_upvalue(self.stack.len() - 1);
                        self.pop();
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
                            Value::Closure(closure) => {
                                self.call_function(closure.fun, arity, closure.upvalues)?
                            }
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
                    Op::Closure => {
                        // remove the name
                        let _ = self.read_constant().unwrap();

                        let constant = self.read_constant().unwrap();
                        let Value::Function(fun) = constant.clone() else {
                            unreachable!("Unexpected: {constant:?}");
                        };

                        let mut upvalues = Vec::with_capacity(fun.upvalue_count);

                        for _ in 0..fun.upvalue_count {
                            let is_local = self.read_u8().unwrap() == 1;
                            let index = self.read_u8().unwrap() as usize;

                            if is_local {
                                upvalues.push(self.capture_upvalue(index));
                                continue;
                            }

                            upvalues
                                .push(self.frame().closure.upvalues.get(index).unwrap().clone());
                        }

                        self.push(Value::Closure(Closure { fun, upvalues }));
                    }

                    Op::Return => {
                        let value = self.pop();
                        self.close_upvalue(self.frame().stack_base_index);

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

    fn call_function(
        &mut self,
        fun: Rc<Function>,
        arity: usize,
        upvalues: Vec<Rc<RefCell<Upvalue>>>,
    ) -> Result<(), RuntimeError> {
        if arity != fun.arity {
            self.print_stacktrace();
            return Err(RuntimeError::FunArgumentCountMismatch(arity, fun.arity));
        }

        if self.frames.len() >= 255 {
            self.print_stacktrace();
            return Err(RuntimeError::StackOverflow);
        }

        self.frames.push(CallFrame {
            closure: Closure { fun, upvalues },
            ip: 0,
            stack_base_index: self.stack.len() - 1 - arity,
        });

        Ok(())
    }

    fn capture_upvalue(&mut self, index: usize) -> Rc<RefCell<Upvalue>> {
        let upvalue = self
            .upvalues
            .iter()
            .find(|upvalue| upvalue.borrow().index <= index);

        if let Some(upvalue) = upvalue {
            if upvalue.borrow().index == index {
                return upvalue.clone();
            }
        }

        let upvalue = Rc::new(RefCell::new(Upvalue {
            index: self.frame().stack_base_index + index,
            closed: None,
        }));

        self.upvalues.push(upvalue.clone());

        upvalue
    }

    fn close_upvalue(&mut self, until: usize) {
        let mut count = 0;

        for upvalue in self.upvalues.iter().rev() {
            // should be closed
            let index = upvalue.borrow().index;
            if index >= until {
                count += 1;

                let value = self.stack.get(index).unwrap();
                upvalue.borrow_mut().closed = Some(Rc::new(RefCell::new(value.clone())));
            }
        }

        self.upvalues.truncate(self.upvalues.len() - count);
    }

    fn define_native(&mut self, name: &str, fun: NativeFn, arity: usize) {
        self.globals
            .insert(name.to_string(), Value::NativeFunction(fun, arity));
    }

    fn print_stacktrace(&self) {
        for frame in self.frames.iter().rev() {
            println!(
                "[line {}] in {}",
                frame.closure.fun.chunk.get_line(frame.ip).unwrap(),
                frame
                    .closure
                    .fun
                    .name
                    .as_ref()
                    .unwrap_or(&"???".to_string())
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
        &self.frame().closure.fun.chunk
    }

    fn debug_ip(&self) {
        if !DEBUG_MODE {
            return;
        }

        println!(
            "\x1b[34;2m--->      STACK:              [{}\x1b[0m\x1b[34;2m] ({})\x1b[0m",
            self.stack
                .iter()
                .enumerate()
                .map(|(i, var)| format!(
                    "{}{var}",
                    if i >= self.frame().stack_base_index {
                        "\x1b[33;1m"
                    } else {
                        ""
                    }
                ))
                .collect::<Vec<String>>()
                .join(", "),
            self.frame().closure.fun.name.as_ref().unwrap()
        );

        if self.upvalues.len() > 0 {
            println!(
                "\x1b[31;2m--->      UPVALUES (OPEN):    [{}]\x1b[0m",
                self.upvalues
                    .iter()
                    .map(|up| format!("{}", up.borrow().index))
                    .collect::<Vec<String>>()
                    .join(", "),
            );
        }

        if self.frame().closure.upvalues.len() > 0 {
            println!(
                "\x1b[31;2m--->      UPVALUES (FRAME):   [{}] ({})\x1b[0m",
                self.frame()
                    .closure
                    .upvalues
                    .iter()
                    .map(|up| if let Some(closed) = &up.borrow().closed {
                        format!("{:?}", closed.borrow())
                    } else {
                        format!("{}", up.borrow().index)
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
                self.frame().closure.fun.name.clone().unwrap_or_default()
            );
        }

        self.frame().closure.fun.chunk.debug_op(self.frame().ip);
    }
}
