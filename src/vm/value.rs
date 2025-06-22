use std::fmt::Display;

use crate::vm::{chunk::Chunk, vm::RuntimeError};

pub type NativeFn = fn(Vec<Value>) -> Result<Value, RuntimeError>;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Function(Function),
    NativeFunction(NativeFn, usize),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Value::String(str) => format!("{str}"),
            Value::Number(num) => format!("{num}"),
            Value::Bool(b) => format!("{b}"),
            Value::Nil => format!("nil"),
            Value::Function(fun) => {
                format!(
                    "<fn{}/{}()>",
                    fun.name
                        .as_ref()
                        .and_then(|name| Some(format!(" {name}")))
                        .unwrap_or_default(),
                    fun.arity
                )
            }
            Value::NativeFunction(fun, arity) => {
                format!("<native fn {fun:?}/{arity}()>")
            }
        };

        write!(f, "{res}")?;

        Ok(())
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl TryInto<f64> for Value {
    type Error = ();

    fn try_into(self) -> Result<f64, Self::Error> {
        if let Value::Number(n) = self {
            Ok(n)
        } else {
            Err(())
        }
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl TryInto<String> for Value {
    type Error = ();

    fn try_into(self) -> Result<String, Self::Error> {
        if let Value::String(s) = self {
            Ok(s)
        } else {
            Err(())
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        self.is_truthy()
    }
}

#[derive(Debug)]
pub enum ValueError {
    CantCompare(Value, Value),
    UnaryOpInvalidTypes(Value),
    BinaryOpInvalidTypes(Value, Value),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Comp {
    Equal,
    Greater,
    Lesser,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(b) => !*b,
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => (a - b).abs() < f64::EPSILON,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (_, Value::Nil) => false,
            (Value::Nil, _) => false,
            (_, _) => false,
        }
    }

    pub fn compare(&self, other: &Value) -> Result<Comp, ValueError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => match (a, b) {
                _ if (a - b).abs() < f64::EPSILON => Ok(Comp::Equal),
                _ if a > b => Ok(Comp::Greater),
                _ => Ok(Comp::Lesser),
            },
            (a, b) => Err(ValueError::CantCompare(a.clone(), b.clone())),
        }
    }

    pub fn add(&self, other: &Value) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::String(a), Value::String(b)) => Ok(format!("{a}{b}").into()),
            (Value::String(a), Value::Number(b)) => Ok(format!("{a}{b}").into()),
            (Value::Number(a), Value::Number(b)) => Ok((a + b).into()),
            (a, b) => Err(ValueError::BinaryOpInvalidTypes(a.clone(), b.clone())),
        }
    }

    pub fn sub(&self, other: &Value) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok((a - b).into()),
            (a, b) => Err(ValueError::BinaryOpInvalidTypes(a.clone(), b.clone())),
        }
    }

    pub fn mul(&self, other: &Value) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::String(a), Value::Number(b)) => Ok(a.repeat(b.floor() as usize).into()),
            (Value::Number(a), Value::Number(b)) => Ok((a * b).into()),
            (a, b) => Err(ValueError::BinaryOpInvalidTypes(a.clone(), b.clone())),
        }
    }

    pub fn div(&self, other: &Value) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok((a / b).into()),
            (a, b) => Err(ValueError::BinaryOpInvalidTypes(a.clone(), b.clone())),
        }
    }

    pub fn modulo(&self, other: &Value) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok((a % b).into()),
            (a, b) => Err(ValueError::BinaryOpInvalidTypes(a.clone(), b.clone())),
        }
    }

    pub fn negate(&self) -> Result<Value, ValueError> {
        match self {
            Value::Number(num) => Ok((-num).into()),
            _ => Err(ValueError::UnaryOpInvalidTypes(self.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub arity: usize,
    pub chunk: Chunk,
}

impl Function {
    pub fn new(name: String, arity: usize) -> Self {
        Self {
            name: Some(name),
            arity,
            chunk: Chunk::default(),
        }
    }
}
