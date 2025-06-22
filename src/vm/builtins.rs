use std::time::{SystemTime, UNIX_EPOCH};

use crate::vm::{value::Value, vm::RuntimeError};

pub fn lox_time(_args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time went backwards")
            .as_secs_f64(),
    ))
}

pub fn lox_tostring(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let value = args.first().unwrap().clone();

    match value {
        Value::String(s) => Ok(Value::String(s)),
        Value::Number(n) => Ok(Value::String(format!("{n}").into())),
        Value::Bool(b) => Ok(Value::String(format!("{b}").into())),
        Value::Nil => Ok(Value::String("nil".to_string().into())),
        _ => todo!(),
    }
}

pub fn lox_assert(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let condition = args.get(0).unwrap().clone();
    let message = args.get(1).unwrap().clone();

    if condition.is_truthy() {
        return Ok(Value::Nil);
    }

    Err(RuntimeError::AssertionFailed(message.to_string()))
}

pub fn lox_panic(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let message = args.get(0).unwrap().clone();
    Err(RuntimeError::Panic(message.to_string()))
}
