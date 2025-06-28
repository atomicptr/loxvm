#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Op {
    Constant,
    Nil,
    True,
    False,
    Pop,
    Dup,
    GetGlobal,
    SetGlobal,
    DefineGlobal,
    GetLocal,
    SetLocal,
    GetUpvalue,
    SetUpvalue,

    // unary operations
    Not,
    Negate,

    // binary operations
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Greater,
    Less,
    Modulo,

    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Closure,

    // keep this last (or edit From<u8>)
    Return,
}

impl Into<u8> for Op {
    fn into(self) -> u8 {
        self as u8
    }
}

impl From<u8> for Op {
    fn from(value: u8) -> Self {
        if value > Op::Return.into() {
            panic!("attempt to convert invalid number to opcode");
        }

        unsafe { std::mem::transmute(value) }
    }
}

impl Op {
    pub fn name(&self) -> String {
        let s: String = format!("{self:?}")
            .chars()
            .map(|c| {
                if c.is_uppercase() {
                    format!("_{c}")
                } else {
                    c.to_uppercase().to_string()
                }
            })
            .collect();
        s.trim_matches('_').to_string()
    }
}
