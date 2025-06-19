#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Op {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetGlobal,
    SetGlobal,
    DefineGlobal,

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

    Print,

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
