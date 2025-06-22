use crate::vm::{op::Op, value::Value};

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: LineVec,
}

impl Chunk {
    pub fn push(&mut self, data: u8, line: usize) {
        self.code.push(data);
        self.lines.push(line);
    }

    pub fn push_constant(&mut self, value: Value, line: usize) {
        self.push(Op::Constant.into(), line.clone());

        let index = self.make_constant(value);
        self.push(index, line);
    }

    pub fn make_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1)
            .try_into()
            .expect("err: too many constants defined")
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn read_u8(&self, index: usize) -> Option<&u8> {
        self.code.get(index)
    }

    pub fn read_constant(&self, index: u8) -> Option<&Value> {
        self.constants.get(index as usize)
    }

    pub fn patch(&mut self, index: usize, data: u8) {
        self.code[index] = data;
    }

    pub fn get_line(&self, offset: usize) -> Option<usize> {
        self.lines.get(offset)
    }

    pub fn debug(&self, name: &str) {
        println!("");
        println!("========= {name} =========");

        let mut offset = 0;

        while offset < self.code.len() {
            offset = self.debug_op(offset);
        }

        println!("=========={}==========", "=".repeat(name.len()));
        println!("");
    }

    pub fn debug_op(&self, offset: usize) -> usize {
        let op = self
            .code
            .get(offset)
            .expect(format!("error: could not access offset: {offset}").as_str());

        let line = self
            .get_line(offset)
            .expect(format!("error: could not access line offset: {offset}").as_str());

        print!("\x1b[32;2m{offset:04} ");

        if offset > 0 && line == self.get_line(offset - 1).unwrap() {
            print!("   | ");
        } else {
            print!("{:>4} ", line);
        }

        let op = Op::from(*op);

        match op {
            Op::Constant => self.debug_op_constant(op, offset),
            Op::GetGlobal => self.debug_op_constant(op, offset),
            Op::SetGlobal => self.debug_op_constant(op, offset),
            Op::DefineGlobal => self.debug_op_constant(op, offset),
            Op::GetLocal => self.debug_op_byte(op, offset),
            Op::SetLocal => self.debug_op_byte(op, offset),
            Op::Jump => self.debug_op_jump(op, 1, offset),
            Op::JumpIfFalse => self.debug_op_jump(op, 1, offset),
            Op::Loop => self.debug_op_jump(op, -1, offset),
            Op::Call => self.debug_op_byte(op, offset),
            op => self.debug_op_simple(op, offset),
        }
    }

    fn debug_op_simple(&self, op: Op, offset: usize) -> usize {
        println!("{:<16}\x1b[0m", op.name());
        offset + 1
    }

    fn debug_op_constant(&self, op: Op, offset: usize) -> usize {
        let constant = self
            .code
            .get(offset + 1)
            .expect(format!("error: could not access offset: {}", offset + 1).as_str());

        let value = self
            .constants
            .get(constant.clone() as usize)
            .expect(format!("error: could not access constant offset: {constant}").as_str());

        println!("{:<16}    {constant:04} {value}\x1b[0m", op.name());

        offset + 2
    }

    fn debug_op_byte(&self, op: Op, offset: usize) -> usize {
        let slot = self
            .code
            .get(offset + 1)
            .expect(format!("error: could not access offset: {}", offset + 1).as_str());

        println!("{:<16} {slot:>4}\x1b[0m", op.name());

        offset + 2
    }

    fn debug_op_jump(&self, op: Op, sign: i32, offset: usize) -> usize {
        let a = self.read_u8(offset + 1).unwrap().clone() as usize;
        let b = self.read_u8(offset + 2).unwrap().clone() as usize;
        let jump = a << 8 | b;

        println!(
            "{:<16}    {offset:04} -> {:04}\x1b[0m",
            op.name(),
            offset as i32 + 3 + sign * jump as i32,
        );

        offset + 3
    }
}

// Simple Run-Length Encoding for line numbers
#[derive(Debug, Default, Clone)]
struct LineVec {
    data: Vec<(usize, u8)>,
}

impl LineVec {
    pub fn push(&mut self, line: usize) {
        match self.data.last_mut() {
            Some(l) if l.0 == line && l.1 < u8::MAX => {
                l.1 += 1;
            }
            _ => self.data.push((line, 1)),
        }
    }

    pub fn get(&self, offset: usize) -> Option<usize> {
        let mut index = 0;
        for (linenum, count) in &self.data {
            let count = *count as usize;

            if (offset - index) < count {
                return Some(linenum.clone());
            }

            index += count;
        }

        None
    }
}

#[cfg(test)]

mod tests {
    use crate::vm::chunk::LineVec;

    #[test]
    fn test_line_vec_rle() {
        let mut lv = LineVec::default();

        for _ in 0..4 {
            lv.push(4);
        }

        for _ in 0..2 {
            lv.push(5);
        }

        for _ in 0..2 {
            lv.push(6);
        }

        assert_eq!(lv.get(0), Some(4));
        assert_eq!(lv.get(3), Some(4));
        assert_eq!(lv.get(4), Some(5));
        assert_eq!(lv.get(5), Some(5));
        assert_eq!(lv.get(6), Some(6));
        assert_eq!(lv.get(7), Some(6));
        assert_eq!(lv.get(8), None);
    }
}
