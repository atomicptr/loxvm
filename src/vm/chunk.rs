use crate::vm::{op, value::Value};

#[derive(Debug, Default)]
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

    pub fn push_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1)
            .try_into()
            .expect("err: too many constants defined")
    }

    fn get_line(&self, offset: usize) -> Option<usize> {
        self.lines.get(offset)
    }

    pub fn debug(&self, name: &str) {
        println!("========= {name} =========");

        let mut offset = 0;

        while offset < self.code.len() {
            offset = self.debug_op(offset);
        }
    }

    fn debug_op(&self, offset: usize) -> usize {
        let op = self
            .code
            .get(offset)
            .expect(format!("error: could not access offset: {offset}").as_str());

        let line = self
            .get_line(offset)
            .expect(format!("error: could not access line offset: {offset}").as_str());

        print!("{offset:04} ");

        if offset > 0 && line == self.get_line(offset - 1).unwrap() {
            print!("   | ");
        } else {
            print!("{:>4} ", line);
        }

        match *op {
            op::RETURN => self.debug_op_simple("OP_RETURN", offset),
            op::CONSTANT => self.debug_op_constant("OP_CONSTANT", offset),

            _ => {
                println!("Unknown opcode: {op}");
                offset + 1
            }
        }
    }

    fn debug_op_simple(&self, name: &str, offset: usize) -> usize {
        println!("{name:<16}");
        offset + 1
    }

    fn debug_op_constant(&self, name: &str, offset: usize) -> usize {
        let constant = self
            .code
            .get(offset + 1)
            .expect(format!("error: could not access offset: {}", offset + 1).as_str());

        let value = self
            .constants
            .get(constant.clone() as usize)
            .expect(format!("error: could not access constant offset: {constant}").as_str());

        println!("{name:<16} {constant:>4} {value}");

        offset + 2
    }
}

// Simple Run-Length Encoding for line numbers
#[derive(Debug, Default)]
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
