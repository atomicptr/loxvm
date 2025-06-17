use crate::vm::{chunk::Chunk, op, vm::VM};

mod vm;

fn main() {
    let mut vm = VM::default();

    let mut chunk = Chunk::default();

    // -((1.2 + 3.4) / 5.6)
    chunk.push_constant(1.2, 1);
    chunk.push_constant(3.4, 1);
    chunk.push(op::ADD, 1);

    chunk.push_constant(5.6, 1);
    chunk.push(op::DIVIDE, 1);
    chunk.push(op::NEGATE, 1);

    chunk.push(op::RETURN, 1);

    vm.run(chunk).expect("works");
}
