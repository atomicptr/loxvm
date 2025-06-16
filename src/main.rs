use crate::vm::{chunk::Chunk, op};

mod vm;

fn main() {
    let mut chunk = Chunk::default();

    let constant = chunk.push_constant(13.37);
    chunk.push(op::CONSTANT, 1);
    chunk.push(constant, 1);

    chunk.push(op::RETURN, 1);

    chunk.debug("Test");
}
