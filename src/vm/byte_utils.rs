use crate::vm::bytecode::Opcode;
use std::mem;

//TODO Is there a more efficient and simpler way to do this?
pub fn read_24_bytes(bytes: &[u8]) -> usize {
    assert_eq!(bytes.len(), 3);
    bytes[0] as usize | (bytes[1] as usize) << 8 | (bytes[2] as usize) << 16
}

pub fn read_bytes(slice: &[u8]) -> usize {
    assert!(slice.len() <= std::mem::size_of::<usize>());
    let mut out: usize = 0;
    for &b in slice.iter() {
        out = out << 8 | b as usize;
    }
    out
}

pub fn read_16_bytes(bytes: &[u8]) -> usize {
    assert_eq!(bytes.len(), 2);
    bytes[1] as usize | (bytes[0] as usize) << 8
}

//TODO is this the best way to turn byte into instruction?
#[inline]
pub fn byte_to_opcode(byte: u8) -> Opcode {
    unsafe { mem::transmute::<u8, Opcode>(byte) }
}
