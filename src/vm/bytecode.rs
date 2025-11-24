use crate::vm::value::Value;

pub type Bytecode = u8;
#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Opcode {
    Constant,
    ConstantLong,

    Negate,
    Subtract,
    Add,
    Divide,
    Multiply,
    Not,

    Equal,
    Greater,
    Less,
    Pop,
    JumpIfFalse,
    Jump,
    JumpBack,

    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,
    Call,

    Return,
    Halt,
}
#[derive(Debug)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub instructions: Vec<Bytecode>,
    pub lines: Vec<usize>,
}
impl Chunk {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            instructions: Vec::new(),
            lines: Vec::new(),
        }
    }
    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
    pub fn write_op(&mut self, bytecode: Bytecode, line: usize) {
        self.instructions.push(bytecode);
        self.lines.push(line);
    }

    pub fn write_constant(&mut self, value: Value, line: usize) {
        let pos = self.add_constant(value);
        if pos >= 1 << 24 {
            eprintln!("Too many constants");
        } else if pos > u8::MAX as usize {
            self.write_op(Opcode::ConstantLong as u8, line);
            // Little endian is standard for modern processor architectures
            pos.to_le_bytes()
                .iter()
                .take(3)
                .for_each(|byte| self.write_op(*byte, line))
        } else {
            self.write_op(Opcode::Constant as u8, line);
            self.write_op(pos as u8, line);
        }
    }
}
