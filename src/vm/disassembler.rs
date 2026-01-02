use crate::vm::byte_utils::{byte_to_opcode, read_bytes};
use crate::vm::bytecode::{Bytecode, Chunk, Opcode};
use crate::vm::value::Value;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    let bytecode = &chunk.instructions;
    println!("==== {} ====", name);
    let mut offset = 0;
    while offset < bytecode.len() {
        offset = disassemble_instruction(bytecode, offset, &chunk.constants, &chunk.lines);
    }
    println!("==== {} end ====", name);
}

pub fn disassemble_instruction(
    bytecode: &[Bytecode],
    mut offset: usize,
    constants: &[Value],
    lines: &[usize],
) -> usize {
    print!("{:0>4} ", offset);

    if offset > 0 && lines[offset] == lines[offset - 1] {
        print!("   | ")
    } else {
        print!("{:>4} ", lines[offset])
    }
    match byte_to_opcode(bytecode[offset]) {
        Opcode::Constant => {
            offset += 1;
            print!("Constant");
            let index = bytecode[offset];
            let val = constants[index as usize];
            println!(" {}", val);
            if let Value::Function(func) = val {
                disassemble_chunk(&func.chunk, func.name.as_str());
            }
        }
        Opcode::ConstantLong => {
            offset += 3;
            print!("Constant Long");
            let arr: &[Bytecode] = &bytecode[(offset - 2)..=offset];
            let index = read_bytes(arr);
            let val = constants[index];
            println!(" {}", val);
        }
        Opcode::Return => println!("Return"),
        Opcode::Halt => println!("Halt"),
        Opcode::Negate => println!("Negate"),
        Opcode::Subtract => println!("Subtract"),
        Opcode::Add => println!("Add"),
        Opcode::Divide => println!("Divide"),
        Opcode::Multiply => println!("Multiply"),
        Opcode::Not => println!("Not"),
        Opcode::Concat => println!("Concat"),
        Opcode::Equal => {
            println!("Equal")
        }
        Opcode::GreaterString => {
            println!("GreaterString")
        }
        Opcode::LessString => {
            println!("LessString")
        }
        Opcode::GreaterNumber => {
            println!("GreaterNumber")
        }
        Opcode::LessNumber => {
            println!("LessNumber")
        }
        Opcode::Pop => {
            println!("Pop")
        }
        Opcode::JumpIfFalse => {
            offset += 2;
            let jump_offset = read_bytes(&bytecode[(offset - 1)..=offset]);
            println!("JumpIfFalse {} ", jump_offset)
        }
        Opcode::Jump => {
            offset += 2;
            let jump_offset = read_bytes(&bytecode[(offset - 1)..=offset]);
            println!("Jump {} ", jump_offset)
        }
        Opcode::JumpIfNil => {
            offset += 2;
            let jump_offset = read_bytes(&bytecode[(offset - 1)..=offset]);
            println!("Jump if nil {} ", jump_offset)
        }
        Opcode::JumpIfNotNil => {
            offset += 2;
            let jump_offset = read_bytes(&bytecode[(offset - 1)..=offset]);
            println!("Jump if not nil {} ", jump_offset)
        }
        Opcode::JumpBack => {
            offset += 2;
            let jump_offset = read_bytes(&bytecode[(offset - 1)..=offset]);
            println!("Jump -{} ", jump_offset)
        }
        Opcode::SetLocal => {
            offset += 1;
            println!("SetLocal {}", bytecode[offset]);
        }
        Opcode::GetLocal => {
            offset += 1;
            println!("GetLocal {}", bytecode[offset]);
        }
        Opcode::SetGlobal => {
            offset += 1;
            println!("SetGlobal {}", bytecode[offset]);
        }
        Opcode::GetGlobal => {
            offset += 1;
            println!("GetGlobal {}", bytecode[offset]);
        }
        Opcode::Call => {
            offset += 1;
            println!("Call {}", bytecode[offset]);
        }
        Opcode::MakeClosure => {
            offset += 1;
            println!("MakeClosure {}", bytecode[offset]);
        }
        Opcode::GetCapture => {
            offset += 1;
            println!("GetCapture {}", bytecode[offset]);
        }
        Opcode::StructAlloc => {
            offset += 1;
            println!("StructAlloc {}", bytecode[offset]);
        }
        Opcode::GetField => {
            offset += 1;
            println!("GetField {}", bytecode[offset]);
        }
        Opcode::SetField => {
            offset += 1;
            println!("SetField {}", bytecode[offset]);
        }
        Opcode::EqualString => println!("EqualString"),
        Opcode::EqualNumber => println!("EqualNumber"),
        Opcode::BindMethod => println!("BindMethod"),

        Opcode::MakeVTable => {
            offset += 1;
            println!("MakeVTable {}", bytecode[offset]);
        }
        Opcode::MakeInterfaceObj => {
            offset += 1;
            println!("MakeInterfaceObj {}", bytecode[offset]);
        }
        Opcode::InterfaceBindMethod => {
            offset += 1;
            println!("InterfaceBindMethod {}", bytecode[offset]);
        }
        Opcode::GetInterfaceMethod => {
            offset += 1;
            println!("GetInterfaceMethod {}", bytecode[offset]);
        }
        Opcode::EnumAlloc => {
            offset += 1;
            println!("EnumAlloc tag{}", bytecode[offset]);
        }
        Opcode::DestructureEnum => {
            println!("DestructureEnum");
        }
        Opcode::CheckEnumTag => {
            offset += 1;
            println!("CheckEnumTag {}", bytecode[offset]);
        }
        Opcode::Nil => println!("Nil"),
        Opcode::Unwrap => println!("Unwrap"),
        Opcode::Reserve => {
            offset += 1;
            println!("Reserve {}", bytecode[offset])
        }
        Opcode::Dup => println!("Dup"),
    };
    offset + 1
}
