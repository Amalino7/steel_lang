use crate::vm::byte_utils::{byte_to_opcode, read_24_bytes, read_bytes};
use crate::vm::bytecode::{Chunk, Opcode};
use crate::vm::disassembler::disassemble_instruction;
use crate::vm::stack::Stack;
use crate::vm::value::Value;
use std::cmp::PartialOrd;
use std::process::exit;

mod byte_utils;
pub mod bytecode;
pub mod disassembler;
mod stack;
pub mod value;

const STACK_MAX: usize = 256;
pub struct VM {
    pub chunk: Chunk,
    ip: usize,
    stack: Stack<STACK_MAX>,
    globals: Vec<Value>,
}

impl VM {
    pub fn new(global_count: usize) -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Stack::<STACK_MAX>::new(),
            globals: vec![Value::Nil; global_count],
        }
    }

    pub fn run(&mut self) -> Value {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            disassemble_instruction(
                self.chunk.instructions.as_ref(),
                self.ip,
                self.chunk.constants.as_ref(),
                self.chunk.lines.as_ref(),
            );
            #[cfg(feature = "debug_stack_execution")]
            {
                println!("Stack: {:?}", self.stack);
            }
            let opcode = self.read_byte();
            match opcode {
                Opcode::Constant => {
                    let index = self.chunk.instructions[self.ip] as usize;
                    self.ip += 1;
                    let val = self.chunk.constants[index].clone();
                    self.stack.push(val);
                }
                Opcode::Return => {
                    let val = self.stack.pop();
                    println!("Returning {}", val);
                    return val;
                }
                Opcode::ConstantLong => {
                    let index = read_24_bytes(&self.chunk.instructions[self.ip..self.ip + 3]);
                    self.ip += 3;
                    let val = self.chunk.constants[index].clone();
                    self.stack.push(val);
                }
                Opcode::Negate => {
                    let val = self.stack.pop();
                    self.stack.push(-val);
                }
                Opcode::Halt => {
                    println!("Halting");
                    exit(-1);
                }
                Opcode::Subtract => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a - b);
                }
                Opcode::Add => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let res = a + b;
                    self.stack.push(res);
                }
                Opcode::Divide => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a / b);
                }
                Opcode::Multiply => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a * b);
                }
                Opcode::Not => {
                    let val = self.stack.pop();
                    self.stack.push(!val);
                }
                Opcode::Equal => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(Value::Boolean(a == b));
                }
                Opcode::Greater => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(Value::Boolean(a > b));
                }
                Opcode::Less => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(Value::Boolean(a < b))
                }
                Opcode::Pop => {
                    println!("Popping {}", self.stack.pop());
                    // self.stack.pop();
                }
                Opcode::JumpIfFalse => {
                    let offset = read_bytes(&self.chunk.instructions[self.ip..self.ip + 2]);
                    self.ip += 2;
                    let cond = self.stack.get_mut();
                    if *cond == Value::Boolean(false) {
                        self.ip += offset;
                    }
                }
                Opcode::Jump => {
                    let offset = read_bytes(&self.chunk.instructions[self.ip..self.ip + 2]);
                    self.ip += 2;
                    self.ip += offset;
                }

                Opcode::JumpBack => {
                    let offset = read_bytes(&self.chunk.instructions[self.ip..self.ip + 2]);
                    self.ip += 2;
                    self.ip -= offset;
                }

                Opcode::SetLocal => {
                    let index = self.chunk.instructions[self.ip] as usize;
                    self.ip += 1;

                    let val = self.stack.get_mut().clone();
                    self.stack.set_at(index, val);
                }
                Opcode::GetLocal => {
                    let index = self.chunk.instructions[self.ip] as usize;
                    self.ip += 1;
                    let val = self.stack.get_at(index);
                    self.stack.push(val);
                }
                Opcode::SetGlobal => {
                    let index = self.chunk.instructions[self.ip] as usize;
                    self.ip += 1;
                    self.globals[index] = self.stack.get_mut().clone();
                }
                Opcode::GetGlobal => {
                    let val = self.globals[self.chunk.instructions[self.ip] as usize].clone();
                    self.ip += 1;
                    self.stack.push(val);
                }
                Opcode::Call => {
                    todo!("Calls are not yet done")
                }
            }
        }
    }

    fn read_byte(&mut self) -> Opcode {
        self.ip += 1;
        byte_to_opcode(self.chunk.instructions[self.ip - 1])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Compiler;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::typechecker::TypeChecker;
    use crate::vm::disassembler::disassemble_chunk;
    use crate::vm::value::Value;
    #[test]
    fn test_simple_add() {
        let mut vm = VM::new(0);
        vm.chunk.write_constant(Value::Number(1.0), 1);
        vm.chunk.write_constant(Value::Number(2.0), 2);
        vm.chunk.write_op(Opcode::Add as u8, 3);
        vm.chunk.write_op(Opcode::Return as u8, 4);
        assert_eq!(vm.run(), Value::Number(3.0));
    }
    #[test]
    fn test_complex_arithmetic() {
        let mut vm = VM::new(0);
        vm.chunk.write_constant(Value::Number(6.9), 1);
        vm.chunk.write_constant(Value::Number(4.0), 2);
        vm.chunk.write_constant(Value::Number(3.0), 3);
        vm.chunk.write_constant(Value::Number(2.0), 4);
        vm.chunk.write_constant(Value::Number(1.0), 4);

        // 6.9 / (4 - 3 * (2 + (-1)))) = 6.9
        vm.chunk.write_op(Opcode::Negate as u8, 5);
        vm.chunk.write_op(Opcode::Add as u8, 5);
        vm.chunk.write_op(Opcode::Multiply as u8, 6);
        vm.chunk.write_op(Opcode::Subtract as u8, 7);
        vm.chunk.write_op(Opcode::Divide as u8, 8);
        vm.chunk.write_op(Opcode::Return as u8, 9);

        assert_eq!(vm.run(), Value::Number(6.9));
    }
    #[test]
    fn test_constant_long() {
        let mut vm = VM::new(0);
        vm.chunk.write_constant(Value::Number(0.0), 1);
        for i in 1..300 {
            vm.chunk.write_constant(Value::Number(i as f64), 1);
            vm.chunk.write_op(Opcode::Add as u8, 1);
        }
        vm.chunk.write_op(Opcode::Return as u8, 1);
        assert_eq!(vm.run(), Value::Number(44850.0)); // (299 * 300) / 2
    }
    #[test]
    fn test_boolean() {
        let mut vm = VM::new(0);
        vm.chunk.write_constant(Value::Boolean(true), 1);
        vm.chunk.write_op(Opcode::Return as u8, 1);
        assert_eq!(vm.run(), Value::Boolean(true));
    }

    #[test]
    fn test_expressions() {
        let src = "let a = 7 + 3 * 2 == 1;";
        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        let analysis = typecheker.check(&mut ast).expect("Failed to typecheck");
        let compiler = Compiler::new(analysis);
        let chunk = compiler.compile(&ast);

        let mut vm = VM::new(analysis.global_count);
        vm.chunk = chunk;
        vm.run();
        assert_eq!(vm.globals[0], Value::Boolean(false));
    }

    #[test]
    fn test_cmp() {
        let src = "let a = 7 >= 1;";
        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        let analysis = typecheker.check(&mut ast).expect("Failed to typecheck");
        let mut compiler = Compiler::new(analysis);
        let chunk = compiler.compile(&ast);
        let mut vm = VM::new(analysis.global_count);
        vm.chunk = chunk;
        vm.run();
        assert_eq!(vm.globals[0], Value::Boolean(true));
    }
    #[test]
    fn test_complex() {
        let src = "let a = 1;
        while a < 10 {
            a = a + 1;
        }
        ";

        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        let analysis = typecheker.check(&mut ast).expect("Failed to typecheck");
        let compiler = Compiler::new(analysis);
        let chunk = compiler.compile(&ast);

        disassemble_chunk(&chunk, "test");
        let mut vm = VM::new(analysis.global_count);

        vm.chunk = chunk;
        vm.run();
        assert_eq!(vm.globals[0], Value::Number(10.0));
    }

    #[test]
    fn test_local_variables() {
        let src = "
            let a = 0;
        {
            let a = 1;
            {
                let b = 2;
                {
                    let c = 3;
                    {
                        let d = 4;
                        a + b + c + d;
                    }
                    let e = 5;
                }
            }
            let f = 6;
            {
                let g = 7;
                a + f + g;
            }
        }
        ";

        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        let analysis = typecheker.check(&mut ast).expect("Failed to typecheck");
        let compiler = Compiler::new(analysis);
        let chunk = compiler.compile(&ast);

        disassemble_chunk(&chunk, "test");
        let mut vm = VM::new(analysis.global_count);

        vm.chunk = chunk;
        let val = vm.run();
    }
}
