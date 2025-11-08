use crate::vm::value::Value;
use std::mem::replace;

#[derive(Debug)]
pub struct Stack<const SIZE: usize> {
    buffer: [Value; SIZE],
    top: usize,
}

impl<const SIZE: usize> Stack<SIZE> {
    pub(crate) fn new() -> Stack<256> {
        Stack {
            top: 0,
            buffer: [const { Value::Nil }; 256],
        }
    }
}

impl<const SIZE: usize> Stack<SIZE> {
    pub fn pop(&mut self) -> Value {
        self.top -= 1;
        replace(&mut self.buffer[self.top], Value::Nil)
    }
    pub fn push(&mut self, value: Value) {
        self.buffer[self.top] = value;
        self.top += 1;
    }
    #[allow(dead_code)]
    pub fn get_mut(&mut self) -> &mut Value {
        &mut self.buffer[self.top - 1]
    }
}
