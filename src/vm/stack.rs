use crate::vm::value::Value;

#[derive(Debug)]
pub struct Stack<const SIZE: usize> {
    pub buffer: [Value; SIZE],
    pub top: usize,
}

impl<const SIZE: usize> Stack<SIZE> {
    pub(crate) fn new() -> Stack<SIZE> {
        Stack {
            top: 0,
            buffer: [const { Value::Nil }; SIZE],
        }
    }
}

impl<const SIZE: usize> Stack<SIZE> {
    pub fn pop(&mut self) -> Value {
        self.top -= 1;
        self.buffer[self.top]
    }
    pub fn set_at(&mut self, index: usize, value: Value) {
        self.buffer[index] = value;
    }
    pub fn get_at(&self, index: usize) -> Value {
        self.buffer[index]
    }
    pub fn push(&mut self, value: Value) {
        self.buffer[self.top] = value;
        self.top += 1;
    }
    #[allow(dead_code)]
    pub fn get_mut(&mut self) -> &mut Value {
        &mut self.buffer[self.top - 1]
    }

    pub fn get_top(&self) -> Value {
        self.buffer[self.top - 1]
    }

    pub fn truncate(&mut self, index: usize) {
        self.top = index;
    }
}
