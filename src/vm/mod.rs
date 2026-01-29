use crate::stdlib::NativeDef;
use crate::vm::byte_utils::{byte_to_opcode, read_16_bytes, read_24_bytes};
use crate::vm::bytecode::Opcode;
use crate::vm::error::RuntimeError;
use crate::vm::gc::{GarbageCollector, Gc};
use crate::vm::stack::Stack;
use crate::vm::value::{
    BoundMethod, Closure, EnumVariant, Function, Instance, InterfaceObj, List, VTable, Value,
};

mod byte_utils;
pub mod bytecode;
pub mod disassembler;
mod error;
pub mod gc;
mod stack;
#[cfg(test)]
mod tests;
pub mod value;

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    slot_offset: usize,
    ip: usize,
    function: Gc<Function>,
}

const STACK_MAX: usize = 256 * 128;

pub struct VM {
    vtables: Vec<Gc<VTable>>,
    frames: Vec<CallFrame>,
    gc: GarbageCollector,
    stack: Stack<STACK_MAX>,
    globals: Vec<Value>,
}

impl VM {
    pub fn new(global_count: usize, garbage_collector: GarbageCollector) -> Self {
        VM {
            frames: Vec::with_capacity(128),
            gc: garbage_collector,
            stack: Stack::<STACK_MAX>::new(),
            globals: vec![Value::Nil; global_count],
            vtables: Vec::new(),
        }
    }

    fn make_error(&self, message: &str) -> RuntimeError {
        let mut stack_trace = Vec::new();
        for frame in self.frames.iter().rev() {
            let line = if frame.ip > 0 {
                frame.function.chunk.lines[frame.ip - 1]
            } else {
                0
            };
            stack_trace.push(format!("{} (line {})", frame.function.name, line));
        }
        RuntimeError {
            message: message.to_string(),
            stack_trace,
        }
    }

    pub fn set_native_functions(&mut self, natives: Vec<NativeDef>) {
        self.globals
            .resize(natives.len() + self.globals.len(), Value::Nil);
        for (i, def) in natives.iter().enumerate() {
            self.globals[i] = Value::NativeFunction(def.func);
        }
    }

    pub fn run(&mut self, main_function: Function) -> Result<Value, RuntimeError> {
        self.frames.push(CallFrame {
            slot_offset: 0,
            ip: 0,
            function: self.gc.alloc(main_function),
        });

        let mut current_frame = self.frames.pop().expect("No active frame");
        let mut chunk = &current_frame.function.chunk;

        loop {
            #[cfg(feature = "debug_trace_execution")]
            disassemble_instruction(
                chunk.instructions.as_ref(),
                self.ip,
                chunk.constants.as_ref(),
                chunk.lines.as_ref(),
            );
            #[cfg(feature = "debug_stack_execution")]
            {
                println!("Stack: {:?}", self.stack);
            }
            let opcode = byte_to_opcode(chunk.instructions[current_frame.ip]);
            current_frame.ip += 1;

            match opcode {
                Opcode::Constant => {
                    let index = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    let val = chunk.constants[index];
                    self.stack.push(val);
                }
                Opcode::Return => {
                    let val = self.stack.pop();
                    if self.frames.is_empty() {
                        return Ok(val);
                    }
                    self.stack.truncate(current_frame.slot_offset);
                    let _frame = self.frames.pop().expect("Stack underflow");
                    current_frame = _frame;
                    chunk = &current_frame.function.chunk; // update chunk
                    self.stack.push(val)
                }
                Opcode::ConstantLong => {
                    let index =
                        read_24_bytes(&chunk.instructions[current_frame.ip..current_frame.ip + 3]);
                    current_frame.ip += 3;
                    let val = chunk.constants[index];
                    self.stack.push(val);
                }
                Opcode::Negate => {
                    let val = self.stack.pop();
                    self.stack.push(-val);
                }
                Opcode::Halt => {
                    self.frames.push(current_frame);
                    return Err(self.make_error("Halt instruction encountered"));
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
                Opcode::Concat => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::String(a), Value::String(b)) => {
                            let mut str = String::with_capacity(a.len() + b.len());
                            str.push_str(a.as_str());
                            str.push_str(b.as_str());
                            let val = self.alloc_string(str, &current_frame);
                            self.stack.push(val);
                        }
                        _ => unreachable!("Can only concatenate strings"),
                    }
                }
                Opcode::Divide => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            if b == 0.0 {
                                self.frames.push(current_frame);
                                return Err(self.make_error("Division by zero"));
                            }
                            self.stack.push(Value::Number(a / b));
                        }
                        _ => {
                            self.frames.push(current_frame);
                            return Err(self.make_error("Operands must be numbers"));
                        }
                    }
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
                Opcode::Unwrap => {
                    let val = self.stack.get_top();
                    if let Value::Nil = val {
                        self.frames.push(current_frame);
                        return Err(self.make_error("Unwrap on nil value"));
                    }
                }
                Opcode::Equal => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(Value::Boolean(a == b));
                }
                Opcode::GreaterNumber => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Boolean(a > b))
                        }
                        _ => unreachable!("Can only compare numbers"),
                    }
                }
                Opcode::LessNumber => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Boolean(a < b))
                        }
                        _ => unreachable!("Can only compare numbers"),
                    }
                }
                Opcode::GreaterString => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::String(a), Value::String(b)) => {
                            self.stack.push(Value::Boolean(a.as_str() > b.as_str()))
                        }
                        _ => unreachable!("Can only compare strings"),
                    }
                }
                Opcode::LessString => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::String(a), Value::String(b)) => {
                            self.stack.push(Value::Boolean(a.as_str() < b.as_str()))
                        }
                        _ => unreachable!("Can only compare strings"),
                    }
                }
                Opcode::Pop => {
                    self.stack.pop();
                }
                Opcode::Dup => {
                    let val = self.stack.get_top();
                    self.stack.push(val);
                }
                Opcode::Reserve => {
                    let amount = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    self.stack.top += amount;
                }
                Opcode::Nil => {
                    self.stack.push(Value::Nil);
                }
                Opcode::JumpIfFalse => {
                    let offset =
                        read_16_bytes(&chunk.instructions[current_frame.ip..current_frame.ip + 2]);
                    current_frame.ip += 2;
                    let cond = self.stack.get_top();
                    if cond == Value::Boolean(false) {
                        current_frame.ip += offset;
                    }
                }
                Opcode::JumpIfNil => {
                    let offset =
                        read_16_bytes(&chunk.instructions[current_frame.ip..current_frame.ip + 2]);
                    current_frame.ip += 2;
                    let val = self.stack.get_top();
                    if val == Value::Nil {
                        current_frame.ip += offset;
                    }
                }
                Opcode::JumpIfNotNil => {
                    let offset =
                        read_16_bytes(&chunk.instructions[current_frame.ip..current_frame.ip + 2]);
                    current_frame.ip += 2;
                    let val = self.stack.get_top();
                    if val != Value::Nil {
                        current_frame.ip += offset;
                    }
                }
                Opcode::Jump => {
                    let offset =
                        read_16_bytes(&chunk.instructions[current_frame.ip..current_frame.ip + 2]);
                    current_frame.ip += 2;
                    current_frame.ip += offset;
                }

                Opcode::JumpBack => {
                    let offset =
                        read_16_bytes(&chunk.instructions[current_frame.ip..current_frame.ip + 2]);
                    current_frame.ip += 2;
                    current_frame.ip -= offset;
                }

                Opcode::SetLocal => {
                    let index = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let val = self.stack.get_top();
                    self.stack.set_at(current_frame.slot_offset + index, val);
                }
                Opcode::GetLocal => {
                    let index = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    let val = self.stack.get_at(current_frame.slot_offset + index);
                    self.stack.push(val);
                }
                Opcode::SetGlobal => {
                    let index = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    self.globals[index] = self.stack.get_top();
                }
                Opcode::GetGlobal => {
                    let val = self.globals[chunk.instructions[current_frame.ip] as usize];
                    current_frame.ip += 1;
                    self.stack.push(val);
                }
                Opcode::Call => {
                    let arg_count = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let top = self.stack.top;
                    let func = self.stack.get_at(top - arg_count - 1);
                    match func {
                        Value::Function(func) => {
                            let new_slot_offset = top - arg_count - 1;
                            let frame = CallFrame {
                                function: func,
                                ip: 0,
                                slot_offset: new_slot_offset,
                            };
                            self.frames.push(current_frame);
                            current_frame = frame;
                            chunk = &current_frame.function.chunk; // updated chunk
                        }
                        Value::Closure(closure) => {
                            let new_slot_offset = top - arg_count - 1;
                            let frame = CallFrame {
                                function: closure.function,
                                ip: 0,
                                slot_offset: new_slot_offset,
                            };

                            self.frames.push(current_frame);
                            current_frame = frame;
                            chunk = &current_frame.function.chunk; // updated chunk
                        }
                        Value::BoundMethod(bound_method) => {
                            let callee_index = top - arg_count - 1;
                            for i in (callee_index + 1..top).rev() {
                                let v = self.stack.get_at(i);
                                self.stack.set_at(i + 1, v);
                            }
                            self.stack.set_at(callee_index + 1, bound_method.receiver);
                            self.stack.top = top + 1;

                            let frame = CallFrame {
                                function: bound_method.method,
                                ip: 0,
                                slot_offset: callee_index,
                            };

                            self.frames.push(current_frame);
                            current_frame = frame;
                            chunk = &current_frame.function.chunk; // updated chunk
                        }
                        Value::NativeFunction(native_fn) => {
                            let args_start = top - arg_count;
                            let mut args = Vec::with_capacity(arg_count);
                            for i in 0..arg_count {
                                args.push(self.stack.get_at(args_start + i));
                            }
                            match native_fn(&args, &mut self.gc) {
                                Ok(result) => {
                                    self.stack.truncate(top - arg_count - 1);
                                    self.stack.push(result);
                                }
                                Err(msg) => {
                                    self.frames.push(current_frame);
                                    return Err(self.make_error(&msg));
                                }
                            }
                        }
                        val => unreachable!("Only functions should be called found {}", val),
                    }
                }
                Opcode::MakeClosure => {
                    let upvalue_count = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    // Collect the captured values from the stack
                    let mut upvalues = Vec::with_capacity(upvalue_count);
                    for _ in 0..upvalue_count {
                        upvalues.push(self.stack.pop());
                    }

                    let func_val = self.stack.pop();
                    let function = match func_val {
                        Value::Function(f) => f,
                        _ => unreachable!("Expected raw function on stack"),
                    };

                    let closure = self.alloc_closure(function, upvalues, &current_frame);
                    self.stack.push(closure);
                }

                Opcode::GetCapture => {
                    let index = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let closure = self.stack.get_at(current_frame.slot_offset);
                    match closure {
                        Value::Closure(closure) => {
                            let capture = closure.captures[index];
                            self.stack.push(capture);
                        }
                        _ => unreachable!("Expected closure on stack"),
                    }
                }
                Opcode::EqualString => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::String(a), Value::String(b)) => {
                            self.stack.push(Value::Boolean(a.as_str() == b.as_str()))
                        }
                        _ => unreachable!("Can only compare strings"),
                    }
                }
                Opcode::EqualNumber => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Boolean(a == b))
                        }
                        _ => unreachable!("Can only compare numbers"),
                    }
                }
                Opcode::StructAlloc => {
                    let count = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let mut fields = Vec::with_capacity(count);
                    for _ in 0..count {
                        fields.push(self.stack.pop());
                    }
                    let name = self.stack.pop();

                    let instance = self.alloc_struct(Instance { name, fields }, &current_frame);
                    self.stack.push(instance);
                }
                Opcode::EnumAlloc => {
                    let tag = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    let payload = self.stack.pop();

                    let name = self.stack.pop();
                    let enum_variant = self.alloc_enum(
                        EnumVariant {
                            tag,
                            payload,
                            enum_name: name,
                        },
                        &current_frame,
                    );
                    self.stack.push(enum_variant);
                }
                Opcode::DestructureEnum => {
                    let enumeration = self.stack.pop();
                    match enumeration {
                        Value::Enum(enum_obj) => {
                            self.stack.push(enum_obj.payload);
                        }
                        _ => unreachable!("Expected enum on stack"),
                    }
                }
                Opcode::CheckEnumTag => {
                    let enumeration = self.stack.pop();
                    let tag_id = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    match enumeration {
                        Value::Enum(enum_obj) => {
                            self.stack.push(Value::Boolean(enum_obj.tag == tag_id));
                        }
                        _ => unreachable!("Expected enum on stack"),
                    }
                }

                Opcode::GetField => {
                    let index = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let instance_val = self.stack.pop();
                    if let Value::Instance(instance) = instance_val {
                        self.stack.push(instance.fields[index]);
                    } else {
                        unreachable!("GetField on non-instance");
                    }
                }
                Opcode::SetField => unsafe {
                    let index = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let instance_val = self.stack.pop();
                    let value = self.stack.pop();
                    if let Value::Instance(mut instance) = instance_val {
                        instance.deref_mut().fields[index] = value;
                        self.stack.push(value);
                    } else {
                        unreachable!("SetField on non-instance");
                    }
                },
                Opcode::GetIndex => {
                    let Value::Number(index) = self.stack.pop() else {
                        unreachable!("GetIndex expected number");
                    };

                    let list = self.stack.pop();
                    let Value::List(list) = list else {
                        unreachable!("GetIndex on non-list");
                    };

                    match list.vec.get(index as usize) {
                        Some(&v) => {
                            self.stack.push(v);
                        }
                        None => {
                            let error_msg =
                                format!("Index {} out of bounds. Len: {}", index, list.vec.len());
                            return Err(self.make_error(error_msg.as_str()));
                        }
                    }
                }
                Opcode::SetIndex => unsafe {
                    let Value::Number(index) = self.stack.pop() else {
                        unreachable!("SetIndex expected number");
                    };
                    let index = index as usize;

                    let list = self.stack.pop();
                    let Value::List(mut list) = list else {
                        unreachable!("SetIndex on non-list");
                    };

                    let value = self.stack.pop();
                    if index >= list.vec.len() {
                        let error_msg =
                            format!("Index {} out of bounds. Len: {}", index, list.vec.len());
                        return Err(self.make_error(error_msg.as_str()));
                    }
                    list.deref_mut().vec[index] = value;
                    self.stack.push(value);
                },
                Opcode::MakeList => {
                    let count = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let mut elements = Vec::with_capacity(count);
                    for _ in 0..count {
                        elements.push(self.stack.pop());
                    }

                    let list = self.alloc_list(List { vec: elements }, &current_frame);
                    self.stack.push(list);
                }
                Opcode::BindMethod => {
                    let function = self.stack.pop();
                    let receiver = self.stack.pop();

                    if let Value::Function(method) = function {
                        let bound = self.gc.alloc(BoundMethod { receiver, method });
                        self.stack.push(Value::BoundMethod(bound));
                    } else {
                        unreachable!("BindMethod expected function");
                    }
                }
                Opcode::MakeVTable => {
                    let count = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let mut methods = Vec::with_capacity(count);
                    for _ in 0..count {
                        let v = self.stack.pop();
                        match v {
                            Value::Function(f) => methods.push(f),
                            Value::Closure(c) => methods.push(c.function),
                            _ => unreachable!("MakeVTable expects function/closure values"),
                        }
                    }

                    let vt = self.gc.alloc(VTable { methods });
                    self.vtables.push(vt);
                }

                Opcode::MakeInterfaceObj => {
                    let idx = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    let data = self.stack.pop();

                    let obj = self.gc.alloc(InterfaceObj {
                        data,
                        vtable: self.vtables[idx],
                    });
                    self.stack.push(Value::InterfaceObj(obj));
                }
                Opcode::GetInterfaceMethod => {
                    let idx = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;
                    let obj = self.stack.pop();
                    let Value::InterfaceObj(obj) = obj else {
                        unreachable!("GetInterfaceMethod expected interface object");
                    };
                    self.stack.push(Value::Function(obj.vtable.methods[idx]));
                    self.stack.push(obj.data);
                }
                Opcode::InterfaceBindMethod => {
                    let idx = chunk.instructions[current_frame.ip] as usize;
                    current_frame.ip += 1;

                    let obj = self.stack.pop();
                    let Value::InterfaceObj(obj) = obj else {
                        unreachable!("InterfaceBindMethod expected interface object");
                    };

                    let method = obj.vtable.methods[idx];
                    let bound = self.gc.alloc(BoundMethod {
                        receiver: obj.data,
                        method,
                    });
                    self.stack.push(Value::BoundMethod(bound));
                }
            }
        }
    }

    fn alloc_enum(&mut self, enumeration: EnumVariant, call_frame: &CallFrame) -> Value {
        let enumeration = self.gc.alloc(enumeration);
        if self.gc.should_collect() {
            self.gc.mark(enumeration);
            self.mark_roots(call_frame);
            self.gc.collect();
        }
        Value::Enum(enumeration)
    }

    fn alloc_struct(&mut self, instance: Instance, current_frame: &CallFrame) -> Value {
        let instance = self.gc.alloc(instance);
        if self.gc.should_collect() {
            self.gc.mark(instance);
            self.mark_roots(current_frame);
            self.gc.collect();
        }
        Value::Instance(instance)
    }

    fn alloc_closure(
        &mut self,
        function: Gc<Function>,
        captures: Vec<Value>,
        current_frame: &CallFrame,
    ) -> Value {
        let closure = self.gc.alloc(Closure { function, captures });
        if self.gc.should_collect() {
            self.gc.mark(closure);
            self.mark_roots(current_frame);
            self.gc.collect();
        }
        Value::Closure(closure)
    }

    fn alloc_list(&mut self, list: List, current_frame: &CallFrame) -> Value {
        let list = self.gc.alloc(list);
        if self.gc.should_collect() {
            self.gc.mark(list);
            self.mark_roots(current_frame);
            self.gc.collect();
        }
        Value::List(list)
    }

    fn mark_roots(&mut self, current_frame: &CallFrame) {
        for vtable in &self.vtables {
            self.gc.mark(*vtable);
        }
        for &global in &self.globals {
            self.gc.mark_value(global);
        }
        for i in 0..self.stack.top {
            self.gc.mark_value(self.stack.buffer[i]);
        }

        self.gc.mark(current_frame.function);
        for frame in &self.frames {
            self.gc.mark(frame.function);
        }
    }

    fn alloc_string(&mut self, s: String, current_frame: &CallFrame) -> Value {
        if self.gc.should_collect() {
            self.mark_roots(current_frame);
            self.gc.collect();
        }
        let str_ref = self.gc.alloc(s);
        Value::String(str_ref)
    }
}
