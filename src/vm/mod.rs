use crate::stdlib::NativeDef;
use crate::vm::byte_utils::byte_to_opcode;
use crate::vm::bytecode::Opcode;
use crate::vm::gc::{GarbageCollector, Gc};
use crate::vm::stack::Stack;
use crate::vm::value::{BoundMethod, Closure, Function, Instance, InterfaceObj, VTable, Value};
use std::ptr;

mod byte_utils;
pub mod bytecode;
pub mod disassembler;
pub mod gc;
mod stack;
mod tests;
pub mod value;

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    ip: *const u8,
    fp: *mut Value,
    constants: *const Value,
    function: Gc<Function>,
}

const STACK_MAX: usize = 256 * 64;
const FRAMES_MAX: usize = 64;

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
            frames: Vec::with_capacity(FRAMES_MAX),
            gc: garbage_collector,
            stack: Stack::<STACK_MAX>::new(),
            globals: vec![Value::Nil; global_count],
            vtables: Vec::new(),
        }
    }

    pub fn set_native_functions(&mut self, natives: Vec<NativeDef>) {
        self.globals
            .resize(natives.len() + self.globals.len(), Value::Nil);
        for i in 0..natives.len() {
            self.globals[i] = Value::NativeFunction(natives[i].func);
        }
    }

    pub fn run(&mut self, main_function: Function) -> Value {
        unsafe {
            // 1. Setup execution roots
            let main_func = self.gc.alloc(main_function);
            let stack_base = self.stack.buffer.as_mut_ptr();

            // 2. Initialize "Registers"
            // ip: Instruction Pointer
            let mut ip = main_func.chunk.instructions.as_ptr();
            // sp: Stack Pointer (points to next free slot)
            let mut sp = stack_base;
            // fp: Frame Pointer (points to local slot 0)
            let mut fp = stack_base;
            // const_base: Direct pointer to constants array for speed
            let mut const_base = main_func.chunk.constants.as_ptr();
            // active_function: Kept in register for GC and access
            let mut active_function = main_func;

            // 3. Initialize Frame Stack Pointers
            // Ensure frames vector has space without reallocating during run
            if self.frames.capacity() < FRAMES_MAX {
                self.frames.reserve(FRAMES_MAX - self.frames.len());
            }
            let frames_base = self.frames.as_mut_ptr();
            let mut frame_sp = frames_base;

            loop {
                // [Diagnostic] Trace execution
                #[cfg(feature = "debug_trace_execution")]
                {
                    let offset =
                        ip.offset_from(active_function.chunk.instructions.as_ptr()) as usize;
                    disassemble_instruction(
                        active_function.chunk.instructions.as_ref(),
                        offset,
                        active_function.chunk.constants.as_ref(),
                        active_function.chunk.lines.as_ref(),
                    );
                }

                let opcode = byte_to_opcode(*ip);
                ip = ip.add(1);

                match opcode {
                    Opcode::Constant => {
                        let index = *ip as usize;
                        ip = ip.add(1);
                        *sp = *const_base.add(index);
                        sp = sp.add(1);
                    }
                    Opcode::ConstantLong => {
                        let b0 = *ip as usize;
                        let b1 = *ip.add(1) as usize;
                        let b2 = *ip.add(2) as usize;
                        let index = b0 | (b1 << 8) | (b2 << 16);
                        ip = ip.add(3);
                        *sp = *const_base.add(index);
                        sp = sp.add(1);
                    }
                    Opcode::Pop => {
                        sp = sp.sub(1);
                    }
                    Opcode::GetLocal => {
                        let index = *ip as usize;
                        ip = ip.add(1);
                        *sp = *fp.add(index);
                        sp = sp.add(1);
                    }
                    Opcode::SetLocal => {
                        let index = *ip as usize;
                        ip = ip.add(1);
                        let val = *sp.sub(1);
                        *fp.add(index) = val;
                    }
                    Opcode::GetGlobal => {
                        let index = *ip as usize;
                        ip = ip.add(1);
                        *sp = *self.globals.get_unchecked(index);
                        sp = sp.add(1);
                    }
                    Opcode::SetGlobal => {
                        let index = *ip as usize;
                        ip = ip.add(1);
                        *self.globals.get_unchecked_mut(index) = *sp.sub(1);
                    }
                    Opcode::Call => {
                        let arg_count = *ip as usize;
                        ip = ip.add(1);
                        let func_ptr = sp.sub(1 + arg_count);
                        let func_val = *func_ptr;

                        match func_val {
                            Value::Function(func) => {
                                *frame_sp = CallFrame {
                                    ip,
                                    fp,
                                    constants: const_base,
                                    function: active_function,
                                };
                                frame_sp = frame_sp.add(1);

                                active_function = func;
                                ip = func.chunk.instructions.as_ptr();
                                const_base = func.chunk.constants.as_ptr();
                                fp = func_ptr;
                            }
                            Value::Closure(closure) => {
                                *frame_sp = CallFrame {
                                    ip,
                                    fp,
                                    constants: const_base,
                                    function: active_function,
                                };
                                frame_sp = frame_sp.add(1);

                                active_function = closure.function;
                                ip = active_function.chunk.instructions.as_ptr();
                                const_base = active_function.chunk.constants.as_ptr();
                                fp = func_ptr;
                            }
                            Value::BoundMethod(bound) => {
                                *func_ptr = bound.receiver;

                                *frame_sp = CallFrame {
                                    ip,
                                    fp,
                                    constants: const_base,
                                    function: active_function,
                                };
                                frame_sp = frame_sp.add(1);

                                active_function = bound.method;
                                ip = active_function.chunk.instructions.as_ptr();
                                const_base = active_function.chunk.constants.as_ptr();
                                fp = func_ptr;
                            }
                            Value::NativeFunction(native_fn) => {
                                let args_start = func_ptr.add(1);
                                let args = std::slice::from_raw_parts(args_start, arg_count);

                                self.stack.top = sp.offset_from(stack_base) as usize;

                                let result = native_fn(args);
                                sp = func_ptr;
                                *sp = result;
                                sp = sp.add(1);
                            }
                            v => {
                                panic!("Expected function value, got {:?}", v);
                            }
                        }
                    }

                    Opcode::Return => {
                        sp = sp.sub(1);
                        let result = *sp;

                        if frame_sp == frames_base {
                            return result;
                        }

                        frame_sp = frame_sp.sub(1);
                        let frame = *frame_sp;
                        let return_slot = fp;

                        ip = frame.ip;
                        fp = frame.fp;
                        const_base = frame.constants;
                        active_function = frame.function;

                        sp = return_slot;
                        *sp = result;
                        sp = sp.add(1);
                    }

                    Opcode::Add => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        *sp.sub(1) = a + b;
                    }
                    Opcode::Subtract => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        *sp.sub(1) = a - b;
                    }
                    Opcode::Multiply => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        *sp.sub(1) = a * b;
                    }
                    Opcode::Divide => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        *sp.sub(1) = a / b;
                    }
                    Opcode::Negate => {
                        let val = *sp.sub(1);
                        *sp.sub(1) = -val;
                    }
                    Opcode::Equal => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        *sp.sub(1) = Value::Boolean(a == b);
                    }
                    Opcode::JumpIfFalse => {
                        let offset = ptr::read_unaligned(ip as *const u16).to_be();
                        ip = ip.add(2);
                        if *sp.sub(1) == Value::Boolean(false) {
                            ip = ip.add(offset as usize);
                        }
                    }
                    Opcode::Jump => {
                        let offset = ptr::read_unaligned(ip as *const u16).to_be();
                        ip = ip.add(2 + offset as usize);
                    }
                    Opcode::JumpBack => {
                        let offset = ptr::read_unaligned(ip as *const u16).to_be();
                        ip = ip.add(2).sub(offset as usize);
                    }
                    Opcode::MakeClosure => {
                        let upvalue_count = *ip as usize;
                        ip = ip.add(1);

                        // Capture upvalues
                        let mut upvalues = Vec::with_capacity(upvalue_count);
                        for _ in 0..upvalue_count {
                            sp = sp.sub(1);
                            upvalues.push(*sp);
                        }

                        sp = sp.sub(1);
                        let func_val = *sp;
                        let function = match func_val {
                            Value::Function(f) => f,
                            _ => panic!("Expected raw function"),
                        };

                        self.sync_state(sp, stack_base, frame_sp, frames_base);
                        let closure = self.alloc_closure(function, upvalues, active_function);

                        *sp = closure;
                        sp = sp.add(1);
                    }
                    Opcode::GetCapture => {
                        let index = *ip as usize;
                        ip = ip.add(1);

                        let closure_val = *fp;
                        if let Value::Closure(closure) = closure_val {
                            let capture = closure.captures.get_unchecked(index);
                            *sp = (*capture).clone();
                            sp = sp.add(1);
                        } else {
                            unreachable!("Expected closure on stack");
                        }
                    }
                    Opcode::EqualString => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        if let (Value::String(sa), Value::String(sb)) = (a, b) {
                            *sp.sub(1) = Value::Boolean(sa.as_str() == sb.as_str());
                        } else {
                            unreachable!("Can only compare strings");
                        }
                    }
                    Opcode::EqualNumber => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        if let (Value::Number(na), Value::Number(nb)) = (a, b) {
                            *sp.sub(1) = Value::Boolean(na == nb);
                        } else {
                            unreachable!("Can only compare numbers");
                        }
                    }
                    Opcode::StructAlloc => {
                        let count = *ip as usize;
                        ip = ip.add(1);

                        let mut fields = Vec::with_capacity(count);
                        for _ in 0..count {
                            sp = sp.sub(1);
                            fields.push(*sp);
                        }

                        sp = sp.sub(1);
                        let name = *sp;

                        self.stack.top = sp.offset_from(stack_base) as usize;
                        let instance =
                            self.alloc_struct(Instance { name, fields }, active_function);

                        *sp = instance;
                        sp = sp.add(1);
                    }

                    Opcode::GetField => {
                        let index = *ip as usize;
                        ip = ip.add(1);

                        sp = sp.sub(1);
                        let instance_val = *sp;

                        if let Value::Instance(instance) = instance_val {
                            let field = instance.fields.get_unchecked(index).clone();
                            *sp = field;
                            sp = sp.add(1);
                        } else {
                            panic!("GetField on non-instance");
                        }
                    }
                    Opcode::SetField => {
                        let index = *ip as usize;
                        ip = ip.add(1);

                        sp = sp.sub(1);
                        let instance_val = *sp;
                        sp = sp.sub(1);
                        let value = *sp;

                        if let Value::Instance(mut instance) = instance_val {
                            // DerefMut for Gc might be safe or unsafe depending on impl
                            // Assuming Gc<T> implies safe deref usually, but minimizing checks:
                            (*instance.deref_mut()).fields[index] = value;
                            *sp = value;
                            sp = sp.add(1);
                        } else {
                            panic!("SetField on non-instance");
                        }
                    }
                    Opcode::BindMethod => {
                        sp = sp.sub(1);
                        let function = *sp;
                        sp = sp.sub(1);
                        let receiver = *sp;

                        if let Value::Function(method) = function {
                            self.stack.top = sp.offset_from(stack_base) as usize;
                            let bound = self.gc.alloc(BoundMethod { receiver, method });

                            *sp = Value::BoundMethod(bound);
                            sp = sp.add(1);
                        } else {
                            panic!("BindMethod expected function");
                        }
                    }
                    Opcode::MakeVTable => {
                        let count = *ip as usize;
                        ip = ip.add(1);

                        let mut methods = Vec::with_capacity(count);
                        for _ in 0..count {
                            sp = sp.sub(1);
                            let v = *sp;
                            match v {
                                Value::Function(f) => methods.push(f),
                                Value::Closure(c) => methods.push(c.function),
                                _ => panic!("MakeVTable expects function/closure values"),
                            }
                        }

                        self.stack.top = sp.offset_from(stack_base) as usize;
                        let vt = self.gc.alloc(VTable { methods });
                        self.vtables.push(vt);
                    }
                    Opcode::MakeInterfaceObj => {
                        let idx = *ip as usize;
                        ip = ip.add(1);

                        sp = sp.sub(1);
                        let data = *sp;

                        self.stack.top = sp.offset_from(stack_base) as usize;
                        let obj = self.gc.alloc(InterfaceObj {
                            data,
                            vtable: *self.vtables.get_unchecked(idx),
                        });
                        *sp = Value::InterfaceObj(obj);
                        sp = sp.add(1);
                    }
                    Opcode::GetInterfaceMethod => {
                        let idx = *ip as usize;
                        ip = ip.add(1);

                        sp = sp.sub(1);
                        let obj_val = *sp;

                        if let Value::InterfaceObj(obj) = obj_val {
                            let method = *obj.vtable.methods.get_unchecked(idx);
                            *sp = Value::Function(method);
                            sp = sp.add(1);
                            *sp = obj.data;
                            sp = sp.add(1);
                        } else {
                            unreachable!("GetInterfaceMethod expected interface object");
                        }
                    }
                    Opcode::InterfaceBindMethod => {
                        let idx = *ip as usize;
                        ip = ip.add(1);

                        sp = sp.sub(1);
                        let obj_val = *sp;

                        if let Value::InterfaceObj(obj) = obj_val {
                            let method = *obj.vtable.methods.get_unchecked(idx);

                            self.stack.top = sp.offset_from(stack_base) as usize;
                            let bound = self.gc.alloc(BoundMethod {
                                receiver: obj.data,
                                method,
                            });
                            *sp = Value::BoundMethod(bound);
                            sp = sp.add(1);
                        } else {
                            unreachable!("InterfaceBindMethod expected interface object");
                        }
                    }
                    Opcode::Not => {
                        let val = *sp.sub(1);
                        *sp.sub(1) = !val;
                    }
                    Opcode::GreaterNumber => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        if let (Value::Number(n_a), Value::Number(n_b)) = (a, b) {
                            *sp.sub(1) = Value::Boolean(n_a > n_b);
                        } else {
                            unreachable!("Can only compare numbers");
                        }
                    }
                    Opcode::LessNumber => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        if let (Value::Number(n_a), Value::Number(n_b)) = (a, b) {
                            *sp.sub(1) = Value::Boolean(n_a < n_b);
                        } else {
                            unreachable!("Can only compare numbers");
                        }
                    }
                    Opcode::GreaterString => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        if let (Value::String(s_a), Value::String(s_b)) = (a, b) {
                            *sp.sub(1) = Value::Boolean(s_a.as_str() > s_b.as_str());
                        } else {
                            unreachable!("Can only compare strings");
                        }
                    }
                    Opcode::LessString => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);
                        if let (Value::String(s_a), Value::String(s_b)) = (a, b) {
                            *sp.sub(1) = Value::Boolean(s_a.as_str() < s_b.as_str());
                        } else {
                            unreachable!("Can only compare strings");
                        }
                    }
                    Opcode::Halt => {
                        panic!("VM halted");
                    }
                    Opcode::Concat => {
                        sp = sp.sub(1);
                        let b = *sp;
                        let a = *sp.sub(1);

                        match (&a, &b) {
                            (Value::String(sa), Value::String(sb)) => {
                                let mut str = String::with_capacity(sa.len() + sb.len());
                                str.push_str(sa.as_str());
                                str.push_str(sb.as_str());

                                self.stack.top = sp.offset_from(stack_base) as usize - 1;
                                let val = self.alloc_string(str, active_function);

                                *sp.sub(1) = val;
                            }
                            _ => unreachable!("Can only concatenate strings"),
                        }
                    }
                }
            }
        }
    }

    // Helper to sync raw pointers back to struct for GC
    unsafe fn sync_state(
        &mut self,
        sp: *mut Value,
        stack_base: *mut Value,
        frame_sp: *mut CallFrame,
        frames_base: *mut CallFrame,
    ) {
        self.stack.top = sp.offset_from(stack_base) as usize;
        let frame_count = frame_sp.offset_from(frames_base) as usize;
        self.frames.set_len(frame_count);
    }

    fn alloc_closure(
        &mut self,
        function: Gc<Function>,
        captures: Vec<Value>,
        active_function: Gc<Function>,
    ) -> Value {
        let closure = self.gc.alloc(Closure { function, captures });
        if self.gc.should_collect() {
            self.gc.mark(closure);
            self.mark_roots(active_function);
            self.gc.collect();
        }
        Value::Closure(closure)
    }

    fn mark_roots(&mut self, active_function: Gc<Function>) {
        for vtable in &self.vtables {
            self.gc.mark(*vtable);
        }
        for global in &self.globals {
            self.gc.mark_value(global);
        }
        for i in 0..self.stack.top {
            self.gc.mark_value(&self.stack.buffer[i]);
        }

        // Mark the currently running function
        self.gc.mark(active_function);

        // Mark functions in the stack frames
        for frame in &self.frames {
            self.gc.mark(frame.function);
        }
    }

    fn alloc_struct(&mut self, instance: Instance, active_function: Gc<Function>) -> Value {
        let instance = self.gc.alloc(instance);
        if self.gc.should_collect() {
            self.gc.mark(instance);
            self.mark_roots(active_function);
            self.gc.collect();
        }
        Value::Instance(instance)
    }

    fn alloc_string(&mut self, s: String, active_function: Gc<Function>) -> Value {
        if self.gc.should_collect() {
            self.mark_roots(active_function);
            self.gc.collect();
        }
        let str_ref = self.gc.alloc(s);
        Value::String(str_ref)
    }
}
