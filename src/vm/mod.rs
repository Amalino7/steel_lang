use crate::stdlib::NativeDef;
use crate::vm::byte_utils::{byte_to_opcode, read_16_bytes, read_24_bytes};
use crate::vm::bytecode::Opcode;
use crate::vm::gc::{GarbageCollector, Gc};
use crate::vm::stack::Stack;
use crate::vm::value::{Closure, Function, Value};
use std::process::exit;

mod byte_utils;
pub mod bytecode;
pub mod disassembler;
pub mod gc;
mod stack;
pub mod value;

struct CallFrame {
    slot_offset: usize,
    ip: usize,
    function: Gc<Function>,
}

const STACK_MAX: usize = 256 * 64;

pub struct VM {
    frames: Vec<CallFrame>,
    gc: GarbageCollector,
    stack: Stack<STACK_MAX>,
    globals: Vec<Value>,
}

impl VM {
    pub fn new(global_count: usize, garbage_collector: GarbageCollector) -> Self {
        VM {
            frames: Vec::with_capacity(64),
            gc: garbage_collector,
            stack: Stack::<STACK_MAX>::new(),
            globals: vec![Value::Nil; global_count],
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
                        return val;
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
                Opcode::JumpIfFalse => {
                    let offset =
                        read_16_bytes(&chunk.instructions[current_frame.ip..current_frame.ip + 2]);
                    current_frame.ip += 2;
                    let cond = self.stack.get_top();
                    if cond == Value::Boolean(false) {
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

                            // Push the closure's upvalues onto the stack
                            for capture in closure.captures.iter() {
                                self.stack.push(*capture);
                            }

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
                            let result = native_fn(&args);

                            self.stack.truncate(top - arg_count - 1);

                            self.stack.push(result);
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
                        _ => panic!("Expected raw function on stack"),
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
            }
        }
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

    fn mark_roots(&mut self, current_frame: &CallFrame) {
        for global in &self.globals {
            self.gc.mark_value(global);
        }
        for i in 0..self.stack.top {
            self.gc.mark_value(&self.stack.buffer[i]);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute_source;
    use crate::vm::bytecode::Chunk;
    #[test]
    fn test_simple_add() {
        let mut vm = VM::new(0, GarbageCollector::new());
        let mut function = Function::new("Main".to_string(), Chunk::new());
        function.chunk.write_constant(Value::Number(1.0), 1);
        function.chunk.write_constant(Value::Number(2.0), 2);
        function.chunk.write_op(Opcode::Add as u8, 3);
        function.chunk.write_op(Opcode::Return as u8, 4);

        assert_eq!(vm.run(function), Value::Number(3.0));
    }
    #[test]
    fn test_complex_arithmetic() {
        let mut vm = VM::new(0, GarbageCollector::new());

        let mut function = Function::new("Main".to_string(), Chunk::new());
        function.chunk.write_constant(Value::Number(6.9), 1);
        function.chunk.write_constant(Value::Number(4.0), 2);
        function.chunk.write_constant(Value::Number(3.0), 3);
        function.chunk.write_constant(Value::Number(2.0), 4);
        function.chunk.write_constant(Value::Number(1.0), 4);

        // 6.9 / (4 - 3 * (2 + (-1)))) = 6.9
        function.chunk.write_op(Opcode::Negate as u8, 5);
        function.chunk.write_op(Opcode::Add as u8, 5);
        function.chunk.write_op(Opcode::Multiply as u8, 6);
        function.chunk.write_op(Opcode::Subtract as u8, 7);
        function.chunk.write_op(Opcode::Divide as u8, 8);
        function.chunk.write_op(Opcode::Return as u8, 9);

        assert_eq!(vm.run(function), Value::Number(6.9));
    }
    #[test]
    fn test_constant_long() {
        let mut vm = VM::new(0, GarbageCollector::new());
        let mut function = Function::new("Main".to_string(), Chunk::new());
        function.chunk.write_constant(Value::Number(0.0), 1);
        for i in 1..300 {
            function.chunk.write_constant(Value::Number(i as f64), 1);
            function.chunk.write_op(Opcode::Add as u8, 1);
        }
        function.chunk.write_op(Opcode::Return as u8, 1);
        assert_eq!(vm.run(function), Value::Number(44850.0)); // (299 * 300) / 2
    }
    #[test]
    fn test_boolean() {
        let mut vm = VM::new(0, GarbageCollector::new());
        let mut function = Function::new("Main".to_string(), Chunk::new());
        function.chunk.write_constant(Value::Boolean(true), 1);
        function.chunk.write_op(Opcode::Return as u8, 1);
        assert_eq!(vm.run(function), Value::Boolean(true));
    }

    #[test]
    // fn test_expressions() {
    //     let src = "let a = 7 + 3 * 2 == 1;";
    //     let scanner = Scanner::new(src);
    //     let mut parser = Parser::new(scanner);
    //     let mut typecheker = TypeChecker::new();
    //     let mut ast = parser.parse().expect("Failed to parse");
    //     let analysis = typecheker.check(&mut ast).expect("Failed to typecheck");
    //
    //     let mut gc = GarbageCollector::new();
    //     let compiler = Compiler::new(analysis, "main".to_string(), &mut gc);
    //     let function = compiler.compile(&ast);
    //
    //     let mut vm = VM::new(analysis.global_count, gc);
    //     vm.run(function);
    //     assert_eq!(vm.globals[0], Value::Boolean(false));
    // }
    //
    // #[test]
    // fn test_cmp() {
    //     let src = "let a = 7 >= 1;";
    //     let scanner = Scanner::new(src);
    //     let mut parser = Parser::new(scanner);
    //     let mut typecheker = TypeChecker::new();
    //     let mut ast = parser.parse().expect("Failed to parse");
    //     let analysis = typecheker.check(&mut ast).expect("Failed to typecheck");
    //
    //     let mut gc = GarbageCollector::new();
    //     let compiler = Compiler::new(analysis, "main".to_string(), &mut gc);
    //     let function = compiler.compile(&ast);
    //
    //     let mut vm = VM::new(analysis.global_count, gc);
    //     vm.run(function);
    //     assert_eq!(vm.globals[0], Value::Boolean(true));
    // }
    // #[test]
    // fn test_while_loop() {
    //     let src = "let a = 1;
    //     while a < 10 {
    //         a = a + 1;
    //     }
    //     ";
    //
    //     let scanner = Scanner::new(src);
    //     let mut parser = Parser::new(scanner);
    //     let mut typecheker = TypeChecker::new();
    //     let mut ast = parser.parse().expect("Failed to parse");
    //     let analysis = typecheker.check(&mut ast).expect("Failed to typecheck");
    //
    //     let mut gc = GarbageCollector::new();
    //     let compiler = Compiler::new(analysis, "main".to_string(), &mut gc);
    //     let function = compiler.compile(&ast);
    //
    //     let mut vm = VM::new(analysis.global_count, gc);
    //
    //     vm.run(function);
    //     assert_eq!(vm.globals[0], Value::Number(10.0));
    // }
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

        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_functions() {
        let src = "
            let a = 0;
            func add2(): number{
                a = a + 1;
                if a <= 5 {
                    return add() + 2;
                } else {
                    return 0;
                }
            }
            func add(): number{
                a = a + 1;
                if a <= 5 {
                    return add2() + 2;
                } else {
                    return 0;
                }
            }
            let new_a = add();
            a = 0;
            let b = add2();
            new_a;
            b;
        ";

        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_fib() {
        let src = "
            let a = 0;
            func fib(n: number): number {
                if n == 1 or n == 2 {
                    return 1;
                }
                return fib(n - 1) + fib(n-2);
            }
            a = fib(20);

            assert(a, 6765);
        ";

        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_short_circuit() {
        let src = "
            let a = 0;
            if a != 0 and (10 / a > 1) {
                // ...
            }
        ";

        execute_source(src, false, "run", true);
    }
    #[test]
    fn torture_test() {
        let src = r#"let g_counter = 0;
        // 2. Function with shadowing and recursion
        func complex(n: number): number {
            let g_counter = "string shadow"; // Shadow global with different type

            if n <= 0 {
                return 0;
            }

            // 3. Logical operator precedence
            if n > 5 and n < 10 or n == 20 {
                return n;
            }

            return 1 + complex(n - 1);
        }

        // 4. Block scoping torture
        {
            let x = 10;
            {
                let x = true; // Shadow
                {
                    let x = "deep"; // Shadow again
                }
                // x is boolean here
                if x {
                    // do nothing
                }
            }
            // x is number here
            x = x + 1;
        }

        // 5. String concatenation edge cases
        let empty = "";
        let combined = empty + "start" + empty + "end";
        print(combined);
         "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_assignment_operators() {
        let src = r#"
            let a = 2;
            a = a += 1;
            assert(a, 3);

            a -= 2;

            assert(a, 1);
            a *= 3;
            a /= 2;

            assert(a, 1.5);

            a *= a -= a += 2;
            assert(a, -3);
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_local_functions() {
        let src = r#"
            func main(): void {

                func local_func(a: number): number {
                    return a + 1;
                }
                let res = local_func(1);
                assert(res, 2);
                print(res);
            }
            main();
        "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_shadowing() {
        let src = r#"
            let a = 2;
            {
                let a = a + 5;
                print(a);
                assert(a, 7);
            }
            let a = 1;
            assert(a, 1);
        "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_function_recursion() {
        let src = r#"
            {
                func fib(n: number): number {
                    if n <= 1 {
                        return n;
                    }
                    return fib(n - 1) + fib(n - 2);
                }
                fib(20);
            }
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_gc() {
        let src = r#"
            let a = 0;
            let str = "";
            while a < 100 {
                str+="a";
                a+=1;
            }
            print(str);
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_higher_order_functions() {
        let src = r#"
        func foo(a: number, b: func():string): func(number): number {
            print(b());
            func bar(c: number): number {
                return 10 + c;
            }
            return bar;
        }
        func str(): string { return "hello";}

        let res = foo(10, str);
        let sum = res(5) + 10;
        print(sum);
        assert(sum, 25);
        assert(res(10), 20);
        "#;

        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_closure() {
        let src = r#"
        func foo(a: number): func(number): number {
            func bar(c: number): number {
                return c + a;
            }
            return bar;
        }
        let res = foo(10);
        let res2 = foo(21);
        assert(res(5), 15);
        assert(res2(10), 31);
        print(res(5));
        print(res2(10));

        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_closure_capture() {
        let src = r#"
        {
            let i = 5;
            while i < 10 {
                func foo(): number {
                    return i;
                }
                i+=1;
                print(foo());
                assert(foo(), i - 1);
            }
        }
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_local_override() {
        let src = r#"{
            let a = 1;
            {
                let b = 2;
            }
            let c = 100;
            assert(a + c, 101);
            print(a + c);
        }
        "#;
        execute_source(src, true, "run", true);
    }

    #[test]
    fn string_concatenation() {
        let src = r#"
            let str = "Hello";
            let i = 1;
            while i < 1000 {
                i += 1;
                str += "a";
            }
            // 2,368305 -> 1,7654
            // for 1000000: 22s
            print(str);
            "#;
        execute_source(src, false, "run", true);
    }
}
