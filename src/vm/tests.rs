#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::execute_source;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::typechecker::TypeChecker;
    use crate::vm::bytecode::{Chunk, Opcode};
    use crate::vm::gc::GarbageCollector;
    use crate::vm::value::{Function, Value};
    use crate::vm::VM;

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
    fn test_expressions() {
        let src = "let a = 7 + 3 * 2 == 1;";
        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        let typed_ast = typecheker.check(&mut ast).expect("Failed to typecheck");

        let mut gc = GarbageCollector::new();
        let compiler = Compiler::new("main".to_string(), &mut gc);
        let function = compiler.compile(&typed_ast);

        let mut vm = VM::new(1, gc);
        vm.run(function);
        assert_eq!(vm.globals[0], Value::Boolean(false));
    }

    #[test]
    fn test_cmp() {
        let src = "let a = 7 >= 1;";
        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        let typed_ast = typecheker.check(&mut ast).expect("Failed to typecheck");

        let mut gc = GarbageCollector::new();
        let compiler = Compiler::new("main".to_string(), &mut gc);
        let function = compiler.compile(&typed_ast);

        let mut vm = VM::new(1, gc);
        vm.run(function);
        assert_eq!(vm.globals[0], Value::Boolean(true));
    }
    #[test]
    fn test_while_loop() {
        let src = "let a = 1;
        while a < 10 {
            a = a + 1;
        }
        ";

        let scanner = Scanner::new(src);
        let mut parser = Parser::new(scanner);
        let mut typecheker = TypeChecker::new();
        let mut ast = parser.parse().expect("Failed to parse");
        let typed_ast = typecheker.check(&mut ast).expect("Failed to typecheck");

        let mut gc = GarbageCollector::new();
        let compiler = Compiler::new("main".to_string(), &mut gc);
        let function = compiler.compile(&typed_ast);

        let mut vm = VM::new(1, gc);

        vm.run(function);
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
            g_counter+=1;
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
        print(complex(10));
        assert(g_counter, 2);

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
        assert(combined, "startend");
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
                    return i + i;
                }
                i+=1;
                print(foo());
                assert(foo(), i + i - 2);
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
        execute_source(src, false, "run", true);
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

    #[test]
    fn test_forward_declaration() {
        let src = r#"
            assert(add(1,2), 3);
            print(add(1,2));
            func add(a: number, b: number): number {
                return a + b;
            }
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_complex_closure() {
        let src = r#"
            func outer(): func() {
              let x = "outside";
              func inner() {
                print(x);
              }
              
              return inner;
            }

            let closure = outer();
            closure();
            "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_complex_closure_2() {
        let src = r#"
            func outer(a: number): func(number): number {
              let x = a;
              func inner(b: number): number {
                return x + b;
              }

              return inner;
            }
            let closure = outer(10);
            assert(closure(5), 15);
            print(closure(5));
            "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_complex_closure_3() {
        let src = r#"
            func outer(a: number):number {
                let x = a;
                func spole() {
                    print(x);
                }
                let y = 10;
                return x + y;
            }
            print(outer(8));
              "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_complex_higher_order_closure() {
        let src = r#"
            func add(a: number, b: number): number {
                return a + b;
            }
            func sub(a: number, b: number): number {
                return a - b;
            }
            func mul(a: number, b: number): number {
                return a * b;
            }

            func outer(op: func(number, number): number): func(number): func(number): number {
              func inner(arg1: number): func(number): number {
                let a = arg1;
                func inner_inner(b: number): number {
                    return op(a, b);
                }
                return inner_inner;
              }
              return inner;
            }

             let adder = outer(add);
             let subtractor = outer(sub);
             let multiplier = outer(mul);
             print(adder(3)(4));
             assert(adder(3)(4), 7);

             print(subtractor(3)(4));
             assert(subtractor(3)(4), -1);

             print(multiplier(3)(4));
             assert(multiplier(3)(4), 12);
             "#;

        execute_source(src, true, "run", true);
    }
    #[test]
    fn test_local_recursion() {
        let src = r#"
        func main(): void {
            func fib(n: number): number {
                if n <= 1 {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
            print(fib(20));
            assert(fib(20), 6765);
        }
        main();
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_mutually_recursive_functions() {
        let src = r#"
            let a = "something";
            func fib(n: number): number {
                if n <= 1 {
                    return n;
                }
                return fib2(n - 1) + fib2(n - 2);
            }
            func fib2(n: number): number {
                if n <= 1 {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
            assert(fib(10), 55);
            assert(fib2(10), 55);
            assert(a, "something");
            "#;

        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_structs() {
        let src = r#"
            struct Point {
                x: number,
                y: number,
            }
            let p1 = Point{x: 1, y: 2};
            assert(p1.x, 1);
            assert(p1.y, 2);

            func make_point(x: number, y: number): Point {
                return Point { x: x, y: y };
            }

            func print_point(p: Point): void {
                println("{", p.x,",", p.y,"}");
            }

            let p2 = make_point(10, 20);
            println(p2.x); // Prints 10
            println(p2.x + p2.y);
            assert(p2.x + p2.y, 30);
            p2.x = 100;
            print_point(p2);
            assert(p2.x, 100);
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_struct_function_fields() {
        let src = r#"
            struct Point {
                x: number,
                y: number,
                add:func(number, number): number,
            }

            func add(a: number, b: number): number {
                 return a + b;
            }

            let p1 = Point {x: 1, y: 2, add: add};
            assert(p1.x, 1);
            assert(p1.y, 2);
            assert(p1.add(6,7), 13);
            println(p1.add(6,7));
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_complex_structs() {
        let src = r#"
            struct Point {
                x: number,
                y: number,
            }
            struct Rectangle {
                width: number,
                height: number,
                corner: Point,
            }
            let rect = Rectangle{width: 10, height: 20, corner: Point{x: 1, y: 2}};
            assert(rect.corner.x, 1);
            assert(rect.corner.y, 2);
            assert(rect.width, 10);
            assert(rect.height, 20);
            rect.corner.x = 100;
            rect.corner.y = 200;
            assert(rect.corner.x, 100);
            assert(rect.corner.y, 200);
            println(rect.corner.x," ", rect.corner.y," ", rect.width," ", rect.height);
            "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_recursive_structs() {
        let src = r#"
            struct Node {
                value: number,
                next: Node,
            }
            // let head = Node{value: 1, next: null};
            // let tail = Node{value: 2, next: head};
            // head.next = tail;
            // assert(head.next.value, 2);
            "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_different_types_in_struct() {
        let src = r#"
            struct Point {
                x: number,
                y: string,
            }
            let p1 = Point{x: 1, y: "2"};
            assert(p1.x, 1);
            assert(p1.y, "2");
            println(p1.x," ", p1.y);
            "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_initialization_order() {
        let src = r#"
            struct Vec5 {
                x: number,
                y: number,
                z: number,
                w: number,
                v: number,
            }
            let v = Vec5{v: 1, w: 2, x: 3, y: 4, z: 5};
            assert(v.v, 1);
            assert(v.w, 2);
            assert(v.x, 3);
            assert(v.y, 4);
            assert(v.z, 5);
            let v1 = Vec5{w: 2, v: 1,z: 1 + 4, y: 4, x: 3};
            assert(v1.v, 1);
            assert(v1.w, 2);
            assert(v1.x, 3);
            assert(v1.y, 4);
            assert(v1.z, 5);
        "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_struct_methods() {
        let src = r#"
            struct Point {
                x: number,
                y: number,
            }
            impl Point {
                func add(self, other: Point): Point {
                    return Point{x: self.x + other.x, y: self.y + other.y};
                }
            }
            impl number {
                func abs(self): number {
                    if self < 0 {
                        return -self;
                    }
                    return self;
                }
            }

            impl void {
                func lmao(self): void {
                    println("Someone called a method on nothing!!");
                }
            }

            let p1 = Point{x: 1, y: 2};
            let p2 = Point{x: 3, y: 4};
            let p3 = p1.add(p2);
            assert(p3.x, 4);
            assert(p3.y, 6);
            println(p3.x," ", p3.y);
            println((-14).abs());
            assert(10.abs(), 10);
            "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_counter() {
        let src = r#"
            struct Counter {
                value: number,
            }
            impl Counter {
                func inc(self): Counter {
                    self.value +=1;
                    return self;
                }
                func new(): Counter {
                    return Counter{value: 0};
                }

                func add(stg: Counter, other: number) {
                    stg.value += other;
                }
            }

            let c = Counter.new();
            c.inc().inc().inc().inc();
            assert(c.value, 4);
            Counter.add(c, 10);
            println(c.value);
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_method_on_nothing() {
        let src = r#"
            impl void {
                func lmao(self): void {
                    println("Someone called a method on nothing!!");
                }
            }

            impl number {
                func static_0(other: string): number {
                    return 0;
                }
            }

            func ah(): void {}

            println(number.static_0(""));
            let a = ah();
            a.lmao();
            println(2 + 4).lmao().lmao().lmao().lmao().lmao();
        "#;
        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_different_method_calls() {
        let src = r#"
            struct Point {
                x: number,
                y: number,
            }
            impl Point {
                // normal method
                func add(self, other: Point): Point {
                    return Point{x: self.x + other.x, y: self.y + other.y};
                }
                // static method
                func add2(one: Point, two: Point): Point {
                    return one.add(two);
                }
            }
            let instance1 = Point{x: 1, y: 2};
            let instance2 = Point{x: 3, y: 4};
            let result = Point.add2(instance1, instance2); // static
            assert(result.x, 4); assert(result.y, 6);
            let result2 = instance1.add(instance2); // normal
            assert(result2.x, 4); assert(result2.y, 6);
            let result3 = Point.add(instance1, instance2); // UFCS
            assert(result3.x, 4); assert(result3.y, 6);
            // let result4 = instance1.add2(instance2); //illegal
            "#;

        execute_source(src, false, "run", true);
    }

    #[test]
    fn test_recursive_method() {
        let src = r#"
            struct Fiber {}
            impl Fiber {
                func fib(self, num: number): number{
                    if num <= 1 {
                        return num;
                    }
                    return self.fib(num - 1) + self.fib(num - 2);
                }
            }
            let res = Fiber{}.fib(20);
            let fib = Fiber{}.fib;
            assert(fib(20), 6765);
            assert(res, 6765);
            println(res);
            "#;
        execute_source(src, false, "run", true);
    }
    #[test]
    fn test_bound_method() {
        let src = r#"
            func _10xer(op: func()){
                let i = 0;
                while i < 10 {
                    op();
                    i+=1;
                }
            }

            struct Counter{val: number}
            impl Counter {
                func inc(self): void {
                    self.val += 1;
                }
            }
            let c = Counter{val: 0};
            c.inc();
            assert(c.val, 1);
            let inc = c.inc;
            inc();
            _10xer(inc);
            println(c.val);
            assert(c.val, 11);
            "#;
        execute_source(src, false, "run", true);
    }
}
