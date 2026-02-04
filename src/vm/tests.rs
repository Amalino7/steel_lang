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

    assert_eq!(vm.run(function).unwrap(), Value::Number(3.0));
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

    assert_eq!(vm.run(function).unwrap(), Value::Number(6.9));
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
    assert_eq!(vm.run(function).unwrap(), Value::Number(44850.0)); // (299 * 300) / 2
}
#[test]
fn test_boolean() {
    let mut vm = VM::new(0, GarbageCollector::new());
    let mut function = Function::new("Main".to_string(), Chunk::new());
    function.chunk.write_constant(Value::Boolean(true), 1);
    function.chunk.write_op(Opcode::Return as u8, 1);
    assert_eq!(vm.run(function).unwrap(), Value::Boolean(true));
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
    let function = compiler.compile(0, &typed_ast);

    let mut vm = VM::new(1, gc);
    vm.run(function).unwrap();
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
    let function = compiler.compile(0, &typed_ast);

    let mut vm = VM::new(1, gc);
    vm.run(function).unwrap();
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
    let function = compiler.compile(0, &typed_ast);

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
#[cfg_attr(miri, ignore)]
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
            a = 1;
            assert(a, 1);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
#[cfg_attr(miri, ignore)]
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
    let src = r#"
        {
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
#[cfg_attr(miri, ignore)]
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

    execute_source(src, false, "run", true);
}
#[test]
#[cfg_attr(miri, ignore)]
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
            let p1 = Point(1, 2);

            assert(p1.x, 1);
            assert(p1.y, 2);

            func make_point(x: number, y: number): Point {
                return Point (x: x, y: y);
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
            print_point(p1);
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

            let p1 = Point (x: 1, y: 2, add: add);
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
            let rect = Rectangle(width: 10, height: 20, corner: Point(x: 1, y: 2));
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
                next: Node?,
            }
            let head = Node(value: 1, next: nil);
            let tail = Node(value: 2, next: head);
            head.next = tail;
            assert(head.next?.value, 2);
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
            let p1 = Point(x: 1, y: "2");
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
            let v = Vec5(v: 1, w: 2, x: 3, y: 4, z: 5);
            assert(v.v, 1);
            assert(v.w, 2);
            assert(v.x, 3);
            assert(v.y, 4);
            assert(v.z, 5);
            let v1 = Vec5(w: 2, v: 1,z: 1 + 4, y: 4, x: 3);
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
                    return Point(x: self.x + other.x, y: self.y + other.y);
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


            let p1 = Point(x: 1, y: 2);
            let p2 = Point(x: 3, y: 4);
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
                    return Counter(0);
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
                    return Point(x: self.x + other.x, y: self.y + other.y);
                }
                // static method
                func add2(one: Point, two: Point): Point {
                    return one.add(two);
                }
            }
            let instance1 = Point(x: 1, y: 2);
            let instance2 = Point(3, y: 4);
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
#[cfg_attr(miri, ignore)]
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
            let res = Fiber().fib(20);
            let fib = Fiber().fib;
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
            let c = Counter(val: 0);
            c.inc();
            assert(c.val, 1);
            let inc = c.inc;
            inc();
            _10xer(inc);
            println(c.val);
            assert(c.val, 12);
            "#;
    execute_source(src, false, "run", true);
}

#[test]
fn test_interface() {
    let src = r#"
            interface Drawable {
                func draw(self): void;
            }

            struct Point { x: number, y: number }

            impl Point : Drawable {
              func draw(self): void {
                println("Drawing point at ", self.x, ", ", self.y);
              }
            }

            struct Other {}
            impl Other : Drawable {
                func draw(self): void {
                    println("Drawing other");
                }
            }

            func draw(d: Drawable): void { d.draw(); }

            let d: Drawable = Point ( x: 1, y: 2 );
            d.draw();
            let o: Drawable = Other();

            o.draw();
            draw(d);
            draw(o);
           "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_interface_autocast() {
    let src = r#"
            interface Drawable {
                func draw(self): void;
            }
            struct Point { x: number, y: number }
            impl Point : Drawable {
                func draw(self): void {
                    println(self.x," ", self.y);
                }
            }

            func something(a: Drawable): void { a.draw(); }

            let p: Drawable = Point ( x: 1, y: 2 );
            p = Point ( x: 3, y: 4 );
            p.draw();
            something(Point ( x: 5, y: 6 ));

            let other = Point ( x: 7, y: 8);
            other.draw();

            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_interface_on_primitive() {
    let src = r#"
            interface Drawable {
                func draw(self): void;
            }
            impl number : Drawable {
                func draw(self): void {
                    println(self);
                    assert(self, 10);
                }
            }
            let n: Drawable = 10;
            n.draw();
        "#;
    execute_source(src, false, "run", true);
}

#[test]
fn test_eq_interface() {
    let src = r#"
            interface Eq {
                func eq(self, other: Eq): boolean;
            }
            struct Point { x: number, y: number }
            impl Point : Eq {
                func eq(self, other: Eq): boolean { // No generics so it isn't useful
                    println(self.x, self.y);
                    return true;
                }
            }

            let p1 = Point ( x: 1, y: 2 );
            let p2 = Point ( x: 3, y: 4 );
            println(p1.eq(p2));
            assert(p1.eq(p2), true);

            func areEqual(a: Eq, b: Eq): boolean { return a.eq(b); }
            assert(areEqual(p1, p2), true);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_print_interface() {
    let src = r#"
            interface Printable {
                func print(self): void;
            }
            struct Point { x: number, y: number }
            impl Point : Printable {
                func print(self): void {
                    assert(self.x, 3); assert(self.y, 4);
                    println(self.x, self.y);
                }
            }

            impl number : Printable {
                func print(self): void {
                    assert(self, 10);
                    println(self);
                }
            }

            func myPrint(p: Printable): void { p.print(); }

            myPrint(Point ( x: 3, y: 4 ));
            myPrint(10);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_interface_with_many() {
    let src = r#"
            interface Shape {
                func area(self): number;
                func perimeter(self): number;
            }
            struct Circle { radius: number }
            impl Circle : Shape {
                func area(self): number { return 3.14 * self.radius * self.radius; }
                func perimeter(self): number { return 2 * 3.14 * self.radius; }
            }
            struct Rectangle { width: number, height: number }
            impl Rectangle : Shape {
                func area(self): number { return self.width * self.height; }
                func perimeter(self): number { return 2 * (self.width + self.height); }
            }
            func printArea(s: Shape): void {
                assert(s.area(), 78.5); // 5*5 * 3.14
                println(s.area());
            }
            func printPerimeter(s: Shape): void {
                assert(s.perimeter(), (10 + 5) * 2 );
                println(s.perimeter());
            }
            printArea(Circle ( radius: 5 ));
            printPerimeter(Rectangle ( width: 10, height: 5 ));

            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_safety() {
    let src = r#"
            let a: number? = nil;
            assert(a, nil);
            println(a);
            let b:number = a ?? 0;

            let c: boolean? = nil;
            println(b);
            assert(b, 0);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_access() {
    let src = r#"
            struct Point { x: number, y: number }
            let p: Point? = Point ( x: 1, y: 2 );
            assert(p?.x, 1);
            assert(p?.y, 2);
            p = nil;

            let num: number = p?.x ?? 0;

            assert(num, 0);
            assert(p?.x, nil);
            assert(p?.y, nil);
            println(p?.x);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_linked_list() {
    let src = r#"
            struct Node {
                value: number,
                next: Node?,
            }
            let head = Node(value: 1, next: nil);
            let tail = Node(value: 2, next: head);
            head.next = tail;
            assert(head.next?.value, 2);
            head.next?.value = 3;
            assert(head.next?.value, 3);
            head.next = nil;
            head.next?.value = 4;
            assert(head.next?.value, nil);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_unwrap() {
    let src = r#"
            struct Point { x: number, y: number }
            let p: Point? = Point ( x: 1, y: 2 );
            let strong_p = p!;
            p = nil;
            assert(strong_p.x, 1);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_complex_linked_list() {
    let src = r#"
            struct Node {
                value: number,
                next: Node?,
            }
            struct LinkedList {
                size: number,
                head: Node?,
            }
            impl LinkedList {
                func new(): LinkedList {
                    return LinkedList(size: 0, head: nil);
                }
                func get(self, index: number): number? {
                    let head = self.head;
                    while head != nil and index > 0 {
                        index -= 1;
                        head = head?.next;
                    }
                    return head?.value;
                }
                func set(self, index: number, value: number): void {
                    let head = self.head;
                    while head != nil and index > 0 {
                        index -= 1;
                        head = head?.next;
                    }
                    head?.value = value;
                }
                func push(self, value: number) {
                    self.size += 1;
                    let new_node = Node(value: value, next: nil);
                    if self.head != nil {
                        let head = self.head!;
                        while head.next != nil {
                            head = head.next!;
                        }
                        head.next = new_node;
                    }
                    else {
                        self.head = new_node;
                    }
                }
                func pop(self): number? {
                    if self.size == 0 {
                        return nil;
                    }
                    else if self.size == 1 {
                        let value = self.head!.value;
                        self.size -= 1;
                        self.head = nil;
                        return value;
                    } else {
                        self.size -= 1;
                        let head = self.head;
                        while head?.next?.next != nil {
                            head = head!.next;
                        }
                        let value = head?.next?.value;
                        head?.next = nil;
                        return value;
                    }
                }
                func get_size(self): number { return self.size; }
            }

            func print_list(list: LinkedList) {
                print("[");
                let i =0;
                while i < list.get_size() {
                    if i + 1 == list.get_size() {
                        print(list.get(i));
                    }
                    else {
                        print(list.get(i), ", ");
                    }
                    i+=1;
                }
                println("]");
            }

            let list = LinkedList.new();
            list.push(1);
            list.push(2);
            list.push(3);
            list.push(4);
            print_list(list);
            assert(list.pop(), 4);
            list.set(1, 5);
            assert(list.get(1), 5);
            print_list(list);
            assert(list.pop(), 3);
            assert(list.pop(), 5);
            print_list(list);
            assert(list.pop(), 1);

            print_list(list);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_functions_on_nil() {
    let src = r#"
            struct Point { x: number, y: number }
            impl Point {
                func do_something(self): void {
                    assert(1,2); // should never be called
                }

                func other(self): void {
                    println("ok");
                    assert(1,1);
                }
            }
            let p: Point? = nil;
            let i = 0;
            let fun = p?.do_something;
            fun?();

            p?.do_something();
            p = Point(1, 2);

            p?.other();
            fun?();
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_functions() {
    let src = r#"

            let fun: func?(): void = nil;
            if fun?() != nil {
                assert(1, 2);
            }

            func other(): void { println("Yellow");}
            fun = other;
            fun?();
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_interface() {
    let src = r#"
            interface Printable {
                func print(self): void;
            }
            impl number : Printable {
                func print(self): void { println(self); }
            }
            let p: Printable? = nil;
            p?.print();
            p = 10;
            p?.print();
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_refinements() {
    let src = r#"
            let num_called = 0;
            interface Printable {
                func print(self): void;
            }
            struct Point { x: number, y: number }
            impl Point : Printable {
                func print(self): void { num_called+=1;  println(self.x, self.y); }
            }
            func main()
            {
                let p: Printable? = Point(x: 1, y: 2);
                if p != nil {
                    // normal check
                    p.print();
                }

                if p == nil {
                    assert(1, 2);
                    return;
                } else {
                    // opposite check
                    p.print();
                }
                // from guard clause
                p.print();

            }
            main();
            assert(num_called, 3);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_refinement_2() {
    let src = r#"
            func main()
            {
                let x: number? = nil;
                x = 10;
                if x != nil {
                    // x is number here
                    println(x + 10);
                } else {
                    // x is nil here
                    println(x);
                    return;
                }
                // x is also number here
                println(x + 1);
            }
            main();
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_refinement_3() {
    let src = r#"
            func main()
            {
                let x: number? = nil;
                let y: number? = 10;

                if x != nil and true {
                    println("nil");
                } else {
                    println("not nil");
                }

                if  x == nil and x != nil {
                    // technically unreachable
                    println(x + 10);
                }
            }
            main();
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_nil_refinement_logical() {
    let src = r#"
            struct Array {length: number}
            func main()
            {
                let x: Array? = Array(length: 10);
                if x != nil and x.length > 0 {
                    assert(x.length, 10);
                    println(x.length);
                }

                x = nil;

                if x == nil or x.length == 0 {
                    println(x);
                    assert(x, nil);
                    return;
                }
            }
            main();
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_enums() {
    let src = r#"
            enum Color { Red, Green, Blue }
            let c = Color.Red;
            match c {
                Color.Red => {println("Red");}
                Color.Green => {println("Green");}
                Color.Blue => {println("Blue");}
            }
            assert(c, Color.Red);
            println(c);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_structure_enums() {
    let src = r#"
            enum Message {
             Quit,
             Move{
                x: number,
                y: number,
             },
             ChangeColor{
                r: number,
                g: number,
                b: number,
             }
            }
            let msg = Message.Move(x: 10, y: 20);
            // msg = Message.Quit;
            // msg = Message.ChangeColor(r: 10, g: 20, b: 30);
            match msg {
                .Quit => {println("Quit");}
                .Move(:x, :y) => {println(x, y);}
                .ChangeColor(a) => {println(a.r, a.g, a.b);}
            }
            msg = Message.ChangeColor(r: 10, g: 20, b: 30);
            msg = Message.Quit;

            match msg {
                .ChangeColor(a) => {println(a.r, a.g, a.b);}
                _ => {println("Not a change color");}
            }
           "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_val_enums() {
    let src = r#"
            enum Shape { Circle(number), Rectangle(number, number) }
            func area(s: Shape): number {
                match s {
                    Shape.Circle(radius) => {return 3.14 * radius * radius;}
                    Shape.Rectangle(width, height) => {return width * height;}
                }
            }
            func area2(s: Shape): number {
                match s {
                    .Circle(radius) => {return 3.14 * radius * radius;}
                    .Rectangle(rect) => {return rect.0 * rect.1;}
                }
            }
            let rect = Shape.Rectangle(6, 10);
            println(rect);
            println(area2(rect));
            assert(area(Shape.Circle(5)), 78.5);
            assert(area(Shape.Rectangle(10, 5)), 50);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_enum_branch_analysis() {
    let src = r#"
            enum Res {Ok(number), Err(string)}
            {
                let res = Res.Ok(10);
                let res2 = Res.Ok(20);
                // res = Res.Err("Hello");
                if res is Ok and res2 is Ok {
                    println(res + res2);
                } else {
                    // println("Error is "+ res);
                }

                if res2 is Ok and res2 > 10 {
                    println("res is bigger than 10: ", res2);
                }
            }

            func main() {
                let res = Res.Ok(15);
                if res is Err {
                    return;
                }
                println(res * 7);
            }
            main();
        "#;
    execute_source(src, true, "run", true);
}
#[test]
fn test_number_result() {
    let src = r#"
            enum Result { Ok(number), Err(string) }
            impl Result {
                func unwrap(self): number {
                    match self {
                        Result.Ok(val) => {return val;}
                        Result.Err(err) => {
                            println("Unwrapping error: ", err);
                            panic("");
                            return 0; // Ureachable
                        }
                    }
                }
                func unwrap_or(self, default: number): number {
                    match self{
                        Result.Ok(val) => {return val;}
                        Result.Err(_) => {return default;}
                    }
                }
            }
            let res = Result.Ok(10);
            let sum = res.unwrap() + 15;
            println(sum);
            assert(sum, 25);
            let res2 = Result.Err("Error");
            // res2.unwrap();
            assert(res2.unwrap_or(10), 10);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_func_named_args() {
    let src = r#"
            func print_msg(msg: string) {
                println("Here ->", msg);
            }
            func other(no: string) {
                println("No: ", no);
            }
            impl number {
                func add(self, other: number): number{
                    return self + other;
                }
            }
            println(10.add(other: 12));
            print_msg(msg: "Nice to meet you!");

            let a = print_msg;
            a(msg: "Ok");
            a = other;
            a(msg: "Not ok!");
        "#;
    execute_source(src, false, "run", true);
}

#[test]
fn test_inner_type() {
    let src = r#"
        struct Vector {
            start: Point,
            end: Point,
        }
        struct Point { x: number, y: number }
        impl Point {
            func print(self) {
                println("{",self.x ,",", self.y, "}");
            }
        }
        let vec = Vector(start: Point(x: 1, y: 2), end: Point(x: 3, y: 4));
        vec.start.print();
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_tuples() {
    let src = r#"
            func takes_str(a: string) {}
            func takes_tuple(a: (number, string)) {
                println(a);
                println(a.0);
                println(a.1);
            }
            func takes_num(a: number) {}
            let tuple = (1, "What");
            takes_tuple(tuple);
            takes_str(tuple.1);
            takes_num(tuple.0);
            assert(tuple.0, 1);
            assert(tuple.1, "What");
        "#;
    execute_source(src, false, "lex", true);
}
#[test]
fn test_tuples2() {
    let src = r#"
            func returns_tuple(): (number, number) { return (1, 2);}
            let tuple = returns_tuple();
            let (a,b) = returns_tuple();
            println(a," ", b);

            let tuple2: (number, number)? = (3, 4);
            println(tuple2?.0);
            println(tuple2?.1);
            assert(tuple2?.0, 3);
            assert(tuple2?.1, 4);

            tuple2?.1 = 2;
            assert(tuple2?.1, 2);
            assert(( 2 + 5) * 5, 35);
            tuple2 = nil;
            assert(tuple2?.0, nil);
            assert(tuple2?.1, nil);
            assert(tuple.0, 1);
            assert(tuple.1, 2);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_destructuring() {
    let src = r#"
            struct Point {x: number, y: number}

            let Point(y: x, x: y) = Point(x: 1, y: 2);
            assert(x, 2);
            assert(y, 1);
            struct Complex {one: (number, number, Other, Other), two: Other}
            struct Other{a: number, b: number}

            let Complex(one: (a,b, _, other), two: Other(a: c,b: d) )  = Complex(one: (1, 2, Other(3,4) , Other(5,6) ), two: Other(7,8));
            println(a,b,other.a,other.b, c, d);
            assert(1, a);
            assert(2, b);
            assert(other.a, 5);
            assert(other.b, 6);
            assert(c, 7);
            assert(d, 8);

        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_generic_struct() {
    let src = r#"
            struct Point<T> {x: T, y: T}
            let p = Point(x: 1, y: 2);
            let Point(:x, :y) = p;
            assert(x + 0, 1);
            assert(y + 0, 2);
            assert(p.x, 1);
            assert(p.y, 2);
            "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_generic_function() {
    let src = r#"
        func identity<T>(stg: T): T {
            return stg;
        }
        func first<T,U>(pair: (T,U)): T {
            return pair.0;
        }
        func second<T>(pair: (T,T)): T {
            return pair.1;
        }

        let num = identity(3);
        let pair = (1, "str");
        println(num + 2);

        let a = first(pair);
        let str = second((1, 2));
        // assert(str + "1", "str1");
        assert(a + 2, 3);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_simple_generic_struct() {
    let src = r#"
        struct Box<T> {top: T}

        func square(a: number): number{
            return a * a;
        }
        func unwrap(a: number?): number {
            return a!;
        }

        func map<T,U> (box: Box<T>, transform: func(T): U): Box<U> {
            let res = transform(box.top);
            return Box(top: res);
        }

        let box = Box(19);
        let other_box = Box("str");
        box = map(box, square);

        let new_box: Box<number?> = Box(nil);
        new_box.top = 10;
        let good = map(new_box, unwrap);

        // let a = Box;
        Box.<number>(top: 10);
        assert(good.top, 10);
        println(good);
        println(box.top);
        assert(box.top, 361);
        // box = other_box; //not legal
        box = Box(10);
        assert(box.top + 12, 22);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_generic_impl() {
    let src = r#"
        struct Box<T> {top: T}
        impl<T> Box<T> {
            func new(top: T): Box<T> {
                return Box(top: top);
            }
            func unwrap(self): T { return self.top; }

            func map<U>(self, transform: func(T): U): Box<U> {
                return Box(top: transform(self.top));
            }
        }

        impl Box<number> {
            func square(self): number { return self.top * self.top;}
        }

        func wrap(a: number): string { return to_str(a);}


        let box = Box.new(10);
        println(Box(20).square());
        let str_box = box.map(wrap);
        assert(str_box.top + "1", "101");
        assert(box.unwrap() , 10);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_generic_enums() {
    let src = r#"
        enum LinkedList<T> {
            Nil, Cons(T, LinkedList<T>)
        }

        enum Result<T,E> { Ok(T), Err(E) }
        impl<T,E> Result<T,E> {
            func map_error<U>(self, transform: func(E): U): Result<T,U> {
                match self {
                    .Err(err) => {
                        let new_err = transform(err);
                        return Result.Err(new_err);
                    }
                    .Ok(ok) => {return Result.Ok(ok);}
                }
            }
        }
        func wrap(a: number): string { return to_str(a);}

        let list = LinkedList.Cons(1, LinkedList.Nil);
        {
            let res: Result<number, number> = Result.Err(21);
            let res = Result.map_error(res, to_str.<number>);
            if res is Ok {
                println(res + 10);
            }
            else {
                println(res + "10");
            }
        }

        assert(list is Cons, true);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_complex_generic() {
    let src = r#"
        struct Box<T,U> {top: T}

        impl<T,U> Box<T,U> {
            func new(arg: T): Box<T,U> {
                return Box(top: arg);
            }
        }


        let box = Box.<number, never>.new(10);
        println(box.top);
        assert(box.top + 12, 22);
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_panic() {
    let src = r#"
        func panic_func() {
            panic("Panic!");
        }

        func rec(a: number) {
            if a > 0 {rec(a - 1);}
            panic("reached bottom!");
        }
        rec(10);
        "#;
    execute_source(src, false, "run", true);
}

#[test]
fn test_option_enum() {
    let src = r#"
        enum Option<T> { Some(T), None }
        {
            let opt = Option.Some(10);
            if opt is Some {
                println(opt + 10);
            }
            assert(opt is Some, true);
            opt = Option.None;
            assert(opt is None, true);
        }
        "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_structured_equality() {
    let src = r#"
        struct Point {x: number, y: number}
        struct Recurse {other: Recurse?}
        let r = Recurse(other: nil);
        assert(r == r, true);
        r.other = r;
        assert(r == r, true); // If reference shortcut wasn't used this would stack overflow

        assert(Point(x: 1, y: 2) == Point(x: 1, y: 2), true);
        assert(("hehe" , 1 ,"a"), ("hehe" , 1 ,"a"));

        let p1 = Point(x: 1, y: 2);
        let p2 = Point(x: 1, y: 2);
        println(r);
        println(p1);
        println(p2);
        assert(p1 == p2, true);
        assert(p1 != p2, false);
            
    "#;
    execute_source(src, false, "run", true);
}

#[test]
fn test_poly() {
    let src = r#"
    interface Shape {
        func area(self): number;
        func perimeter(self): number;
    }
    struct Circle { radius: number }
    impl Circle : Shape {
        func area(self): number { return 3.14 * self.radius * self.radius; }
        func perimeter(self): number { return 2 * 3.14 * self.radius; }
    }
    struct Rectangle { width: number, height: number }
    impl Rectangle : Shape {
        func area(self): number { return self.width * self.height; }
        func perimeter(self): number { return 2 * (self.width + self.height); }
    }
    func printArea(s: Shape): void {
        s.area();
        assert(s.area(), 78.5); // 5*5 * 3.14
        // println(s.area());
    }
    func printPerimeter(s: Shape): void {
        s.perimeter();
        assert(s.perimeter(), (10 + 5) * 2 );
        // println(s.perimeter());
    }

    let i = 0;
    while i < 1000 {
        let c: Shape = Circle(radius:5);
        let r: Shape = Rectangle(width:10,height:5);
        printArea(c);
        printPerimeter(r);
        i = i + 1;
    }
    "#;
    execute_source(src, false, "run", true);
}
#[test]
fn test_list() {
    let src = r#"
            func process(a: List<number>): number {
                return a[0];
            }
            let list = [1,2,3,4,5];
            assert(list[2], 3);
            println(list[2]);
            println(list);
            list[1] = 10;
            assert(list, [1,10,3,4,5]);
            assert(process(list), 1);
            list[2] = 10;
            process([]);
            assert(false, true);
            println(list);
        "#;
    execute_source(src, false, "run", true);
}
