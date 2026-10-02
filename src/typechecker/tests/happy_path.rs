use crate::typechecker::tests::helpers::*;

#[test]
fn test_basic_types() {
    assert_typechecks(
        r#"
        let a = 5;
        let b = 10.0;
        let c = "Hello World!";
        let d = true;
        let e = false;
        let f = 5 + 10;
        let g = 5 - 10;
        "#,
    );
}

#[test]
fn test_function_call() {
    assert_typechecks(
        r#"
        func add(a: string, b: string): string {
            return a + b;
        }

        let result = add("5", "10");

        if true {
            result + result;
        }

        func f1() {
            f2();
        }
        func f2() {
            f1();
        }
        "#,
    );
}

#[test]
fn test_function_correct_return_if_else() {
    assert_typechecks(
        r#"
        func add(a: number, b: number): number {
            if a > b {
                a / 4;
            } else {
                return b;
            }

            return 10;
        }
        "#,
    );
}

#[test]
fn test_function_correct_return_both_branches() {
    assert_typechecks(
        r#"
        func add(a: number, b: number): number {
            if a > b {
                return a / 4;
            } else {
                return b;
            }
        }
        "#,
    );
}

#[test]
fn test_nested_function_correct_return() {
    assert_typechecks(
        r#"
        func add(a: number, b: number): number {
            func add2(a: number, b: number): number {
                return add(a, b);
            }

            return add2(a, b);
        }
        "#,
    );
}

#[test]
fn test_complex_function_types() {
    assert_typechecks(
        r#"
        func foo(a: number, _b: func(): string): func(number): number {
            func bar(c: number): number {
                return a + c;
            }
            return bar;
        }
        func str(): string { return "hello"; }

        let res = foo(10, str);
        let sum = res(5) + 10;
        "#,
    );
}

#[test]
fn test_variable_shadowing_types() {
    assert_typechecks(
        r#"
        let a: number = 10;
        {
            let a: string = "shadow";
            a = a + "ed";
        }
        let b = a + 5;
        "#,
    );
}

#[test]
fn test_assign_void() {
    assert_typechecks(
        r#"
        func noReturn(): void {
            return;
        }
        let x = noReturn();
        "#,
    );
}

#[test]
fn test_lambda_return_infers_type_from_return_stmt() {
    // The lambda body always returns via `return`, so the block expr has type Never.
    // The return type should be inferred from the return statement, not the block type.
    assert_typechecks(
        r#"
        func apply<T>(f: func(T): T, x: T): T {
            return f(x);
        }
        apply(|x: number| { return x + 1; }, 5);
        "#,
    );
}

#[test]
fn test_lambda_return_with_explicit_annotation() {
    assert_typechecks(
        r#"
        func apply(f: func(number): number, x: number): number {
            return f(x);
        }
        apply(|x: number|: number { return x * 2; }, 3);
        "#,
    );
}

#[test]
fn test_imported_variables_and_aliases() {
    Tester::new(
        r#"
        import values/{one, two as second};
        let sum: number = one + second;
        "#,
    )
    .with_module(
        "values",
        r#"
        public let one = 10;
        public let two = 20;
        "#,
    )
    .run();
}

#[test]
fn test_glob_import() {
    Tester::new(
        r#"
        import values/{*};
        let sum: number = one + two;
        "#,
    )
    .with_module(
        "values",
        r#"
        public let one = 10;
        public let two = 20;
        "#,
    )
    .run();
}

#[test]
fn test_nested_module_import() {
    Tester::new(
        r#"
        import package/math/add;
        let value: number = add(10, 20);
        "#,
    )
    .with_module(
        "package.math",
        r#"
        public func add(a: number, b: number): number { return a + b; }
        "#,
    )
    .run();
}

#[test]
fn test_imported_struct_methods() {
    Tester::new(
        r#"
        import geometry/Point;
        let p: Point = Point.new(1, 2);
        let dist: number = p.distance_squared();
        let p2: Point = p.add(Point(x: 3, y: 4));
        "#,
    )
    .with_module(
        "geometry",
        r#"
        public struct Point { public x: number, public y: number }
        impl Point {
            public func new(x: number, y: number): Point {
                return Point(x: x, y: y);
            }
            public func distance_squared(self): number {
                return self.x * self.x + self.y * self.y;
            }
            public func add(self, other: Point): Point {
                return Point(x: self.x + other.x, y: self.y + other.y);
            }
        }
        "#,
    )
    .run();
}

#[test]
fn test_imported_struct_alias_methods() {
    Tester::new(
        r#"
        import geometry/Point as Vector;
        let v: Vector = Vector(x: 10, y: 20);
        let len: number = v.length();
        "#,
    )
    .with_module(
        "geometry",
        r#"
        public struct Point { public x: number, public y: number }
        impl Point {
            public func length(self): number {
                return self.x + self.y;
            }
        }
        "#,
    )
    .run();
}

#[test]
fn test_imported_enum_methods() {
    Tester::new(
        r#"
        import results/Outcome;
        let res: Outcome = Outcome.Ok(10);
        let flag: boolean = res.is_ok();
        "#,
    )
    .with_module(
        "results",
        r#"
        public enum Outcome { Ok(number), Err(string) }
        impl Outcome {
            public func is_ok(self): boolean {
                match self {
                    Outcome.Ok(_) => { return true; }
                    Outcome.Err(_) => { return false; }
                }
            }
        }
        "#,
    )
    .run();
}

#[test]
fn test_imported_interface() {
    Tester::new(
        r#"
        import contracts/Printable;

        struct Book { title: string }
        impl Book : Printable {
            public func print(self): void {}
        }

        func display(p: Printable): void {
            p.print();
        }
        "#,
    )
    .with_module(
        "contracts",
        r#"
        public interface Printable {
            func print(self): void;
        }
        "#,
    )
    .run();
}

#[test]
fn test_generic_type_methods_imported() {
    Tester::new(
        r#"
        import containers/Wrapper;
        let w: Wrapper<number> = Wrapper(item: 42);
        let val: number = w.get();
        "#,
    )
    .with_module(
        "containers",
        r#"
        public struct Wrapper<T> { public item: T }
        impl<T> Wrapper<T> {
            public func get(self): T {
                return self.item;
            }
        }
        "#,
    )
    .run();
}
