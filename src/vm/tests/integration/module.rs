use crate::vm::tests::helpers::TestBuilder;

#[test]
fn test_basic_module() {
    TestBuilder::new(
        "main",
        r#"
        import random/garbage/{one, two};
        assert(one + two, 30);
        "#,
    )
    .with_module(
        "random.garbage",
        r#"
        public let one = 10;
        public let two = 20;
        "#,
    )
    .assert_runs()
}

#[test]
fn test_module_alias_import() {
    TestBuilder::new(
        "main",
        r#"
        import math/add as sum;
        assert(sum(10, 20), 30);
        "#,
    )
    .with_module(
        "math",
        r#"
        public func add(a: number, b: number): number { return a + b; }
        "#,
    )
    .assert_runs()
}

#[test]
fn test_module_glob_import() {
    TestBuilder::new(
        "main",
        r#"
        import values/{*};
        assert(one + two, 30);
        "#,
    )
    .with_module(
        "values",
        r#"
        public let one = 10;
        public let two = 20;
        "#,
    )
    .assert_runs()
}

#[test]
fn test_nested_module_imports() {
    TestBuilder::new(
        "main",
        r#"
        import package/{math/one, math/two as second};
        assert(one + second, 30);
        "#,
    )
    .with_module(
        "package.math",
        r#"
        public let one = 10;
        public let two = 20;
        "#,
    )
    .assert_runs()
}

#[test]
fn test_imported_struct_and_methods() {
    TestBuilder::new(
        "main",
        r#"
        import geometry/Point;
        let p = Point.new(3, 4);
        assert(p.x, 3);
        assert(p.y, 4);
        assert(p.distance_squared(), 25);
        let p2 = p.add(Point(x: 1, y: 2));
        assert(p2.x, 4);
        assert(p2.y, 6);
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
    .assert_runs();
}

#[test]
fn test_imported_struct_alias_and_methods() {
    TestBuilder::new(
        "main",
        r#"
        import geometry/Point as Vector;
        let v = Vector(x: 5, y: 12);
        assert(v.length_squared(), 169);
        "#,
    )
    .with_module(
        "geometry",
        r#"
        public struct Point { public x: number, public y: number }
        impl Point {
            public func length_squared(self): number {
                return self.x * self.x + self.y * self.y;
            }
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_imported_enum_and_methods() {
    TestBuilder::new(
        "main",
        r#"
        import results/Outcome;
        let ok = Outcome.Success(42);
        let err = Outcome.Failure("failed");
        assert(ok.is_ok(), true);
        assert(err.is_ok(), false);
        assert(ok.unwrap_or(0), 42);
        assert(err.unwrap_or(0), 0);
        "#,
    )
    .with_module(
        "results",
        r#"
        public enum Outcome { Success(number), Failure(string) }
        impl Outcome {
            public func is_ok(self): boolean {
                match self {
                    Outcome.Success(_) => { return true; }
                    Outcome.Failure(_) => { return false; }
                }
            }
            public func unwrap_or(self, default_val: number): number {
                match self {
                    Outcome.Success(val) => { return val; }
                    Outcome.Failure(_) => { return default_val; }
                }
            }
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_imported_interface_implementation() {
    TestBuilder::new(
        "main",
        r#"
        import types/{Describable, Item};

        struct User { name: string }
        impl User : Describable {
            public func describe(self): string {
                return "User: " + self.name;
            }
        }

        func print_desc(d: Describable): string {
            return d.describe();
        }

        let u = User(name: "Alice");
        let item = Item(title: "Book");
        assert(print_desc(u), "User: Alice");
        assert(print_desc(item), "Item: Book");
        "#,
    )
    .with_module(
        "types",
        r#"
        public interface Describable {
            func describe(self): string;
        }

        public struct Item { public title: string }
        impl Item : Describable {
            public func describe(self): string {
                return "Item: " + self.title;
            }
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_glob_imported_types_and_methods() {
    TestBuilder::new(
        "main",
        r#"
        import models/{*};
        let acc = Account.create(100);
        acc.deposit(50);
        assert(acc.balance, 150);
        let s = Status.Active;
        assert(s.code(), 1);
        "#,
    )
    .with_module(
        "models",
        r#"
        public struct Account { public balance: number }
        impl Account {
            public func create(initial: number): Account {
                return Account(balance: initial);
            }
            public func deposit(self, amount: number): void {
                self.balance += amount;
            }
        }

        public enum Status { Inactive, Active }
        impl Status {
            public func code(self): number {
                match self {
                    Status.Inactive => { return 0; }
                    Status.Active => { return 1; }
                }
            }
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_nested_module_types_and_methods() {
    TestBuilder::new(
        "main",
        r#"
        import data/collections/Stack;
        let s = Stack.new();
        s.push(10);
        assert(s.peek(), 10);
        "#,
    )
    .with_module(
        "data.collections",
        r#"
        public struct Stack { public items: List<number> }
        impl Stack {
            public func new(): Stack {
                return Stack(items: []);
            }
            public func push(self, val: number): void {
                self.items.push(val);
            }
            public func peek(self): number {
                return self.items[self.items.len() - 1];
            }
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_generic_type_methods_imported() {
    TestBuilder::new(
        "main",
        r#"
        import containers/Box;
        let b = Box(value: 123);
        assert(b.get(), 123);
        "#,
    )
    .with_module(
        "containers",
        r#"
        public struct Box<T> { public value: T }
        impl<T> Box<T> {
            public func get(self): T {
                return self.value;
            }
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_inherent_method_without_importing_type() {
    TestBuilder::new(
        "main",
        r#"
        import factory/make;
        let p = make();
        assert(p.norm(), 3);
        "#,
    )
    .with_module(
        "factory",
        r#"
        public struct Point { public x: number }
        impl Point {
            public func norm(self): number { return self.x; }
        }
        public func make(): Point { return Point(x: 3); }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_visibility_end_to_end() {
    TestBuilder::new(
        "main",
        r#"
        import shapes/{make, Shape};
        import units/{*};

        let s = make(3);
        assert(s.side, 3);
        assert(s.area(), 9);

        let shape: Shape = s;
        assert(shape.describe(), 9);

        assert(2.km(), 2000);
        "#,
    )
    .with_module(
        "shapes",
        r#"
        public interface Shape { func describe(self): number; }
        public struct Square { public side: number }
        impl Square : Shape {
            public func describe(self): number { return self.area(); }
            public func area(self): number { return self.side * self.side; }
        }
        public func make(side: number): Square { return Square(side: side); }
        "#,
    )
    .with_module(
        "units",
        r#"
        impl number {
            public func km(self): number { return self * 1000; }
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_enum_variants_from_other_module() {
    TestBuilder::new(
        "main",
        r#"
        import shapes/Shape;
        let a = Shape.Unit;
        let b = Shape.Circle(2);
        let c = Shape.Rect(w: 2, h: 3);
        func area(s: Shape): number {
            match s {
                Shape.Unit => { return 0; }
                Shape.Circle(r) => { return r; }
                Shape.Rect(:w, :h) => { return w * h; }
            }
        }
        assert(area(a) + area(b) + area(c), 8);
        "#,
    )
    .with_module(
        "shapes",
        "public enum Shape { Unit, Circle(number), Rect { w: number, h: number } }",
    )
    .assert_runs();
}

#[test]
fn test_interface_dispatch_without_importing_impl() {
    TestBuilder::new(
        "main",
        r#"
        import api/{Named, make};
        let n: Named = make();
        assert(n.name(), "bob");
        "#,
    )
    .with_module(
        "api",
        r#"
        public interface Named { func name(self): string; }
        public struct Person { n: string }
        impl Person : Named { public func name(self): string { return self.n; } }
        public func make(): Person { return Person(n: "bob"); }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_prelude_api_reachable_without_imports() {
    TestBuilder::new(
        "main",
        r#"
        let xs = [1, 2, 3];
        assert(xs.len(), 3);
        println("hi");
        "#,
    )
    .assert_runs();
}

#[test]
fn test_closure_keeps_module_access_rights() {
    TestBuilder::new(
        "main",
        r#"
        import a/counter;
        let f = counter();
        assert(f(), 1);
        "#,
    )
    .with_module(
        "a",
        r#"
        struct Hidden { n: number }
        public func counter(): func(): number {
            let h = Hidden(n: 1);
            return || h.n;
        }
        "#,
    )
    .assert_runs();
}

#[test]
fn test_public_global_read_and_assign_from_importer() {
    TestBuilder::new(
        "main",
        r#"
        import a/counter;
        counter = counter + 1;
        assert(counter, 6);
        "#,
    )
    .with_module("a", "public let counter = 5;")
    .assert_runs();
}
