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
        let one = 10;
        let two = 20;
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
        func add(a: number, b: number): number { return a + b; }
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
        let one = 10;
        let two = 20;
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
        let one = 10;
        let two = 20;
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
        struct Point { x: number, y: number }
        impl Point {
            func new(x: number, y: number): Point {
                return Point(x: x, y: y);
            }
            func distance_squared(self): number {
                return self.x * self.x + self.y * self.y;
            }
            func add(self, other: Point): Point {
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
        struct Point { x: number, y: number }
        impl Point {
            func length_squared(self): number {
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
        enum Outcome { Success(number), Failure(string) }
        impl Outcome {
            func is_ok(self): boolean {
                match self {
                    Outcome.Success(_) => { return true; }
                    Outcome.Failure(_) => { return false; }
                }
            }
            func unwrap_or(self, default_val: number): number {
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
            func describe(self): string {
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
        interface Describable {
            func describe(self): string;
        }

        struct Item { title: string }
        impl Item : Describable {
            func describe(self): string {
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
        struct Account { balance: number }
        impl Account {
            func create(initial: number): Account {
                return Account(balance: initial);
            }
            func deposit(self, amount: number): void {
                self.balance += amount;
            }
        }

        enum Status { Inactive, Active }
        impl Status {
            func code(self): number {
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
        struct Stack { items: [number] }
        impl Stack {
            func new(): Stack {
                return Stack(items: []);
            }
            func push(self, val: number): void {
                self.items.push(val);
            }
            func peek(self): number {
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
        struct Box<T> { value: T }
        impl<T> Box<T> {
            func get(self): T {
                return self.value;
            }
        }
        "#,
    )
    .assert_runs();
}
