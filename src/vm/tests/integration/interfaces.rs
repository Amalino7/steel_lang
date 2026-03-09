use crate::vm::tests::helpers::*;

#[test]
fn test_basic_interface() {
    assert_runs(
        r#"
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

        let d: Drawable = Point(x: 1, y: 2);
        d.draw();
        let o: Drawable = Other();
        o.draw();
        draw(d);
        draw(o);
        "#,
    );
}

#[test]
fn test_interface_autocast() {
    assert_runs(
        r#"
        interface Drawable {
            func draw(self): void;
        }
        struct Point { x: number, y: number }
        impl Point : Drawable {
            func draw(self): void {
                println(self.x, " ", self.y);
            }
        }

        func something(a: Drawable): void { a.draw(); }

        let p: Drawable = Point(x: 1, y: 2);
        p = Point(x: 3, y: 4);
        p.draw();
        something(Point(x: 5, y: 6));

        let other = Point(x: 7, y: 8);
        other.draw();
        "#,
    );
}

#[test]
fn test_interface_on_primitive() {
    assert_runs(
        r#"
        interface Drawable {
            func draw(self): void;
        }
        impl number : Drawable {
            func draw(self): void {
                assert(self, 10);
            }
        }
        let n: Drawable = 10;
        n.draw();
        "#,
    );
}

#[test]
fn test_interface_multiple_types() {
    assert_runs(
        r#"
        interface Printable {
            func print(self): void;
        }
        struct Point { x: number, y: number }
        impl Point : Printable {
            func print(self): void {
                assert(self.x, 3);
                assert(self.y, 4);
            }
        }
        impl number : Printable {
            func print(self): void {
                assert(self, 10);
            }
        }

        func myPrint(p: Printable): void { p.print(); }

        myPrint(Point(x: 3, y: 4));
        myPrint(10);
        "#,
    );
}

#[test]
fn test_interface_with_multiple_methods() {
    assert_runs(
        r#"
        interface Shape {
            func area(self): number;
            func perimeter(self): number;
        }
        struct Circle { radius: number }
        impl Circle : Shape {
            func area(self): number { return 3 * self.radius * self.radius; }
            func perimeter(self): number { return 2 * 3 * self.radius; }
        }
        struct Rectangle { width: number, height: number }
        impl Rectangle : Shape {
            func area(self): number { return self.width * self.height; }
            func perimeter(self): number { return 2 * (self.width + self.height); }
        }

        let c: Shape = Circle(radius: 5);
        let r: Shape = Rectangle(width: 10, height: 5);
        assert(c.area(), 75);
        assert(r.perimeter(), 30);
        "#,
    );
}

#[test]
fn test_polymorphism_loop() {
    assert_runs(
        r#"
        interface Shape {
            func area(self): number;
            func perimeter(self): number;
        }
        struct Circle { radius: number }
        impl Circle : Shape {
            func area(self): number { return 3 * self.radius * self.radius; }
            func perimeter(self): number { return 2 * 3 * self.radius; }
        }
        struct Rectangle { width: number, height: number }
        impl Rectangle : Shape {
            func area(self): number { return self.width * self.height; }
            func perimeter(self): number { return 2 * (self.width + self.height); }
        }

        let i = 0;
        while i < 100 {
            let c: Shape = Circle(radius: 5);
            let r: Shape = Rectangle(width: 10, height: 5);
            assert(c.area(), 75);
            assert(r.perimeter(), 30);
            i = i + 1;
        }
        "#,
    );
}

#[test]
fn test_nil_interface() {
    assert_runs(
        r#"
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
        "#,
    );
}
