use crate::vm::tests::helpers::*;

// --- Nil / Optional ---

#[test]
fn test_nil_safety_basic() {
    assert_runs(
        r#"
        let a: number? = nil;
        assert(a, nil);
        let b: number = a ?? 0;
        assert(b, 0);
        "#,
    );
}

#[test]
fn test_nil_safe_access() {
    assert_runs(
        r#"
        struct Point { x: number, y: number }
        let p: Point? = Point(x: 1, y: 2);
        assert(p?.x, 1);
        assert(p?.y, 2);
        p = nil;
        let num: number = p?.x ?? 0;
        assert(num, 0);
        assert(p?.x, nil);
        "#,
    );
}

#[test]
fn test_nil_linked_list_operations() {
    assert_runs(
        r#"
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
        "#,
    );
}

#[test]
fn test_force_unwrap_non_nil() {
    assert_runs(
        r#"
        struct Point { x: number, y: number }
        let p: Point? = Point(x: 1, y: 2);
        let strong_p = p!;
        assert(strong_p.x, 1);
        "#,
    );
}

#[test]
fn test_nil_method_on_optional() {
    assert_runs(
        r#"
        struct Point { x: number, y: number }
        impl Point {
            func do_something(self): void {
                assert(1, 2); // should never be called on nil
            }
            func other(self): void {
                assert(1, 1);
            }
        }
        let p: Point? = nil;
        p?.do_something();

        p = Point(1, 2);
        p?.other();
        "#,
    );
}

#[test]
fn test_nil_optional_function() {
    assert_runs(
        r#"
        let fun: func?(): void = nil;
        if fun?() != nil {
            assert(1, 2); // unreachable
        }

        func other(): void { println("Yellow"); }
        fun = other;
        fun?();
        "#,
    );
}

#[test]
fn test_nil_refinement_basic() {
    assert_runs(
        r#"
        func main() {
            let x: number? = nil;
            x = 10;
            if x != nil {
                println(x + 10);
            } else {
                println(x);
                return;
            }
            println(x + 1);
        }
        main();
        "#,
    );
}

#[test]
fn test_nil_refinement_interface() {
    assert_runs(
        r#"
        let num_called = 0;
        interface Printable {
            func print(self): void;
        }
        struct Point { x: number, y: number }
        impl Point : Printable {
            func print(self): void { num_called += 1; println(self.x, self.y); }
        }
        func main() {
            let p: Printable? = Point(x: 1, y: 2);
            if p != nil {
                p.print();
            }
            if p == nil {
                assert(1, 2);
                return;
            } else {
                p.print();
            }
            p.print();
        }
        main();
        assert(num_called, 3);
        "#,
    );
}

#[test]
fn test_nil_refinement_logical_operators() {
    assert_runs(
        r#"
        struct Array { length: number }
        func main() {
            let x: Array? = Array(length: 10);
            if x != nil and x.length > 0 {
                assert(x.length, 10);
            }

            x = nil;

            if x == nil or x.length == 0 {
                assert(x, nil);
                return;
            }
        }
        main();
        "#,
    );
}

#[test]
fn test_unrefinement_after_assignment() {
    assert_runs(
        r#"
        {
            let a: number? = 10;
            if a != nil {
                assert(a * a, 100);
                if true {
                    a = nil;
                }
                a = 10;
                println(a ?? 0);
            }
        }
        "#,
    );
}

// --- Tuples ---

#[test]
fn test_tuple_basic() {
    assert_runs(
        r#"
        func takes_tuple(a: (number, string)) {
            assert(a.0, 1);
            assert(a.1, "What");
        }
        let tuple = (1, "What");
        takes_tuple(tuple);
        assert(tuple.0, 1);
        assert(tuple.1, "What");
        "#,
    );
}

#[test]
fn test_tuple_return_and_destructure() {
    assert_runs(
        r#"
        func returns_tuple(): (number, number) { return (1, 2); }
        let tuple = returns_tuple();
        let (a, b) = returns_tuple();
        assert(a, 1);
        assert(b, 2);

        let tuple2: (number, number)? = (3, 4);
        assert(tuple2?.0, 3);
        assert(tuple2?.1, 4);

        tuple2?.1 = 2;
        assert(tuple2?.1, 2);

        tuple2 = nil;
        assert(tuple2?.0, nil);
        assert(tuple.0, 1);
        assert(tuple.1, 2);
        "#,
    );
}

#[test]
fn test_struct_destructuring() {
    assert_runs(
        r#"
        struct Point { x: number, y: number }

        let Point(y: x, x: y) = Point(x: 1, y: 2);
        assert(x, 2);
        assert(y, 1);

        struct Complex { one: (number, number, Other, Other), two: Other }
        struct Other { a: number, b: number }

        let Complex(one: (a, b, _, other), two: Other(a: c, b: d)) = Complex(
            one: (1, 2, Other(3, 4), Other(5, 6)),
            two: Other(7, 8)
        );
        assert(a, 1);
        assert(b, 2);
        assert(other.a, 5);
        assert(other.b, 6);
        assert(c, 7);
        assert(d, 8);
        "#,
    );
}

// --- Misc ---

#[test]
fn test_panic_builtin() {
    // panic() produces a runtime error - execute_source just prints it
    assert_runs(
        r#"
        func panic_func() {
            panic("Panic!");
        }

        func rec(a: number) {
            if a > 0 { rec(a - 1); }
            panic("reached bottom!");
        }
        rec(10);
        "#,
    );
}
