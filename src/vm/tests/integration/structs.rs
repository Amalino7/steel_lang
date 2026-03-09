use crate::vm::tests::helpers::*;

#[test]
fn test_basic_struct() {
    assert_runs(
        r#"
        struct Point {
            x: number,
            y: number,
        }
        let p1 = Point(1, 2);
        assert(p1.x, 1);
        assert(p1.y, 2);

        func make_point(x: number, y: number): Point {
            return Point(x: x, y: y);
        }

        let p2 = make_point(10, 20);
        assert(p2.x + p2.y, 30);
        p2.x = 100;
        assert(p2.x, 100);
        "#,
    );
}

#[test]
fn test_struct_function_fields() {
    assert_runs(
        r#"
        struct Point {
            x: number,
            y: number,
            add: func(number, number): number,
        }

        func add(a: number, b: number): number {
            return a + b;
        }

        let p1 = Point(x: 1, y: 2, add: add);
        assert(p1.x, 1);
        assert(p1.y, 2);
        assert(p1.add(6, 7), 13);
        "#,
    );
}

#[test]
fn test_nested_structs() {
    assert_runs(
        r#"
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
        "#,
    );
}

#[test]
fn test_recursive_structs() {
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
        "#,
    );
}

#[test]
fn test_struct_mixed_field_types() {
    assert_runs(
        r#"
        struct Point {
            x: number,
            y: string,
        }
        let p1 = Point(x: 1, y: "2");
        assert(p1.x, 1);
        assert(p1.y, "2");
        "#,
    );
}

#[test]
fn test_struct_named_initialization_order() {
    assert_runs(
        r#"
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
        "#,
    );
}

#[test]
fn test_struct_methods() {
    assert_runs(
        r#"
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

        let p1 = Point(x: 1, y: 2);
        let p2 = Point(x: 3, y: 4);
        let p3 = p1.add(p2);
        assert(p3.x, 4);
        assert(p3.y, 6);
        assert(10.abs(), 10);
        assert((-14).abs(), 14);
        "#,
    );
}

#[test]
fn test_struct_static_and_instance_methods() {
    assert_runs(
        r#"
        struct Counter {
            value: number,
        }
        impl Counter {
            func inc(self): Counter {
                self.value += 1;
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
        assert(c.value, 14);
        "#,
    );
}

#[test]
fn test_different_method_call_forms() {
    assert_runs(
        r#"
        struct Point {
            x: number,
            y: number,
        }
        impl Point {
            func add(self, other: Point): Point {
                return Point(x: self.x + other.x, y: self.y + other.y);
            }
            func add2(one: Point, two: Point): Point {
                return one.add(two);
            }
        }
        let instance1 = Point(x: 1, y: 2);
        let instance2 = Point(3, y: 4);
        let result = Point.add2(instance1, instance2);
        assert(result.x, 4);
        assert(result.y, 6);
        let result2 = instance1.add(instance2);
        assert(result2.x, 4);
        assert(result2.y, 6);
        let result3 = Point.add(instance1, instance2);
        assert(result3.x, 4);
        assert(result3.y, 6);
        "#,
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_recursive_method() {
    assert_runs(
        r#"
        struct Fiber {}
        impl Fiber {
            func fib(self, num: number): number {
                if num <= 1 {
                    return num;
                }
                return self.fib(num - 1) + self.fib(num - 2);
            }
        }
        let res = Fiber().fib(20);
        assert(res, 6765);
        "#,
    );
}

#[test]
fn test_bound_method() {
    assert_runs(
        r#"
        func _10xer(op: func()) {
            let i = 0;
            while i < 10 {
                op();
                i += 1;
            }
        }

        struct Counter { val: number }
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
        assert(c.val, 12);
        "#,
    );
}

#[test]
fn test_method_on_void() {
    assert_runs(
        r#"
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
        "#,
    );
}

#[test]
fn test_inner_type_method_call() {
    assert_runs(
        r#"
        struct Vector {
            start: Point,
            end: Point,
        }
        struct Point { x: number, y: number }
        impl Point {
            func print(self) {
                println("{", self.x, ",", self.y, "}");
            }
        }
        let vec = Vector(start: Point(x: 1, y: 2), end: Point(x: 3, y: 4));
        vec.start.print();
        "#,
    );
}

#[test]
fn test_complex_linked_list() {
    assert_runs(
        r#"

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
                let current_index = index;
                while head != nil and current_index > 0 {
                    current_index -= 1;
                    head = head.next;
                }
                return head?.value;
            }
            func set(self, index: number, value: number): void {
                let head = self.head;
                let current_index = index;
                while head != nil and current_index > 0 {
                    current_index -= 1;
                    head = head.next;
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
                } else {
                    self.head = new_node;
                }
            }
            func pop(self): number? {
                if self.size == 0 {
                    return nil;
                } else if self.size == 1 {
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

        let list = LinkedList.new();
        list.push(1);
        list.push(2);
        list.push(3);
        list.push(4);
        assert(list.pop(), 4);
        list.set(1, 5);
        assert(list.get(1), 5);
        assert(list.pop(), 3);
        assert(list.pop(), 5);
        assert(list.pop(), 1);
        "#,
    );
}

#[test]
fn test_structured_equality() {
    assert_runs(
        r#"
        struct Point { x: number, y: number }
        struct Recurse { other: Recurse? }
        let r = Recurse(other: nil);
        assert(r == r, true);
        r.other = r;
        assert(r == r, true);

        assert(Point(x: 1, y: 2) == Point(x: 1, y: 2), true);

        let p1 = Point(x: 1, y: 2);
        let p2 = Point(x: 1, y: 2);
        assert(p1 == p2, true);
        assert(p1 != p2, false);
        "#,
    );
}
