use crate::vm::tests::helpers::*;

#[test]
fn test_generic_struct() {
    assert_runs(
        r#"
        struct Point<T> { x: T, y: T }
        let p = Point(x: 1, y: 2);
        let Point(:x, :y) = p;
        assert(x + 0, 1);
        assert(y + 0, 2);
        assert(p.x, 1);
        assert(p.y, 2);
        "#,
    );
}
#[test]
fn test_recursive_infer() {
    assert_runs(
        r#"

       func identity<T>(stg: T): T { return stg; }
       let a = identity(identity);
       let b = identity;

       let first = a(10);
       let other = a("12");
       assert(first + b(1), 11);
       assert(other + b("3"), "123");

         "#,
    );
}
#[test]
fn test_map() {
    assert_runs(
        r#"
        func map<T, U>(a:T, transform: func(T): U): U {
            return transform(a);
        }
        let val = 10;
        println(map(val, to_str));
        "#,
    );
}

#[test]
fn test_generic_function() {
    assert_runs(
        r#"
        func identity<T>(stg: T): T {
            return stg;
        }
        func first<T, U>(pair: (T, U)): T {
            return pair.0;
        }
        func second<T>(pair: (T, T)): T {
            return pair.1;
        }

        let num = identity(3);
        let pair = (1, "str");
        assert(num + 2, 5);

        let a = first(pair);
        assert(a + 2, 3);
        "#,
    );
}

#[test]
fn test_generic_struct_with_map() {
    assert_runs(
        r#"
        struct Box<T> { top: T }

        func square(a: number): number {
            return a * a;
        }
        func unwrap(a: number?): number {
            return a!;
        }

        func map<T, U>(box: Box<T>, transform: func(T): U): Box<U> {
            let res = transform(box.top);
            return Box(top: res);
        }

        let box = Box(19);
        box = map(box, square);
        assert(box.top, 361);

        let new_box = Box.<number?>(nil);
        new_box.top = 10;
        let good = map(new_box, unwrap);
        assert(good.top, 10);
        "#,
    );
}

#[test]
fn test_generic_impl() {
    assert_runs(
        r#"
        struct Box<T> { top: T }
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
            func square(self): number { return self.top * self.top; }
        }

        func wrap(a: number): string { return to_str(a); }

        let box = Box.new(10);
        assert(Box(20).square(), 400);
        let str_box = box.map(wrap);
        assert(str_box.top + "1", "101");
        assert(box.unwrap(), 10);
        "#,
    );
}

#[test]
fn test_generic_with_complex_type_params() {
    assert_runs(
        r#"
        struct Box<T, U> { top: T }

        impl<T, U> Box<T, U> {
            func new(arg: T): Box<T, U> {
                return Box(top: arg);
            }
        }

        let box = Box.<number, never>.new(10);
        assert(box.top + 12, 22);
        "#,
    );
}

#[test]
fn test_generic_enums() {
    assert_runs(
        r#"
        enum LinkedList<T> {
            Nil, Cons(T, LinkedList<T>)
        }

        enum Result<T, E> { Ok(T), Err(E) }
        impl<T, E> Result<T, E> {
            func map_error<U>(self, transform: func(E): U): Result<T, U> {
                match self {
                    .Err(err) => {
                        let new_err = transform(err);
                        return Result.Err(new_err);
                    }
                    .Ok(ok) => { return Result.Ok(ok); }
                }
            }
        }

        let list = LinkedList.Cons(1, LinkedList.Nil);
        assert(list is Cons, true);

        {
            let res: Result<number, number> = Result.Err(21);
            let res = Result.map_error(res, to_str.<number>);
            if res is Ok {
                println(res + 10);
            } else {
                println(res + "10");
            }
        }
        "#,
    );
}
