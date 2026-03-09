use crate::vm::tests::helpers::*;

#[test]
fn test_simple_enum() {
    assert_runs(
        r#"
        enum Color { Red, Green, Blue }
        let c = Color.Red;
        match c {
            Color.Red => { assert(true, true); }
            Color.Green => { assert(true, false); }
            Color.Blue => { assert(true, false); }
        }
        assert(c, Color.Red);
        "#,
    );
}

#[test]
fn test_struct_enum_variant() {
    assert_runs(
        r#"
        enum Message {
            Quit,
            Move {
                x: number,
                y: number,
            },
            ChangeColor {
                r: number,
                g: number,
                b: number,
            }
        }
        let msg = Message.Move(x: 10, y: 20);
        match msg {
            .Quit => { assert(true, false); }
            .Move(:x, :y) => {
                assert(x, 10);
                assert(y, 20);
            }
            .ChangeColor(a) => { assert(true, false); }
        }
        "#,
    );
}

#[test]
fn test_value_enum_variants() {
    assert_runs(
        r#"
        enum Shape { Circle(number), Rectangle(number, number) }
        func area(s: Shape): number {
            match s {
                Shape.Circle(radius) => { return 3 * radius * radius; }
                Shape.Rectangle(width, height) => { return width * height; }
            }
        }
        assert(area(Shape.Circle(5)), 75);
        assert(area(Shape.Rectangle(10, 5)), 50);
        "#,
    );
}

#[test]
fn test_enum_branch_analysis() {
    assert_runs(
        r#"
        enum Res { Ok(number), Err(string) }
        {
            let res = Res.Ok(10);
            let res2 = Res.Ok(20);
            if res is Ok and res2 is Ok {
                println(res + res2);
            }

            if res2 is Ok and res2 > 10 {
                println("res2 is bigger than 10: ", res2);
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
        "#,
    );
}

#[test]
fn test_enum_with_methods() {
    assert_runs(
        r#"
        enum Result { Ok(number), Err(string) }
        impl Result {
            func unwrap(self): number {
                match self {
                    Result.Ok(val) => { return val; }
                    Result.Err(err) => {
                        panic("");
                        return 0;
                    }
                }
            }
            func unwrap_or(self, default: number): number {
                match self {
                    Result.Ok(val) => { return val; }
                    Result.Err(_) => { return default; }
                }
            }
        }
        let res = Result.Ok(10);
        let sum = res.unwrap() + 15;
        assert(sum, 25);
        let res2 = Result.Err("Error");
        assert(res2.unwrap_or(10), 10);
        "#,
    );
}

#[test]
fn test_option_enum() {
    assert_runs(
        r#"
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
        "#,
    );
}

#[test]
fn test_enum_loop_with_is() {
    assert_runs(
        r#"
        enum LinkedList<T> { Nil, Cons(T, LinkedList<T>) }
        impl<T> LinkedList<T> {
            func prepend(self, value: T): LinkedList<T> {
                return LinkedList.Cons(value, self);
            }
        }
        {
            let list: LinkedList<number> = LinkedList.Nil;
            list = list.prepend(10);
            list = list.prepend(20);
            list = list.prepend(30);
            let count = 0;
            while list is Cons {
                count += 1;
                list = list.1;
            }
            assert(count, 3);
        }
        "#,
    );
}

#[test]
fn test_enum_unrefinement() {
    assert_runs(
        r#"
        enum Either { Left(number), Right(string) }
        {
            let left = Either.Left(10);
            if left is Left {
                assert(left + 10, 20);
                left = Either.Right("100");
            }
        }
        "#,
    );
}
