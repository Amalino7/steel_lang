use crate::vm::tests::helpers::*;

#[test]
fn test_mutually_recursive_functions() {
    assert_runs(
        r#"
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
        "#,
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_fib() {
    assert_runs(
        r#"
        let a = 0;
        func fib(n: number): number {
            if n == 1 or n == 2 {
                return 1;
            }
            return fib(n - 1) + fib(n - 2);
        }
        a = fib(20);
        assert(a, 6765);
        "#,
    );
}

#[test]
fn test_local_functions() {
    assert_runs(
        r#"
        func main(): void {
            func local_func(a: number): number {
                return a + 1;
            }
            let res = local_func(1);
            assert(res, 2);
        }
        main();
        "#,
    );
}

#[test]
fn test_higher_order_functions() {
    assert_runs(
        r#"
        func foo(a: number, b: func(): string): func(number): number {
            print(b());
            func bar(c: number): number {
                return 10 + c;
            }
            return bar;
        }
        func str(): string { return "hello"; }

        let res = foo(10, str);
        let sum = res(5) + 10;
        assert(sum, 25);
        assert(res(10), 20);
        "#,
    );
}

#[test]
fn test_closure() {
    assert_runs(
        r#"
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
        "#,
    );
}

#[test]
fn test_closure_capture() {
    assert_runs(
        r#"
        {
            let i = 5;
            while i < 10 {
                func foo(): number {
                    return i + i;
                }
                i += 1;
                assert(foo(), i + i - 2);
            }
        }
        "#,
    );
}

#[test]
fn test_complex_closure() {
    assert_runs(
        r#"
        func outer(): func() {
            let x = "outside";
            func inner() {
                print(x);
            }
            return inner;
        }

        let closure = outer();
        closure();
        "#,
    );
}

#[test]
fn test_complex_closure_with_args() {
    assert_runs(
        r#"
        func outer(a: number): func(number): number {
            let x = a;
            func inner(b: number): number {
                return x + b;
            }
            return inner;
        }
        let closure = outer(10);
        assert(closure(5), 15);
        "#,
    );
}

#[test]
fn test_complex_higher_order_closure() {
    assert_runs(
        r#"
        func add(a: number, b: number): number { return a + b; }
        func sub(a: number, b: number): number { return a - b; }
        func mul(a: number, b: number): number { return a * b; }

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
        assert(adder(3)(4), 7);
        assert(subtractor(3)(4), -1);
        assert(multiplier(3)(4), 12);
        "#,
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_local_recursion() {
    assert_runs(
        r#"
        func main(): void {
            func fib(n: number): number {
                if n <= 1 {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
            assert(fib(20), 6765);
        }
        main();
        "#,
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_function_recursion() {
    assert_runs(
        r#"
        {
            func fib(n: number): number {
                if n <= 1 {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
            fib(20);
        }
        "#,
    );
}

#[test]
fn test_named_arguments() {
    assert_runs(
        r#"
        func print_msg(msg: string) {
            println("Here ->", msg);
        }
        impl number {
            func add(self, other: number): number {
                return self + other;
            }
        }
        println(10.add(other: 12));
        print_msg(msg: "Nice to meet you!");
        "#,
    );
}
