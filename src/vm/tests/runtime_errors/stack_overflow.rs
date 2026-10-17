use crate::vm::tests::helpers::*;

#[test]
fn test_unbounded_recursion_is_a_runtime_error() {
    assert_panics(
        r#"
        func d(n: number): number {
            return d(n + 1);
        }
        d(0);
        "#,
    );
}

#[test]
fn test_recursion_within_limit_runs() {
    assert_runs(
        r#"
        func sum(n: number): number {
            if n == 0 {
                return 0;
            }
            return n + sum(n - 1);
        }
        assert(sum(1000), 500500);
        "#,
    );
}

#[test]
fn test_unbounded_method_recursion_is_a_runtime_error() {
    assert_panics(
        r#"
        struct Looper {}
        impl Looper {
            func go(self, n: number): number {
                return self.go(n + 1);
            }
        }
        Looper().go(0);
        "#,
    );
}

#[test]
fn test_unbounded_local_recursion_is_a_runtime_error() {
    assert_panics(
        r#"
        func main(): void {
            func d(n: number): number {
                return d(n + 1);
            }
            d(0);
        }
        main();
        "#,
    );
}
