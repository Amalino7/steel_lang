use crate::vm::tests::helpers::*;

#[test]
fn test_arithmetic_add() {
    assert_global("let a = 1 + 2;", 0, crate::vm::value::Value::Number(3.0));
}

#[test]
fn test_modulo_basic() {
    assert_global("let a = 10 % 3;", 0, crate::vm::value::Value::Number(1.0));
}

#[test]
fn test_modulo_even() {
    assert_global("let a = 10 % 2;", 0, crate::vm::value::Value::Number(0.0));
}

#[test]
fn test_modulo_compound_assignment() {
    assert_global(
        r#"
        let a = 17;
        a %= 5;
        "#,
        0,
        crate::vm::value::Value::Number(2.0),
    );
}

#[test]
fn test_power_basic() {
    assert_global(
        "let a = 2 ** 10;",
        0,
        crate::vm::value::Value::Number(1024.0),
    );
}

#[test]
fn test_power_right_associative() {
    // 2 ** 3 ** 2  ==  2 ** (3 ** 2)  ==  2 ** 9  ==  512
    assert_global(
        "let a = 2 ** 3 ** 2;",
        0,
        crate::vm::value::Value::Number(512.0),
    );
}

#[test]
fn test_power_unary_minus_on_base() {
    // -3 ** 2  ==  -(3 ** 2)  ==  -9  (unary minus wraps the whole expression)
    assert_global("let a = -3 ** 2;", 0, crate::vm::value::Value::Number(-9.0));
}

#[test]
fn test_power_unary_minus_on_exponent() {
    assert_global("let a = 4 ** -1;", 0, crate::vm::value::Value::Number(0.25));
}

#[test]
fn test_power_precedence_over_multiply() {
    assert_global(
        "let a = 2 * 3 ** 2;",
        0,
        crate::vm::value::Value::Number(18.0),
    );
}
#[test]
fn test_tricky_power_precedence() {
    // -2 ** -2 ** 2  ==  -(2 ** -(2 ** 2))  ==  -0.0625
    assert_global(
        "let a = -2 ** -2 ** 2;",
        0,
        crate::vm::value::Value::Number(-0.0625),
    )
}

#[test]
fn test_arithmetic_expression() {
    assert_global(
        "let a = 7 + 3 * 2 == 1;",
        0,
        crate::vm::value::Value::Boolean(false),
    );
}

#[test]
fn test_comparison() {
    assert_global("let a = 7 >= 1;", 0, crate::vm::value::Value::Boolean(true));
}

#[test]
fn test_while_loop() {
    assert_global(
        r#"
        let a = 1;
        while a < 10 {
            a = a + 1;
        }
        "#,
        0,
        crate::vm::value::Value::Number(10.0),
    );
}

#[test]
fn test_local_variables() {
    assert_runs(
        r#"
        let a = 0;
        {
            let a = 1;
            {
                let b = 2;
                {
                    let c = 3;
                    {
                        let d = 4;
                        a + b + c + d;
                    }
                    let e = 5;
                }
            }
            let f = 6;
            {
                let g = 7;
                a + f + g;
            }
        }
        "#,
    );
}

#[test]
fn test_short_circuit() {
    assert_runs(
        r#"
        let a = 0;
        if a != 0 and (10 / a > 1) {
            // should not evaluate second condition
        }
        "#,
    );
}

#[test]
fn test_assignment_operators() {
    assert_runs(
        r#"
        let a = 2;
        a = a += 1;
        assert(a, 3);

        a -= 2;
        assert(a, 1);
        a *= 3;
        a /= 2;
        assert(a, 1.5);

        a *= a -= a += 2;
        assert(a, -3);
        "#,
    );
}

#[test]
fn test_shadowing() {
    assert_runs(
        r#"
        let a = 2;
        {
            let a = a + 5;
            assert(a, 7);
        }
        a = 1;
        assert(a, 1);
        "#,
    );
}

#[test]
fn test_forward_declaration() {
    assert_runs(
        r#"
        assert(add(1, 2), 3);
        func add(a: number, b: number): number {
            return a + b;
        }
        "#,
    );
}

#[test]
fn test_local_override() {
    assert_runs(
        r#"
        {
            let a = 1;
            {
                let b = 2;
            }
            let c = 100;
            assert(a + c, 101);
        }
        "#,
    );
}

#[test]
fn torture_test() {
    assert_runs(
        r#"
        let g_counter = 0;
        func complex(n: number): number {
            g_counter += 1;
            let g_counter = "string shadow";

            if n <= 0 {
                return 0;
            }

            if n > 5 and n < 10 or n == 20 {
                return n;
            }

            return 1 + complex(n - 1);
        }
        print(complex(10));
        assert(g_counter, 2);

        {
            let x = 10;
            {
                let x = true;
                {
                    let x = "deep";
                }
                if x {
                    // do nothing
                }
            }
            x = x + 1;
        }

        let empty = "";
        let combined = empty + "start" + empty + "end";
        assert(combined, "startend");
        "#,
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_gc_stress() {
    assert_runs(
        r#"
        let a = 0;
        let str = "";
        while a < 100 {
            str += "a";
            a += 1;
        }
        "#,
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_string_concatenation_large() {
    assert_runs(
        r#"
        let str = "Hello";
        let i = 1;
        while i < 1000 {
            i += 1;
            str += "a";
        }
        "#,
    );
}
