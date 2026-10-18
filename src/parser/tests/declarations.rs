use super::helpers::*;

#[test]
fn test_function_declaration() {
    assert_parses(
        "func add(a: number, b: number): number { return a + b; }",
        "(func add (param a (type number)) (param b (type number)) (ret (type number)) \
         (block-stmt (return (+ a b))))",
    );
}

#[test]
fn test_generic_function() {
    assert_parses(
        "func identity<T>(x: T): T { return x; }",
        "(func identity (generics T) (param x (type T)) (ret (type T)) (block-stmt (return x)))",
    );
}

#[test]
fn test_function_without_return_type_defaults_to_void() {
    assert_parses("func f() {}", "(func f (ret (type void)) (block-stmt))");
}

#[test]
fn test_public_declarations() {
    assert_parses(
        "public func f() {} public extern func g(); public struct S {} public enum E { A }",
        "(func public f (ret (type void)) (block-stmt)) \
         (extern-func public g (ret (type void))) \
         (struct public S) \
         (enum public E (variant A))",
    );
}

#[test]
fn test_extern_function() {
    assert_parses(
        "extern func g(a: number): string;",
        "(extern-func g (param a (type number)) (ret (type string)))",
    );
}

#[test]
fn test_struct_declaration() {
    assert_parses(
        "struct Point { x: number, y: number }",
        "(struct Point (field x (type number)) (field y (type number)))",
    );
}

#[test]
fn test_public_struct_fields() {
    assert_parses(
        "public struct P<T> { public x: number, y: T }",
        "(struct public P (generics T) (field public x (type number)) (field y (type T)))",
    );
}

#[test]
fn test_enum_variants() {
    assert_parses(
        "enum Color { Red, Green, Blue }",
        "(enum Color (variant Red) (variant Green) (variant Blue))",
    );
    assert_parses(
        "enum Option { Some(number), None }",
        "(enum Option (variant Some (type number)) (variant None))",
    );
    assert_parses(
        "enum Result { Ok { value: number }, Err { msg: string } }",
        "(enum Result (variant Ok (field value (type number))) (variant Err (field msg (type string))))",
    );
}

#[test]
fn test_impl_block() {
    assert_parses(
        "impl Point { func new(): Point { return Point ( x: 0, y: 0 ); } }",
        "(impl (type Point) \
         (func new (ret (type Point)) (block-stmt (return (call Point (x: 0) (y: 0))))))",
    );
}

#[test]
fn test_generic_impl_with_interfaces() {
    assert_parses(
        "impl<T> Point<T>: A, B {}",
        "(impl (generics T) (type Point (type T)) (interfaces A B))",
    );
}

#[test]
fn test_method_with_self() {
    assert_parses(
        "impl P { func get(self): number { return 1; } }",
        "(impl (type P) (func get (param self (type Self)) (ret (type number)) (block-stmt (return 1))))",
    );
}

#[test]
fn test_interface_declaration() {
    assert_parses(
        "interface Printable { func print(self): void; }",
        "(interface Printable (method-sig print (param self (type Self)) (ret (type void))))",
    );
}

#[test]
fn test_public_interface() {
    assert_parses(
        "public interface I { func f(self): void; }",
        "(interface public I (method-sig f (param self (type Self)) (ret (type void))))",
    );
}

#[test]
fn test_type_annotations() {
    assert_parses(
        "let a: number? = nil;",
        "(let (bind a) (optional (type number)) nil)",
    );
    assert_parses(
        "let a: List<number> = x;",
        "(let (bind a) (type List (type number)) x)",
    );
    assert_parses(
        "let a: (number, string) = x;",
        "(let (bind a) (tuple-type (type number) (type string)) x)",
    );
    assert_parses(
        "let a: func(number, string): bool = x;",
        "(let (bind a) (fn-type (type number) (type string) (ret (type bool))) x)",
    );
}

#[test]
fn test_declaration_spans() {
    assert_span("struct P { x: number }", "struct/name", "P");
    assert_span("struct P { x: number }", "struct/field/type", "number");
    assert_span("func f(a: number, b: string) {}", "func/param[1]", "b");
}
