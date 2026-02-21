use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_undefined_field_access() {
    // Note: field read access on structs falls through to method lookup and
    // produces UndefinedMethod (not UndefinedField). UndefinedField is only
    // emitted for assignment (write) to an undefined field.
    Tester::new(
        r#"
        struct Point { x: number, y: number }
        let p = Point(x: 1, y: 2);
        let z = p.z;
        "#,
    )
    .expect_error(
        |e| matches!(e, TypeCheckerError::UndefinedMethod { method_name, .. } if method_name == "z"),
    )
    .run();
}

#[test]
fn test_undefined_field_assignment() {
    Tester::new(
        r#"
        struct Point { x: number, y: number }
        let p = Point(x: 1, y: 2);
        p.z = 10;
        "#,
    )
    .expect_error(
        |e| matches!(e, TypeCheckerError::UndefinedField { field_name, .. } if field_name == "z"),
    )
    .run();
}

// Note: field access on primitives (number, string, boolean) produces UndefinedMethod
// since the typechecker treats `.field` as a method lookup for named types.
// TypeHasNoFields is only emitted for unnamed types (functions, optionals, etc.).

#[test]
fn test_field_access_on_number_gives_undefined_method() {
    Tester::new(
        r#"
        let x = 5;
        let y = x.non_existent_field;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod { .. }))
    .run();
}

#[test]
fn test_field_access_on_string_gives_undefined_method() {
    Tester::new(
        r#"
        let s = "hello";
        let x = s.non_existent_field;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod { .. }))
    .run();
}

#[test]
fn test_field_access_on_boolean_gives_undefined_method() {
    Tester::new(
        r#"
        let b = true;
        let x = b.non_existent_field;
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod { .. }))
    .run();
}

#[test]
fn test_struct_outside_global_scope_in_function() {
    Tester::new(
        r#"
        func foo() {
            struct Point { x: number }
        }
        "#,
    )
    .expect_error(
        |e| matches!(e, TypeCheckerError::StructOutsideOfGlobalScope { name, .. } if name == "Point"),
    )
    .run();
}

#[test]
fn test_struct_outside_global_scope_in_block() {
    Tester::new(
        r#"
        {
            struct Point { x: number }
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::StructOutsideOfGlobalScope { .. }))
    .run();
}

#[test]
fn test_undefined_method() {
    Tester::new(
        r#"
        struct Point { x: number }
        impl Point {
            func new(): Point { return Point(x: 0); }
        }
        let p = Point(x: 5);
        p.undefined_method();
        "#,
    )
    .expect_error(|e| {
        matches!(e, TypeCheckerError::UndefinedMethod { method_name, .. } if method_name == "undefined_method")
    })
    .run();
}

#[test]
fn test_static_method_on_instance() {
    Tester::new(
        r#"
        struct Point { x: number }
        impl Point {
            func new(): Point { return Point(x: 0); }
        }
        let p = Point(x: 5);
        p.new();
        "#,
    )
    .expect_error(
        |e| matches!(e, TypeCheckerError::StaticMethodOnInstance { method_name, .. } if method_name == "new"),
    )
    .run();
}
