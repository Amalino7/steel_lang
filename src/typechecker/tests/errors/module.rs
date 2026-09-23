use crate::typechecker::tests::helpers::*;

#[test]
fn test_error_undefined_method_on_imported_type() {
    Tester::new(
        r#"
        import geometry/Point;
        let p = Point(x: 1, y: 2);
        p.non_existent_method();
        "#,
    )
    .with_module(
        "geometry",
        r#"
        struct Point { x: number, y: number }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod(_)))
    .run();
}

#[test]
fn test_error_type_mismatch_imported_function() {
    Tester::new(
        r#"
        import math/add;
        let x = add(1, "not_a_number");
        "#,
    )
    .with_module(
        "math",
        r#"
        func add(a: number, b: number): number { return a + b; }
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::CallParam(_) | TypeCheckerError::TypeMismatch { .. }
        )
    })
    .run();
}

#[test]
fn test_error_type_mismatch_imported_method() {
    Tester::new(
        r#"
        import geometry/Point;
        let p = Point(x: 1, y: 2);
        p.add("invalid_arg");
        "#,
    )
    .with_module(
        "geometry",
        r#"
        struct Point { x: number, y: number }
        impl Point {
            func add(self, other: Point): Point { return other; }
        }
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::CallParam(_) | TypeCheckerError::TypeMismatch { .. }
        )
    })
    .run();
}

#[test]
fn test_error_unimported_type_usage() {
    Tester::new(
        r#"
        import math/add;
        let p: Point = nil;
        "#,
    )
    .with_module(
        "math",
        r#"
        func add(a: number, b: number): number { return a + b; }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedType { .. }))
    .run();
}

#[test]
fn test_error_static_method_on_imported_instance() {
    Tester::new(
        r#"
        import geometry/Point;
        let p = Point(x: 1, y: 2);
        p.new(3, 4);
        "#,
    )
    .with_module(
        "geometry",
        r#"
        struct Point { x: number, y: number }
        impl Point {
            func new(x: number, y: number): Point { return Point(x: x, y: y); }
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::StaticMethodOnInstance { .. }))
    .run();
}

#[test]
fn test_error_imported_interface_missing_method() {
    Tester::new(
        r#"
        import contracts/Printable;
        struct Doc { title: string }
        impl Doc : Printable {}
        "#,
    )
    .with_module(
        "contracts",
        r#"
        interface Printable {
            func print(self): void;
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::MissingInterfaceMethods { .. }))
    .run();
}

#[test]
fn test_item_does_not_exist() {
    Tester::new(
        r#"
        import outside/garbage;
        garbage + "";
    "#,
    )
    .with_module(
        "outside",
        r#"
        let meaning = "hello";
    "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::ImportNotFound {
                name,..
            } if name == "garbage"
        )
    })
    .run();
}

#[test]
fn test_item_close_name() {
    Tester::new(
        r#"
        import outside/naem;
        naem + "";
    "#,
    )
    .with_module(
        "outside",
        r#"
        let name = "James";
    "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::ImportNotFound {
                name,suggestions, ..
            } if name == "naem" && !suggestions.is_empty() && suggestions[0] == "name"
        )
    })
    .run();
}
