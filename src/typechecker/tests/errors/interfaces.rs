use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::tests::helpers::*;

#[test]
fn test_missing_interface_method() {
    Tester::new(
        r#"
        interface Printable {
            func print(self): void;
            func debug(self): string;
        }

        struct Point { x: number }
        impl Point : Printable {
            func print(self): void { }
        }
        "#,
    )
    .expect_error(|e| {
        matches!(e, TypeCheckerError::MissingInterfaceMethods {
            missing_methods, ..
        } if missing_methods.iter().any(|m| m == "debug"))
    })
    .run();
}

#[test]
fn test_missing_all_interface_methods() {
    Tester::new(
        r#"
        interface Drawable {
            func draw(self): void;
        }

        struct Point { x: number }
        impl Point : Drawable {
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::MissingInterfaceMethods { .. }))
    .run();
}

#[test]
fn test_interface_method_return_type_mismatch() {
    Tester::new(
        r#"
        interface Printable {
            func print(self): void;
        }

        struct Point { x: number }
        impl Point : Printable {
            func print(self): string { return "hi"; }
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::InterfaceMethodTypeMismatch { .. }))
    .run();
}

#[test]
fn test_valid_interface_implementation() {
    assert_typechecks(
        r#"
        interface Printable {
            func print(self): void;
        }

        struct Point { x: number, y: number }
        impl Point : Printable {
            func print(self): void { }
        }

        func do_print(p: Printable): void {
            p.print();
        }
        "#,
    );
}

#[test]
fn test_valid_interface_multiple_methods() {
    assert_typechecks(
        r#"
        interface Shape {
            func area(self): number;
            func perimeter(self): number;
        }

        struct Circle { radius: number }
        impl Circle : Shape {
            func area(self): number { return 3 * self.radius * self.radius; }
            func perimeter(self): number { return 2 * 3 * self.radius; }
        }
        "#,
    );
}
