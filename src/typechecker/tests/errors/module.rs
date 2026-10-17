use crate::typechecker::core::error::BindingError;
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
        public struct Point { public x: number, public y: number }
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
        public func add(a: number, b: number): number { return a + b; }
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
        public struct Point { public x: number, public y: number }
        impl Point {
            public func add(self, other: Point): Point { return other; }
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
        public func add(a: number, b: number): number { return a + b; }
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
        public struct Point { public x: number, public y: number }
        impl Point {
            public func new(x: number, y: number): Point { return Point(x: x, y: y); }
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
        public interface Printable {
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
        public let meaning = "hello";
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
        public let name = "James";
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

const TEST_PRELUDE: &str = r#"
    public func max(a: number, b: number): number { return a + b; }
    public struct Pair { public a: number }
"#;

#[test]
fn test_prelude_function_is_callable_from_an_imported_module() {
    Tester::new(
        r#"
        import util/twice;
        let x: number = twice(1);
        "#,
    )
    .with_prelude(TEST_PRELUDE)
    .with_module(
        "util",
        r#"
        public func twice(n: number): number { return max(n, n); }
        "#,
    )
    .run();
}

#[test]
fn test_redefining_a_std_function_is_an_error() {
    Tester::new("func max(a: number, b: number): number { return a + b; }")
        .with_prelude(TEST_PRELUDE)
        .expect_error(
            |e| matches!(e, TypeCheckerError::Binding(BindingError::Redeclaration { name, .. }) if name == "max"),
        )
        .run();
}

#[test]
fn test_redefining_a_std_type_is_an_error() {
    Tester::new("struct Pair { a: number }")
        .with_prelude(TEST_PRELUDE)
        .expect_error(|e| matches!(e, TypeCheckerError::Duplicate(_)))
        .run();
}

#[test]
fn test_extern_func_outside_std_is_an_error() {
    Tester::new("extern func clock(): number;")
        .expect_error(|e| matches!(e, TypeCheckerError::ExternOutsideStd { .. }))
        .run();
}

#[test]
fn test_extern_func_in_a_user_module_is_an_error_even_with_std() {
    Tester::new("import util/x; let y = x;")
        .with_prelude("")
        .with_module("util", "public extern func x(): number;")
        .expect_error(|e| matches!(e, TypeCheckerError::ExternOutsideStd { .. }))
        .run();
}

#[test]
fn test_extern_method_outside_std_is_an_error() {
    Tester::new("impl number { public extern func floor(self): number; }")
        .expect_error(|e| matches!(e, TypeCheckerError::ExternOutsideStd { .. }))
        .run();
}

#[test]
fn test_extern_func_in_std_without_a_native_is_an_error() {
    Tester::new("")
        .with_prelude("public extern func nothing_binds_this(): number;")
        .expect_error(
            |e| matches!(e, TypeCheckerError::UnknownNative { name, .. } if name == "nothing_binds_this"),
        )
        .run();
}

#[test]
fn test_extern_method_in_std_without_a_native_is_an_error() {
    Tester::new("")
        .with_prelude("impl number { public extern func no_such(self): number; }")
        .expect_error(
            |e| matches!(e, TypeCheckerError::UnknownNative { name, .. } if name == "number.no_such"),
        )
        .run();
}

// The importing module is the entry, so it is the first file the resolver numbers.
const ENTRY_FILE: crate::scanner::FileId = crate::scanner::FileId(1);

#[test]
fn test_wildcard_import_clash_is_reported_in_the_importing_file() {
    Tester::new("import m1/{*}; import m2/{*};")
        .with_module("m1", "public func dup(): number { return 1; }")
        .with_module("m2", "public func dup(): number { return 2; }")
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::Binding(BindingError::Redeclaration { name, span, .. })
                    if name == "dup" && span.file_id == ENTRY_FILE
            )
        })
        .run();
}

#[test]
fn test_wildcard_type_clash_is_reported_in_the_importing_file() {
    Tester::new("import m1/{*}; import m2/{*};")
        .with_module("m1", "public struct Dup { public a: number }")
        .with_module("m2", "public struct Dup { public b: number }")
        .expect_error(|e| {
            matches!(e, TypeCheckerError::Duplicate(d) if d.name == "Dup" && d.span.file_id == ENTRY_FILE)
        })
        .run();
}

#[test]
fn test_named_import_clash_between_different_items_is_reported_at_the_import() {
    Tester::new("import m1/dup; import m2/dup;")
        .with_module("m1", "public func dup(): number { return 1; }")
        .with_module("m2", "public let dup = 2;")
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::Binding(BindingError::Redeclaration { name, span, .. })
                    if name == "dup" && span.file_id == ENTRY_FILE
            )
        })
        .run();
}

#[test]
fn test_redeclaring_an_imported_function_points_at_the_import() {
    // The original is the import in this file, not the definition in `m`.
    Tester::new("import m/dup;\nfunc dup(): number { return 2; }")
        .with_module("m", "public func dup(): number { return 1; }")
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::Binding(BindingError::Redeclaration { name, original, .. })
                    if name == "dup" && original.file_id == ENTRY_FILE && original.start == 9
            )
        })
        .run();
}

#[test]
fn test_redeclaring_an_aliased_type_names_the_alias() {
    Tester::new("import m/{Foo as Bar};\nstruct Bar { x: number }")
        .with_module("m", "public struct Foo { public x: number }")
        .expect_error(|e| {
            matches!(
                e,
                TypeCheckerError::Duplicate(d)
                    if d.name == "Bar" && d.original.is_some_and(|o| o.file_id == ENTRY_FILE)
            )
        })
        .run();
}

#[test]
fn test_redeclaring_a_name_bound_by_an_unavailable_import_names_it_and_points_at_the_import() {
    // The clash depends only on this file's text, so it is reported even though the module is missing.
    Tester::new(
        "import nope/Foo;
struct Foo { x: number }",
    )
    .expect_resolver_errors(1)
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::Duplicate(d)
                if d.name == "Foo"
                    && d.span.file_id == ENTRY_FILE
                    && d.original.is_some_and(|o| o.file_id == ENTRY_FILE && o.start == 12)
        )
    })
    .run();
}
