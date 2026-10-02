use crate::typechecker::tests::helpers::*;

const SHAPES: &str = r#"
    public struct Point { public x: number }
    impl Point {
        public func norm(self): number { return self.x; }
    }
"#;

#[test]
fn test_extension_shadows_inherent_method() {
    Tester::new(
        r#"
        import a/Point;
        impl Point {
            public func norm(self): number { return 1; }
        }
        "#,
    )
    .with_module("a", SHAPES)
    .expect_error(|e| matches!(e, TypeCheckerError::ExtensionShadowsInherent { .. }))
    .run();
}

#[test]
fn test_conflicting_extensions_from_two_imports() {
    Tester::new(
        r#"
        import a/Point;
        import ext1/{*};
        import ext2/{*};
        "#,
    )
    .with_module("a", "public struct Point { public x: number }")
    .with_module(
        "ext1",
        r#"
        import a/Point;
        impl Point { public func twice(self): number { return 1; } }
        "#,
    )
    .with_module(
        "ext2",
        r#"
        import a/Point;
        impl Point { public func twice(self): number { return 2; } }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::ConflictingExtension { .. }))
    .run();
}

#[test]
fn test_import_private_function() {
    Tester::new("import a/secret;")
        .with_module("a", "func secret() {}")
        .expect_error(
            |e| matches!(e, TypeCheckerError::PrivateImport { name, .. } if name == "secret"),
        )
        .run();
}

#[test]
fn test_import_private_type() {
    Tester::new("import a/Hidden;")
        .with_module("a", "struct Hidden { x: number }")
        .expect_error(
            |e| matches!(e, TypeCheckerError::PrivateImport { name, .. } if name == "Hidden"),
        )
        .run();
}

#[test]
fn test_import_reexport_not_supported() {
    Tester::new("import b/x;")
        .with_module("a", "public let x = 1;")
        .with_module("b", "import a/x;")
        .expect_error(|e| {
            matches!(e, TypeCheckerError::ImportNotFound { name, note: Some(_), .. } if name == "x")
        })
        .run();
}

#[test]
fn test_import_primitive_for_extensions_only() {
    Tester::new(
        r#"
        import units/number;
        let d = (5).km();
        "#,
    )
    .with_module(
        "units",
        "impl number { public func km(self): number { return self * 1000; } }",
    )
    .run();
}

const SECRET: &str = r#"
    public struct Account { public id: number, balance: number }
    impl Account {
        public func peek(self): number { return self.balance; }
        public func bump(self) { self.balance = self.balance + 1; }
    }
"#;

#[test]
fn test_private_field_read_from_other_module() {
    Tester::new(
        r#"
        import a/Account;
        func f(acc: Account): number { return acc.balance; }
        "#,
    )
    .with_module("a", SECRET)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateField { field_name, .. } if field_name.as_ref() == "balance"))
    .run();
}

#[test]
fn test_private_field_assign_from_other_module() {
    Tester::new(
        r#"
        import a/Account;
        func f(acc: Account) { acc.balance = 3; }
        "#,
    )
    .with_module("a", SECRET)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateField { .. }))
    .run();
}

#[test]
fn test_private_field_destructure_from_other_module() {
    Tester::new(
        r#"
        import a/Account;
        func f(acc: Account): number { let Account(balance: b) = acc; return b; }
        "#,
    )
    .with_module("a", SECRET)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateField { .. }))
    .run();
}

#[test]
fn test_private_constructor_from_other_module() {
    Tester::new(
        r#"
        import a/Account;
        let acc = Account(1, 2);
        "#,
    )
    .with_module("a", SECRET)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateConstructor { private_fields, .. } if private_fields.len() == 1 && private_fields[0].0 == "balance"))
    .run();
}

#[test]
fn test_private_field_ok_inside_defining_module() {
    Tester::new(SECRET).run();
}

const HIDDEN: &str = r#"
    public struct Vault { public id: number }
    impl Vault {
        func secret(self): number { return self.id; }
        func make(): Vault { return Vault(1); }
    }
"#;

#[test]
fn test_private_method_call_from_other_module() {
    Tester::new(
        r#"
        import a/Vault;
        func f(v: Vault): number { return v.secret(); }
        "#,
    )
    .with_module("a", HIDDEN)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateMethod { method_name, .. } if method_name.as_ref() == "secret"))
    .run();
}

#[test]
fn test_private_static_method_from_other_module() {
    Tester::new(
        r#"
        import a/Vault;
        let v = Vault.make();
        "#,
    )
    .with_module("a", HIDDEN)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateMethod { method_name, .. } if method_name.as_ref() == "make"))
    .run();
}

#[test]
fn test_private_method_property_style_from_other_module() {
    Tester::new(
        r#"
        import a/Vault;
        func f(v: Vault): number { let g = v.secret; return g(); }
        "#,
    )
    .with_module("a", HIDDEN)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateMethod { .. }))
    .run();
}

#[test]
fn test_private_method_not_suggested() {
    Tester::new(
        r#"
        import a/Vault;
        func f(v: Vault): number { return v.secrte(); }
        "#,
    )
    .with_module("a", HIDDEN)
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod(u) if u.suggestions.is_empty()))
    .run();
}

#[test]
fn test_interface_method_impl_must_be_public() {
    Tester::new(
        r#"
        public interface Shape { func area(self): number; }
        struct Sq { side: number }
        impl Sq: Shape {
            func area(self): number { return self.side; }
        }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::InterfaceMethodNotPublic { method_name, .. } if method_name == "area"))
    .run();
}

#[test]
fn test_private_interface_method_impl_may_be_private() {
    Tester::new(
        r#"
        interface Shape { func area(self): number; }
        struct Sq { side: number }
        impl Sq: Shape {
            func area(self): number { return self.side; }
        }
        "#,
    )
    .run();
}

fn is_leak(e: &TypeCheckerError, ty: &str) -> bool {
    matches!(e, TypeCheckerError::PrivateTypeInPublicApi { private_type, .. } if private_type.as_ref() == ty)
}

#[test]
fn test_private_type_in_public_function() {
    Tester::new(
        r#"
        struct Secret { x: number }
        public func f(): List<Secret> { return []; }
        "#,
    )
    .expect_error(|e| is_leak(e, "Secret"))
    .run();
}

#[test]
fn test_private_type_in_public_field_and_payload() {
    Tester::new(
        r#"
        struct Secret { x: number }
        public struct Holder { public inner: Secret? }
        public enum Wrap { Some(Secret), Named { value: Secret } }
        "#,
    )
    .expect_error(|e| is_leak(e, "Secret"))
    .expect_error(|e| is_leak(e, "Secret"))
    .run();
}

#[test]
fn test_private_type_in_public_method_and_interface() {
    Tester::new(
        r#"
        struct Secret { x: number }
        public interface Leaky { func get(self): Secret; }
        public struct Box { public x: number }
        impl Box {
            public func open(self): Secret { return Secret(x: self.x); }
            func fine(self): Secret { return Secret(x: self.x); }
        }
        "#,
    )
    .expect_error(|e| is_leak(e, "Secret"))
    .expect_error(|e| is_leak(e, "Secret"))
    .run();
}

#[test]
fn test_no_leak_for_private_items_and_imported_types() {
    Tester::new(
        r#"
        import a/Account;
        struct Secret { x: number }
        func helper(s: Secret): Secret { return s; }
        struct Inner { public s: Secret }
        public func g(a: Account): number { return a.id; }
        "#,
    )
    .with_module("a", "public struct Account { public id: number }")
    .run();
}

#[test]
fn test_private_type_in_public_global_and_extension() {
    Tester::new(
        r#"
        import a/Account;
        struct Secret { x: number }
        public let s = Secret(x: 1);
        impl Account {
            public func secret(self): Secret { return Secret(x: self.id); }
        }
        "#,
    )
    .with_module("a", "public struct Account { public id: number }")
    .expect_error(|e| is_leak(e, "Secret"))
    .expect_error(|e| is_leak(e, "Secret"))
    .run();
}

// ---- coverage added by /test (see docs/features/visibility/tests.md) ----

fn is_private_import(e: &TypeCheckerError, n: &str) -> bool {
    matches!(e, TypeCheckerError::PrivateImport { name, .. } if name == n)
}

#[test]
fn test_import_private_global() {
    Tester::new("import a/g;")
        .with_module("a", "let g = 1;")
        .expect_error(|e| is_private_import(e, "g"))
        .run();
}

#[test]
fn test_import_private_enum_and_interface() {
    Tester::new("import a/{E, I};")
        .with_module("a", "enum E { A } interface I { func f(self): void; }")
        .expect_error(|e| is_private_import(e, "E"))
        .expect_error(|e| is_private_import(e, "I"))
        .run();
}

#[test]
fn test_import_public_items_of_every_kind() {
    Tester::new(
        r#"
        import a/{f, g, S, E, I, now};
        let x: number = f() + g;
        let s = S(v: 1);
        let e = E.A;
        let n = now();
        func take(i: I): I { return i; }
        "#,
    )
    .with_module(
        "a",
        r#"
        public func f(): number { return 1; }
        public let g = 2;
        public struct S { public v: number }
        public enum E { A }
        public interface I { func m(self): void; }
        public extern func now(): number;
        "#,
    )
    .run();
}

#[test]
fn test_group_import_failure_does_not_abort_others() {
    Tester::new(
        r#"
        import a/{hidden, shown, missing};
        let x: number = shown;
        "#,
    )
    .with_module("a", "let hidden = 1; public let shown = 2;")
    .expect_error(|e| is_private_import(e, "hidden"))
    .expect_error(
        |e| matches!(e, TypeCheckerError::ImportNotFound { name, .. } if name == "missing"),
    )
    .run();
}

#[test]
fn test_glob_does_not_bind_transitive_imports() {
    Tester::new(
        r#"
        import b/{*};
        let y = x;
        "#,
    )
    .with_module("a", "public let x = 1;")
    .with_module("b", "import a/x; public let z = x;")
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedVariable { .. }))
    .run();
}

#[test]
fn test_glob_binds_public_only() {
    Tester::new(
        r#"
        import a/{*};
        let ok = shown;
        let bad = hidden;
        "#,
    )
    .with_module("a", "public let shown = 1; let hidden = 2;")
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedVariable { .. }))
    .run();
}

#[test]
fn test_glob_brings_extensions() {
    Tester::new(
        r#"
        import units/{*};
        let d = (5).km();
        "#,
    )
    .with_module(
        "units",
        "impl number { public func km(self): number { return self * 1000; } }",
    )
    .run();
}

#[test]
fn test_import_suggestion_hides_private_items() {
    Tester::new("import a/secrte;")
        .with_module("a", "let secret = 1; public let other = 2;")
        .expect_error(|e| {
            matches!(e, TypeCheckerError::ImportNotFound { suggestions, .. }
                if !suggestions.iter().any(|s| s == "secret"))
        })
        .run();
}

#[test]
fn test_private_field_nil_safe_access() {
    Tester::new(
        r#"
        import a/Account;
        func f(acc: Account?): number? { return acc?.balance; }
        "#,
    )
    .with_module("a", SECRET)
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateField { .. }))
    .run();
}

#[test]
fn test_all_public_struct_constructs_elsewhere() {
    Tester::new(
        r#"
        import a/P;
        let p = P(x: 1, y: 2);
        let q: number = p.x + p.y;
        "#,
    )
    .with_module(
        "a",
        "public struct P { public x: number, public y: number }",
    )
    .run();
}

#[test]
fn test_generic_struct_private_field() {
    Tester::new(
        r#"
        import a/Box;
        func f(b: Box<number>): number { return b.inner; }
        "#,
    )
    .with_module("a", "public struct Box<T> { inner: T }")
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateField { .. }))
    .run();
}

#[test]
fn test_private_inherent_method_on_escaped_value() {
    // `Vault` is never imported; only the public factory is.
    Tester::new(
        r#"
        import a/open;
        let n = open().secret();
        "#,
    )
    .with_module(
        "a",
        r#"
        public struct Vault { public id: number }
        impl Vault { func secret(self): number { return self.id; } }
        public func open(): Vault { return Vault(id: 1); }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateMethod { .. }))
    .run();
}

#[test]
fn test_private_methods_ok_inside_defining_module() {
    Tester::new(
        r#"
        public struct V { public id: number }
        impl V {
            func secret(self): number { return self.id; }
            func make(): V { return V(id: 1); }
            public func run(self): number { return V.make().secret() + self.secret(); }
        }
        "#,
    )
    .run();
}

#[test]
fn test_extension_requires_import() {
    Tester::new("let d = (5).km();")
        .with_module(
            "units",
            "impl number { public func km(self): number { return self * 1000; } }",
        )
        .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod(_)))
        .run();
}

#[test]
fn test_extension_on_imported_type_requires_import_from_extender() {
    Tester::new(
        r#"
        import a/P;
        func f(p: P): number { return p.double(); }
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module(
        "ext",
        r#"
        import a/P;
        impl P { public func double(self): number { return self.x * 2; } }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod(_)))
    .run();
}

#[test]
fn test_extension_import_does_not_bind_private_type_name() {
    // ext only borrows `P`; importing `ext/P` brings the extension but not the name.
    Tester::new(
        r#"
        import ext/P;
        import a/make;
        let p = make();
        let n: number = p.double();
        let bad = P(x: 1);
        "#,
    )
    .with_module(
        "a",
        "public struct P { public x: number } public func make(): P { return P(x: 1); }",
    )
    .with_module(
        "ext",
        r#"
        import a/P;
        impl P { public func double(self): number { return self.x * 2; } }
        "#,
    )
    .expect_error(|e| {
        matches!(
            e,
            TypeCheckerError::UndefinedVariable { .. } | TypeCheckerError::UndefinedType { .. }
        )
    })
    .run();
}

#[test]
fn test_import_alias_keeps_extensions() {
    Tester::new(
        r#"
        import a/P as Q;
        import ext/P as Unused;
        func g(q: Q): number { return q.double(); }
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module(
        "ext",
        r#"
        import a/P;
        impl P { public func double(self): number { return self.x * 2; } }
        "#,
    )
    .expect_warning(|w| matches!(w, TypeCheckerWarning::UnboundExtensionAlias { alias, .. } if alias == "Unused"))
    .run();
}

#[test]
fn test_alias_of_public_type_does_not_warn() {
    Tester::new("import a/P as Q; func g(q: Q): number { return q.x; }")
        .with_module("a", "public struct P { public x: number }")
        .run();
}

#[test]
fn test_extension_import_without_alias_does_not_warn() {
    Tester::new(
        r#"
        import a/P;
        import ext/P;
        func g(p: P): number { return p.double(); }
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module(
        "ext",
        r#"
        import a/P;
        impl P { public func double(self): number { return self.x * 2; } }
        "#,
    )
    .run();
}

#[test]
fn test_failed_private_type_import_does_not_cascade() {
    // The type exists, so uses in type position must not add "type not defined" errors.
    Tester::new(
        r#"
        import a/Hidden;
        func f(h: Hidden): number { return h.x; }
        "#,
    )
    .with_module("a", "struct Hidden { public x: number }")
    .expect_error(|e| matches!(e, TypeCheckerError::PrivateImport { name, .. } if name == "Hidden"))
    .run();
}

#[test]
fn test_failed_reexport_type_import_does_not_cascade() {
    Tester::new(
        r#"
        import b/P;
        func f(p: P): number { return p.x; }
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module("b", "import a/P;")
    .expect_error(|e| matches!(e, TypeCheckerError::ImportNotFound { name, .. } if name == "P"))
    .run();
}

#[test]
fn test_private_field_error_points_at_field_declaration() {
    // The label must be the field's own span, not the whole struct's.
    Tester::new(
        r#"
        import a/Account;
        func f(acc: Account): number { return acc.balance; }
        "#,
    )
    .with_module("a", SECRET)
    .expect_error(|e| {
        matches!(e, TypeCheckerError::PrivateField { definition, .. }
            if SECRET[definition.start..definition.end] == *"balance")
    })
    .run();
}

#[test]
fn test_import_resolves_by_local_alias_in_module() {
    Tester::new(
        r#"
        import a/P;
        import ext/Renamed;
        func f(p: P): number { return p.double(); }
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module(
        "ext",
        r#"
        import a/P as Renamed;
        impl Renamed { public func double(self): number { return self.x * 2; } }
        "#,
    )
    .run();
}

#[test]
fn test_extensions_not_transitive() {
    // `b` imports ext's extensions, but importing `b/P` must not re-export them.
    Tester::new(
        r#"
        import a/P;
        import b/P;
        func f(p: P): number { return p.double(); }
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module(
        "ext",
        r#"
        import a/P;
        impl P { public func double(self): number { return self.x * 2; } }
        "#,
    )
    .with_module("b", "import a/P; import ext/P; public let k = 1;")
    .expect_error(|e| matches!(e, TypeCheckerError::ImportNotFound { .. }))
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedMethod(_)))
    .run();
}

#[test]
fn test_extension_shadows_private_inherent_method() {
    Tester::new(
        r#"
        import a/Vault;
        impl Vault { public func secret(self): number { return 1; } }
        "#,
    )
    .with_module(
        "a",
        r#"
        public struct Vault { public id: number }
        impl Vault { func secret(self): number { return self.id; } }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::ExtensionShadowsInherent { .. }))
    .run();
}

#[test]
fn test_conflicting_local_and_imported_extension() {
    Tester::new(
        r#"
        import a/Point;
        import ext1/{*};
        impl Point { public func twice(self): number { return 2; } }
        "#,
    )
    .with_module("a", "public struct Point { public x: number }")
    .with_module(
        "ext1",
        r#"
        import a/Point;
        impl Point { public func twice(self): number { return 1; } }
        "#,
    )
    .expect_error(|e| matches!(e, TypeCheckerError::ConflictingExtension { .. }))
    .run();
}

#[test]
fn test_non_interface_method_in_impl_may_be_private() {
    Tester::new(
        r#"
        public interface Shape { func area(self): number; }
        struct Sq { side: number }
        impl Sq : Shape {
            public func area(self): number { return self.helper(); }
            func helper(self): number { return self.side; }
        }
        "#,
    )
    .run();
}

#[test]
fn test_no_privacy_error_on_error_receiver() {
    // The receiver is Type::Error; only the undefined variables are reported.
    Tester::new(
        r#"
        import a/Account;
        func f(): number { return nope.balance + nope2.secret(); }
        "#,
    )
    .with_module("a", SECRET)
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedVariable { .. }))
    .expect_error(|e| matches!(e, TypeCheckerError::UndefinedVariable { .. }))
    .run();
}

fn is_leak_in(e: &TypeCheckerError, item: &str) -> bool {
    matches!(e, TypeCheckerError::PrivateTypeInPublicApi { item: i, .. } if i.as_ref() == item)
}

#[test]
fn test_leak_errors_are_ordered_by_source_position() {
    // Hash-map order would shuffle these; the pass must report them as written.
    Tester::new(
        r#"
        struct Secret { x: number }
        public func first(): Secret { return Secret(x: 1); }
        public let second: Secret? = nil;
        public func third(_s: Secret) {}
        public struct Fourth { public inner: Secret }
        public func fifth(): List<Secret> { return []; }
        "#,
    )
    .expect_error(|e| is_leak_in(e, "first"))
    .expect_error(|e| is_leak_in(e, "second"))
    .expect_error(|e| is_leak_in(e, "third"))
    .expect_error(|e| is_leak_in(e, "Fourth"))
    .expect_error(|e| is_leak_in(e, "fifth"))
    .run();
}

#[test]
fn test_wildcard_extension_conflict_is_reported_at_the_import() {
    Tester::new(
        r#"
        import a/P;
        import ext1/{*};
        import ext2/{*};
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module(
        "ext1",
        r#"
        import a/P;
        impl P { public func twice(self): number { return 1; } }
        "#,
    )
    .with_module(
        "ext2",
        r#"
        import a/P;
        impl P { public func twice(self): number { return 2; } }
        "#,
    )
    .expect_error(|e| {
        // The primary span is in the importing module, not in either extension's file.
        matches!(e, TypeCheckerError::ConflictingExtension { span, first, second, .. }
            if span.file_id != first.file_id && span.file_id != second.file_id
                && first.file_id != second.file_id)
    })
    .run();
}

#[test]
fn test_named_extension_conflict_is_reported_at_the_import() {
    Tester::new(
        r#"
        import a/P;
        import ext1/P;
        import ext2/P;
        "#,
    )
    .with_module("a", "public struct P { public x: number }")
    .with_module(
        "ext1",
        r#"
        import a/P;
        impl P { public func twice(self): number { return 1; } }
        "#,
    )
    .with_module(
        "ext2",
        r#"
        import a/P;
        impl P { public func twice(self): number { return 2; } }
        "#,
    )
    .expect_error(|e| {
        matches!(e, TypeCheckerError::ConflictingExtension { span, first, second, .. }
            if span.file_id != first.file_id && span.file_id != second.file_id)
    })
    .run();
}
