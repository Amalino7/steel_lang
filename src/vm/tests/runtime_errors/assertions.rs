use crate::vm::tests::helpers::*;

/// Helper that runs source with native functions (panic, assert, print)
/// and verifies a runtime error occurs.
fn assert_panics_with_natives(source: &str) {
    use crate::compiler::Compiler;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::stdlib::get_natives;
    use crate::typechecker::core::ast::StmtKind;
    use crate::typechecker::TypeChecker;
    use crate::vm::gc::GarbageCollector;
    use crate::vm::VM;

    let natives = get_natives();
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let ast = parser.parse().expect("Failed to parse");
    let mut typechecker = TypeChecker::new_with_natives(&natives);
    let (typed_ast, _) = typechecker.check(&ast).expect("Failed to typecheck");

    let global_count = match &typed_ast.kind {
        StmtKind::Global { global_count, .. } => *global_count as usize,
        _ => 0,
    };

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(0, &typed_ast);

    let mut vm = VM::new(global_count, gc);
    vm.set_native_functions(natives);

    assert!(
        vm.run(function).is_err(),
        "Expected runtime error but execution succeeded"
    );
}

#[test]
fn test_panic_builtin_causes_error() {
    assert_panics_with_natives(
        r#"
        panic("custom error message");
        "#,
    );
}

#[test]
fn test_panic_in_function_causes_error() {
    assert_panics_with_natives(
        r#"
        func fail(): void {
            panic("intentional panic");
        }
        fail();
        "#,
    );
}

#[test]
fn test_panic_in_nested_call_causes_error() {
    assert_panics_with_natives(
        r#"
        func inner(): void {
            panic("deep panic");
        }
        func outer(): void {
            inner();
        }
        outer();
        "#,
    );
}
