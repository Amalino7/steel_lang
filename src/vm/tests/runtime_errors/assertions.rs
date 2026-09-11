/// Helper that runs source with native functions (panic, assert, print)
/// and verifies a runtime error occurs.
fn assert_panics_with_natives(source: &str) {
    use crate::compiler::Compiler;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::stdlib::{get_natives, get_prelude};
    use crate::typechecker::TypeChecker;
    use crate::typechecker::core::ast::{FunctionBody, StmtKind};
    use crate::vm::VM;
    use crate::vm::gc::GarbageCollector;

    let full_source = format!("{}{}", source, get_prelude());
    let natives = get_natives();
    let scanner = Scanner::new(&full_source);
    let mut parser = Parser::new(scanner);
    let ast = parser.parse().expect("Failed to parse");
    let mut typechecker = TypeChecker::new_with_natives(&natives);
    let (typed_ast, _) = typechecker.check(&ast).expect("Failed to typecheck");

    let (global_count, extern_fns) = match &typed_ast.kind {
        StmtKind::Global {
            global_count,
            extern_fns,
            ..
        } => (*global_count as usize, extern_fns.clone()),
        _ => (0, vec![]),
    };

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(0, &FunctionBody::Block(Box::new(typed_ast)));

    let mut vm = VM::new(global_count, &mut gc);
    vm.set_natives_by_name(&natives, &extern_fns);

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
