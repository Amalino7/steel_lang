use crate::resolver::ModuleGraph;
use crate::typechecker::GlobalIdGenerator;
use crate::typechecker::system::TypeSystem;

/// Helper that runs source with native functions (panic, assert, print)
/// and verifies a runtime error occurs.
fn assert_panics_with_natives(source: &str) {
    use crate::compiler::Compiler;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::stdlib::{get_natives, get_prelude};
    use crate::typechecker::TypeChecker;
    use crate::vm::VM;
    use crate::vm::gc::GarbageCollector;

    let full_source = format!("{}{}", source, get_prelude());
    let natives = get_natives();
    let scanner = Scanner::new(&full_source);
    let mut parser = Parser::new(scanner);
    let ast = parser.parse().expect("Failed to parse");
    let mut sys = TypeSystem::new();
    let id_generator = GlobalIdGenerator::new();
    let module_graph = ModuleGraph::new();
    let mut typechecker = TypeChecker::new(&natives, &mut sys, &id_generator, &module_graph);
    let (typed_ast, _) = typechecker.check(&ast, None).expect("Failed to typecheck");

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(typed_ast.reserved as u8, &typed_ast.file_ast);

    let mut vm = VM::new(id_generator.count(), &mut gc);
    vm.set_natives_by_name(&natives, &typed_ast.extern_fns);

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
