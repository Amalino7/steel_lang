use crate::Mode;
use crate::compiler::Compiler;
use crate::execute_source;
use crate::parser::Parser;
use crate::resolver::ModuleGraph;
use crate::scanner::Scanner;
use crate::typechecker::system::TypeSystem;
use crate::typechecker::{GlobalIdGenerator, TypeChecker};
use crate::vm::VM;
use crate::vm::gc::GarbageCollector;
use crate::vm::value::Value;

/// Execute source and verify it runs successfully
pub fn assert_runs(source: &str) {
    execute_source(source, false, Mode::Run, true);
}

/// Execute source and verify a global variable has expected value
pub fn assert_global(source: &str, global_index: usize, expected: Value) {
    let scanner = Scanner::new(source, 0);
    let mut parser = Parser::new(scanner);
    let mut sys = TypeSystem::new();
    let id_generator = GlobalIdGenerator::new();
    let module_graph = ModuleGraph::new();
    let mut typechecker = TypeChecker::new(&[], &mut sys, &id_generator, &module_graph);
    let ast = parser.parse().expect("Failed to parse");
    let (typed_ast, _) = typechecker.check(&ast, None).expect("Failed to typecheck");

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(typed_ast.reserved as u8, &typed_ast.file_ast);

    let mut vm = VM::new(id_generator.count(), &mut gc);
    vm.run(function).expect("VM execution failed");

    assert_eq!(
        vm.globals[global_index], expected,
        "Global at index {} does not match expected value",
        global_index
    );
}

/// Execute source (with the full prelude) and verify a runtime error occurs.
/// Use this instead of `assert_panics` when the source calls prelude methods.
pub fn assert_panics_with_prelude(source: &str) {
    use crate::stdlib::{get_natives, get_prelude};

    let full_source = format!("{}{}", source, get_prelude());
    let natives = get_natives();
    let scanner = Scanner::new(&full_source, 0);
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

/// Execute source and verify a global variable holds a string with the given content.
pub fn assert_global_string(source: &str, global_index: usize, expected: &str) {
    let scanner = Scanner::new(source, 0);
    let mut parser = Parser::new(scanner);

    let mut sys = TypeSystem::new();
    let id_generator = GlobalIdGenerator::new();
    let module_graph = ModuleGraph::new();
    let mut typechecker = TypeChecker::new(&[], &mut sys, &id_generator, &module_graph);
    let ast = parser.parse().expect("Failed to parse");
    let (typed_ast, _) = typechecker.check(&ast, None).expect("Failed to typecheck");

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(typed_ast.reserved as u8, &typed_ast.file_ast);

    let mut vm = VM::new(id_generator.count(), &mut gc);
    vm.run(function).expect("VM execution failed");

    match &vm.globals[global_index] {
        Value::String(s) => assert_eq!(
            s.as_str(),
            expected,
            "Global string at index {} does not match",
            global_index
        ),
        v => panic!(
            "Expected string at global index {}, got {:?}",
            global_index, v
        ),
    }
}

/// Assert that the source fails at the parse stage.
pub fn assert_parse_fails(source: &str) {
    let scanner = Scanner::new(source, 0);
    let mut parser = Parser::new(scanner);
    assert!(
        parser.parse().is_err(),
        "Expected a parse error but parsing succeeded"
    );
}

/// Execute source and verify a runtime error occurs.
/// Does not check the error message - just that an error happened.
pub fn assert_panics(source: &str) {
    let scanner = Scanner::new(source, 0);
    let mut parser = Parser::new(scanner);
    let mut sys = TypeSystem::new();
    let id_generator = GlobalIdGenerator::new();
    let module_graph = ModuleGraph::new();
    let mut typechecker = TypeChecker::new(&[], &mut sys, &id_generator, &module_graph);
    let ast = parser.parse().expect("Failed to parse");
    let (typed_ast, _) = typechecker.check(&ast, None).expect("Failed to typecheck");

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(typed_ast.reserved as u8, &typed_ast.file_ast);

    let mut vm = VM::new(id_generator.count(), &mut gc);
    assert!(
        vm.run(function).is_err(),
        "Expected runtime error but execution succeeded"
    );
}
