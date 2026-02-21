use crate::compiler::Compiler;
use crate::execute_source;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::typechecker::core::ast::StmtKind;
use crate::typechecker::TypeChecker;
use crate::vm::gc::GarbageCollector;
use crate::vm::value::Value;
use crate::vm::VM;

/// Execute source and verify it runs successfully
pub fn assert_runs(source: &str) {
    execute_source(source, false, "run", true);
}

/// Execute source and verify a global variable has expected value
pub fn assert_global(source: &str, global_index: usize, expected: Value) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let mut typechecker = TypeChecker::new();
    let ast = parser.parse().expect("Failed to parse");
    let (typed_ast, _) = typechecker.check(&ast).expect("Failed to typecheck");

    let global_count = match &typed_ast.kind {
        StmtKind::Global { global_count, .. } => *global_count as usize,
        _ => 0,
    };

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(0, &typed_ast);

    let mut vm = VM::new(global_count, gc);
    vm.run(function).expect("VM execution failed");

    assert_eq!(
        vm.globals[global_index], expected,
        "Global at index {} does not match expected value",
        global_index
    );
}

/// Execute source and verify a runtime error occurs.
/// Does not check the error message - just that an error happened.
pub fn assert_panics(source: &str) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let mut typechecker = TypeChecker::new();
    let ast = parser.parse().expect("Failed to parse");
    let (typed_ast, _) = typechecker.check(&ast).expect("Failed to typecheck");

    let global_count = match &typed_ast.kind {
        StmtKind::Global { global_count, .. } => *global_count as usize,
        _ => 0,
    };

    let mut gc = GarbageCollector::new();
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(0, &typed_ast);

    let mut vm = VM::new(global_count, gc);
    assert!(
        vm.run(function).is_err(),
        "Expected runtime error but execution succeeded"
    );
}
