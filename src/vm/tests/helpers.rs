use crate::compiler::Compiler;
use crate::execute_source;
use crate::Mode;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::typechecker::core::ast::StmtKind;
use crate::typechecker::TypeChecker;
use crate::vm::gc::GarbageCollector;
use crate::vm::value::Value;
use crate::vm::VM;

/// Execute source and verify it runs successfully
pub fn assert_runs(source: &str) {
    execute_source(source, false, Mode::Run, true);
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

    let mut vm = VM::new(global_count, &mut gc);
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
    let scanner = Scanner::new(&full_source);
    let mut parser = Parser::new(scanner);
    let ast = parser.parse().expect("Failed to parse");
    let mut typechecker = crate::typechecker::TypeChecker::new_with_natives(&natives);
    let (typed_ast, _) = typechecker.check(&ast).expect("Failed to typecheck");

    let (global_count, extern_fns) = match &typed_ast.kind {
        crate::typechecker::core::ast::StmtKind::Global {
            global_count,
            extern_fns,
            ..
        } => (*global_count as usize, extern_fns.clone()),
        _ => (0, vec![]),
    };

    let mut gc = GarbageCollector::new();
    let compiler = crate::compiler::Compiler::new("main".to_string(), &mut gc);
    let function = compiler.compile(0, &typed_ast);

    let mut vm = VM::new(global_count, &mut gc);
    vm.set_natives_by_name(&natives, &extern_fns);

    assert!(
        vm.run(function).is_err(),
        "Expected runtime error but execution succeeded"
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

    let mut vm = VM::new(global_count, &mut gc);
    assert!(
        vm.run(function).is_err(),
        "Expected runtime error but execution succeeded"
    );
}
