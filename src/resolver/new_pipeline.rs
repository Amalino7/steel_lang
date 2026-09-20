use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::resolver::ModuleResolver;
use crate::scanner::Scanner;
use crate::stdlib::{get_natives, get_prelude};
use crate::typechecker::system::TypeSystem;
use crate::typechecker::{GlobalIdGenerator, TypeChecker};
use crate::vm::VM;
use crate::vm::gc::GarbageCollector;
use std::path::Path;

pub fn pipeline(entry_file: &Path) {
    let mut res = ModuleResolver::new();
    let mut graph = res.resolve(entry_file).unwrap(); // TODO errors
    let mut sys = TypeSystem::new();
    let mut gc = GarbageCollector::new();

    let id_generator = GlobalIdGenerator::new();

    let natives = get_natives();

    let mut compiled_files = vec![];

    let mut extern_fns = vec![];

    let exports = {
        let prelude = get_prelude();
        let scanner = Scanner::new(prelude);
        let mut parser = Parser::new(scanner);
        let mut ty_checker = TypeChecker::new(&natives, &mut sys, &id_generator, &graph);
        let (typed_file, _) = ty_checker.check(&parser.parse().unwrap(), None).unwrap();

        let compiler = Compiler::new("prelude".to_string(), &mut gc);
        compiled_files.push(compiler.compile(typed_file.reserved as u8, &typed_file.file_ast));
        extern_fns.extend(typed_file.extern_fns);
        typed_file.exports
    };

    for idx in 0..graph.modules.len() {
        let scanner = Scanner::new(&graph.modules[idx].source);
        let mut parser = Parser::new(scanner);
        let mut ty_checker = TypeChecker::new(&[], &mut sys, &id_generator, &graph);
        let (typed_file, _) = ty_checker
            .check(&parser.parse().unwrap(), Some(&exports))
            .unwrap();
        graph.modules[idx].exports = typed_file.exports;

        let compiler = Compiler::new(graph.modules[idx].name.clone(), &mut gc);
        extern_fns.extend(typed_file.extern_fns);
        compiled_files.push(compiler.compile(typed_file.reserved as u8, &typed_file.file_ast));
    }
    let compiler = Compiler::new("main".to_string(), &mut gc);
    let global_func = compiler.compile_many(compiled_files);

    let mut vm = VM::new(id_generator.count(), &mut gc);
    vm.set_natives_by_name(&natives, &extern_fns);

    vm.run(global_func).unwrap();
}
