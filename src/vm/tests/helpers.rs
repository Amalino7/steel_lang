use crate::RunResult;
use crate::parser::Parser;
use crate::resolver::new_pipeline::pipeline;
use crate::resolver::resolution::Source;
use crate::scanner::{FileId, Scanner};
use crate::vm::VM;
use crate::vm::value::Value;
use crate::{ColorChoice, Mode, RunConfig};
use std::collections::HashMap;

/// Execute source and verify it runs successfully
pub fn assert_runs(source: &str) {
    let res = pipeline(&RunConfig::new(
        "test.steel",
        Source::File {
            name: "test.steel",
            source,
        },
        Mode::Run,
        false,
    ));
    assert_eq!(res.result, RunResult::Ok)
}

pub struct TestBuilder {
    sources: HashMap<String, &'static str>,
    entry: &'static str,
}

impl TestBuilder {
    pub fn new(name: &'static str, src: &'static str) -> Self {
        let mut builder = TestBuilder {
            sources: Default::default(),
            entry: name,
        };
        builder.sources.insert(name.to_string(), src);
        builder.entry = name;
        builder
    }
    pub fn with_module(mut self, name: &'static str, src: &'static str) -> Self {
        self.sources.insert(name.to_string(), src);
        self
    }
    pub fn assert_runs(self) {
        let res = pipeline(&RunConfig::new(
            "test.steel",
            Source::SourceMap {
                entry: self.entry,
                map: self.sources,
            },
            Mode::Run,
            true,
        ));
        assert_eq!(res.result, RunResult::Ok)
    }
}

/// Execute source and verify a global variable has expected value
pub fn assert_global(source: &str, global_index: usize, expected: Value) {
    let res = pipeline(&RunConfig {
        file_name: "main",
        source: Source::File {
            name: "main",
            source,
        },
        mode: Mode::Compile,
        debug: false,
        include_prelude: false,
        diagnostics: false,
        color: ColorChoice::Auto,
        emit: vec![],
        error_limit: None,
    });
    let mut program = res.program.expect("Expected file to compile.");

    let mut vm = VM::new(program.global_count, &mut program.gc);
    vm.run(program.func).expect("VM execution failed");

    assert_eq!(
        vm.globals[global_index], expected,
        "Global at index {} does not match expected value",
        global_index
    );
}

/// Execute source and verify a global variable holds a string with the given content.
pub fn assert_global_string(source: &str, global_index: usize, expected: &str) {
    let res = pipeline(&RunConfig {
        file_name: "main",
        source: Source::File {
            name: "main",
            source,
        },
        mode: Mode::Compile,
        debug: false,
        include_prelude: false,
        diagnostics: false,
        color: ColorChoice::Auto,
        emit: vec![],
        error_limit: None,
    });
    let mut program = res.program.expect("Expected file to compile.");

    let mut vm = VM::new(program.global_count, &mut program.gc);
    vm.run(program.func).expect("VM execution failed");

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
    let scanner = Scanner::new(source, FileId(0));
    let mut parser = Parser::new(scanner);
    assert!(
        parser.parse().is_err(),
        "Expected a parse error but parsing succeeded"
    );
}

/// Execute source and verify a runtime error occurs.
/// Does not check the error message - just that an error happened.
pub fn assert_panics(source: &str) {
    let res = pipeline(&RunConfig::new(
        "test.steel",
        Source::File {
            name: "test.steel",
            source,
        },
        Mode::Run,
        false,
    ));
    assert_eq!(res.result, RunResult::RuntimeError)
}
