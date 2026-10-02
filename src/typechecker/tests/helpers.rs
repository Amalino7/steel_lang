pub(super) use self::TypeCheckerTest as Tester;
use crate::parser::Parser;
use crate::resolver::ModuleResolver;
use crate::scanner::Scanner;
pub(super) use crate::typechecker::core::error::{TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::system::TypeSystem;
use crate::typechecker::{GlobalIdGenerator, TypeChecker};
use std::collections::HashMap;

pub(super) struct TypeCheckerTest<'src> {
    sources: HashMap<String, &'src str>,
    entry: &'src str,
    error_matchers: Vec<fn(&TypeCheckerError) -> bool>,
    warning_matchers: Vec<fn(&TypeCheckerWarning) -> bool>,
}

impl<'src> TypeCheckerTest<'src> {
    #[must_use = "Test builder must be run to execute the test"]
    pub fn new(source: &'src str) -> Self {
        let mut builder = Self {
            sources: Default::default(),
            entry: "test",
            error_matchers: vec![],
            warning_matchers: vec![],
        };
        builder.sources.insert("test".to_string(), source);
        builder
    }
    #[must_use = "Test builder must be run to execute the test"]
    pub fn with_module(mut self, name: &'src str, source: &'src str) -> Self {
        self.sources.insert(name.to_string(), source);
        self
    }
    #[must_use = "Test builder must be run to execute the test"]
    pub fn expect_error(mut self, matcher: fn(&TypeCheckerError) -> bool) -> Self {
        self.error_matchers.push(matcher);
        self
    }
    #[must_use = "Test builder must be run to execute the test"]
    pub fn expect_warning(mut self, matcher: fn(&TypeCheckerWarning) -> bool) -> Self {
        self.warning_matchers.push(matcher);
        self
    }

    pub fn run(self) {
        let mut resolver = ModuleResolver::new();
        assert!(resolver.errors.is_empty(), "Resolution should pass.");
        let mut graph = resolver.resolve_mock(self.entry.to_string(), &self.sources);

        let mut warnings = vec![];
        let mut errors = vec![];
        let mut sys = TypeSystem::new();
        let id_generator = GlobalIdGenerator::new();

        for id in 0..graph.modules.len() {
            let src = &graph.modules[id].source;
            let file_id = graph.modules[id].file_id;

            let scanner = Scanner::new(src, file_id);
            let mut parser = Parser::new(scanner);
            let mut ast = parser.parse().expect("Parser failed");

            let mut checker = TypeChecker::new(&[], &mut sys, &id_generator, &graph, file_id);
            let res = checker.check(ast.as_mut_slice(), None);

            let exports = match res {
                Ok((file, warn)) => {
                    warnings.extend(warn);
                    file.exports
                }
                Err((err, exports)) => {
                    errors.extend(err);
                    warnings.extend(checker.warnings);
                    *exports
                }
            };
            graph.modules[id].exports = exports;
        }

        self.verify_errors(errors);
        self.verify_warnings(warnings);
    }

    fn verify_errors(&self, errors: Vec<TypeCheckerError>) {
        assert_eq!(
            errors.len(),
            self.error_matchers.len(),
            "Error count mismatch.\nExpected: {}\nActual: {}\nFound Errors: {:#?}",
            self.error_matchers.len(),
            errors.len(),
            errors
        );

        for (i, matcher) in self.error_matchers.iter().enumerate() {
            assert!(
                matcher(&errors[i]),
                "Error at index {} did not match expectation.\nFound: {:#?}",
                i,
                errors[i]
            );
        }
    }

    fn verify_warnings(&self, warnings: Vec<TypeCheckerWarning>) {
        assert_eq!(
            warnings.len(),
            self.warning_matchers.len(),
            "Warning count mismatch.\nExpected: {}\nActual: {}\nFound Warnings: {:#?}",
            self.warning_matchers.len(),
            warnings.len(),
            warnings
        );

        for (i, matcher) in self.warning_matchers.iter().enumerate() {
            assert!(
                matcher(&warnings[i]),
                "Warning at index {} did not match expectation.\nFound: {:#?}",
                i,
                warnings[i]
            );
        }
    }
}

/// Helper for the most common success case
pub(super) fn assert_typechecks(source: &str) {
    TypeCheckerTest::new(source).run();
}
