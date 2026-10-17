pub(super) use self::TypeCheckerTest as Tester;
use crate::parser::Parser;
use crate::resolver::ModuleResolver;
use crate::scanner::Scanner;
pub(super) use crate::typechecker::core::error::{TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::system::TypeSystem;
use crate::typechecker::{Exports, GlobalIdGenerator, TypeChecker};
use std::collections::HashMap;

pub(super) struct TypeCheckerTest<'src> {
    sources: HashMap<String, &'src str>,
    entry: &'src str,
    prelude: Option<&'src str>,
    resolver_errors: usize,
    error_matchers: Vec<fn(&TypeCheckerError) -> bool>,
    warning_matchers: Vec<fn(&TypeCheckerWarning) -> bool>,
}

impl<'src> TypeCheckerTest<'src> {
    #[must_use = "Test builder must be run to execute the test"]
    pub fn new(source: &'src str) -> Self {
        let mut builder = Self {
            sources: Default::default(),
            entry: "test",
            prelude: None,
            resolver_errors: 0,
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
    /// Checks the source against `prelude` as the `std` module, imported into the test module like the real one.
    #[must_use = "Test builder must be run to execute the test"]
    pub fn with_prelude(mut self, prelude: &'src str) -> Self {
        self.prelude = Some(prelude);
        self
    }
    /// Expects the resolver to report `count` errors (unavailable modules); the checker still runs.
    #[must_use = "Test builder must be run to execute the test"]
    pub fn expect_resolver_errors(mut self, count: usize) -> Self {
        self.resolver_errors = count;
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
        let mut resolver = match self.prelude {
            Some(prelude) => ModuleResolver::new_with_prelude(prelude),
            None => ModuleResolver::new(),
        };
        let graph = resolver
            .resolve_mock(self.entry.to_string(), &self.sources)
            .unwrap_or_else(|err| panic!("Entry module failed to load: {err}"));
        assert_eq!(
            resolver.errors.len(),
            self.resolver_errors,
            "Resolver error count mismatch.
Found: {:#?}",
            resolver.errors
        );

        let mut warnings = vec![];
        let mut errors = vec![];
        let mut sys = TypeSystem::new();
        let id_generator = GlobalIdGenerator::new();
        let mut surfaces: Vec<Option<Exports>> = vec![];

        for id in 0..graph.modules.len() {
            let src = &graph.modules[id].source;
            let file_id = graph.modules[id].file_id;

            let scanner = Scanner::new(src, file_id);
            let mut parser = Parser::new(scanner);
            let ast = match parser.parse() {
                Ok(ast) => ast,
                Err(errs) => panic!(
                    "Parser failed in module '{}':
{:#?}",
                    graph.modules[id].name, errs
                ),
            };

            let mut checker =
                TypeChecker::new(&[], &mut sys, &id_generator, &graph, &surfaces, file_id);
            let res = checker.check(&ast);

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
            surfaces.push(Some(exports));
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
