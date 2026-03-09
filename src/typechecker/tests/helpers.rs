pub(super) use self::TypeCheckerTest as Tester;
use crate::parser::Parser;
use crate::scanner::Scanner;
pub(super) use crate::typechecker::core::error::{TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::TypeChecker;

pub(super) struct TypeCheckerTest<'src> {
    source: &'src str,
    error_matchers: Vec<fn(&TypeCheckerError) -> bool>,
    warning_matchers: Vec<fn(&TypeCheckerWarning) -> bool>,
}

impl<'src> TypeCheckerTest<'src> {
    #[must_use = "Test builder must be run to execute the test"]
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            error_matchers: Vec::new(),
            warning_matchers: Vec::new(),
        }
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
        let scanner = Scanner::new(self.source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().expect("Parser failed");
        let mut checker = TypeChecker::new();

        match checker.check(ast.as_mut_slice()) {
            Ok((_, warnings)) => {
                self.verify_errors(Vec::new());
                self.verify_warnings(warnings);
            }
            Err(errors) => {
                self.verify_errors(errors);
                self.verify_warnings(checker.warnings);
            }
        }
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
