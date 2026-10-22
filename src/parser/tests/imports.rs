use super::helpers::*;
use crate::parser::Parser;
use crate::parser::printer::import_node;
use crate::scanner::{FileId, Scanner};

#[test]
fn test_valid_imports_parse() {
    for source in [
        "import a/{b, c,};",
        "import a/b;",
        "import a/b/c as d;",
        "import a/{*};",
        "import a/{b, c/d, e as f, g/{*}};",
    ] {
        assert_parses_ok(source);
    }
}

#[test]
fn test_import_shapes() {
    assert_parses("import a/b;", "(import a/b)");
    assert_parses("import a/b/c as d;", "(import a/b/c as d)");
    assert_parses("import a/{*};", "(import a/*)");
    assert_parses("import a/{b, c/d};", "(import a/{b, c/d})");
}

#[test]
fn test_imports_are_returned_separately_from_body() {
    let mut parser = Parser::new(Scanner::new("import a/b; let x = 1; let y = 2;", FileId(0)));
    let file = parser.parse().expect("valid file");
    assert_eq!(file.imports.len(), 1);
    assert_eq!(file.body.len(), 2);
}

#[test]
fn test_header_imports_survive_malformed_import_and_body_error() {
    let source = "import a/b; import ; import d/e; let x = ;";
    let mut parser = Parser::new(Scanner::new(source, FileId(0)));
    let names: Vec<_> = parser
        .parse_imports()
        .iter()
        .map(|i| import_node(i).compact_print(false))
        .collect();
    assert_eq!(names, ["(import a/b)", "(import d/e)"]);
    // the malformed import is reported, and the full parse also reports the body error
    assert_error_count(source, 2);
}

#[test]
fn test_import_after_declaration_is_error() {
    assert_error_count("let x = 1; import a/b; let y = 2;", 1);
}

#[test]
fn test_nested_import_is_error() {
    assert_error_count("func f(): void { import a/b; } let y = 2;", 1);
}

#[test]
fn test_public_import_is_error() {
    assert_error_count("public import a/b; let y = 2;", 1);
}

#[test]
fn test_late_public_import_is_one_error() {
    assert_error_count("let x = 1; public import a/b; let y = 2;", 1);
}

#[test]
fn test_import_without_module_is_error() {
    assert_error_count("import foo; let y = 2;", 1);
    assert_error_count("import foo as f; let y = 2;", 1);
}

#[test]
fn test_import_trailing_slash_is_error() {
    assert_error_count("import a/; let y = 2;", 1);
    assert_error_count("import a/{b/};", 1);
}

#[test]
fn test_import_empty_group_is_error() {
    assert_error_count("import a/{}; let y = 2;", 1);
}

#[test]
fn test_import_group_requires_commas() {
    assert_error_count("import a/{b c};", 1);
}

#[test]
fn test_import_group_needs_slash() {
    assert_error_count("import a{b};", 1);
}

#[test]
fn test_non_ascii_import() {
    assert_parses_ok("import a/dεfrœ;");
    // The unexpected symbol is reported and dropped, which leaves `a//b`: a second error.
    assert_error_count("import a/😎/b; let y = 2;", 2);
    assert_error_count("import a/œaʃðæ/b/; let y = 2;", 1);
}
