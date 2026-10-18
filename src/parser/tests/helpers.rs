//! Assertions for parser tests.
//!
//! Structure is checked through the S-expression printer (`parser::printer`), positions as `line:col`
//! (1-based, columns counted in characters), and spans as the source text they cover.

use crate::parser::Parser;
use crate::parser::error::ParserError;
use crate::parser::printer::{Node, file_node};
use crate::scanner::{FileId, Scanner, Span};

fn parser(src: &str) -> Parser<'_> {
    Parser::new(Scanner::new(src, FileId(0)))
}

fn parse_errors(src: &str) -> Vec<ParserError<'_>> {
    parser(src).parse().err().unwrap_or_default()
}

/// `line:col` of a byte offset, e.g. `1:15`.
fn position(src: &str, offset: usize) -> String {
    let before = &src[..offset.min(src.len())];
    let line = before.matches('\n').count() + 1;
    let col = before.rsplit('\n').next().map_or(0, |l| l.chars().count()) + 1;
    format!("{line}:{col}")
}

fn parse_tree(src: &str) -> Node {
    let mut parser = parser(src);
    match parser.parse() {
        Ok(file) => file_node(&file),
        Err(errors) => {
            let messages: Vec<_> = errors.iter().map(|e| e.message()).collect();
            panic!("expected `{src}` to parse, got: {messages:#?}");
        }
    }
}

fn render_children(node: &Node, spans: bool) -> String {
    node.child_nodes()
        .iter()
        .map(|c| c.compact_print(spans))
        .collect::<Vec<_>>()
        .join(" ")
}

/// The file parses without errors and its imports and statements print as `expected`, space separated.
#[track_caller]
pub fn assert_parses(src: &str, expected: &str) {
    assert_eq!(
        render_children(&parse_tree(src), false),
        expected,
        "source: {src}"
    );
}

/// Like [`assert_parses`] but every node is followed by `@start..end`.
#[track_caller]
pub fn assert_parses_with_spans(src: &str, expected: &str) {
    assert_eq!(
        render_children(&parse_tree(src), true),
        expected,
        "source: {src}"
    );
}

/// `expr` parses as a single expression with the given structure.
#[track_caller]
pub fn assert_expr(expr: &str, expected: &str) {
    let src = format!("let _e = {expr};");
    let tree = parse_tree(&src);
    let value = tree.child_nodes()[0]
        .child_nodes()
        .last()
        .expect("let has a value");
    assert_eq!(value.compact_print(false), expected, "expression: {expr}");
}

/// The file parses without errors.
#[track_caller]
pub fn assert_parses_ok(src: &str) {
    parse_tree(src);
}

/// Every reported error starts at the given `line:col`, in the order reported, and there are no others.
#[track_caller]
pub fn assert_parse_errors(src: &str, positions: &[&str]) {
    let errors = parse_errors(src);
    let found: Vec<_> = errors
        .iter()
        .map(|e| position(src, e.span().start))
        .collect();
    assert_eq!(
        found,
        positions,
        "error positions for `{src}`; messages: {:#?}",
        errors.iter().map(|e| e.message()).collect::<Vec<_>>()
    );
}

/// The file produces exactly `count` errors.
#[track_caller]
pub fn assert_error_count(src: &str, count: usize) {
    let errors = parse_errors(src);
    assert_eq!(
        errors.len(),
        count,
        "error count for `{src}`; messages: {:#?}",
        errors.iter().map(|e| e.message()).collect::<Vec<_>>()
    );
}

/// At least one error message contains `fragment`. Messages are going to be reworded, so prefer positions.
#[track_caller]
pub fn assert_error_contains(src: &str, fragment: &str) {
    let messages: Vec<_> = parse_errors(src).iter().map(|e| e.message()).collect();
    assert!(
        messages.iter().any(|m| m.contains(fragment)),
        "no error containing `{fragment}` for `{src}`; messages: {messages:#?}"
    );
}

/// The node found by following `path` (see [`Node::find_path`]) covers exactly `expected` in the source.
/// Paths select direct children only: `let/binary/num[1]` is the second `num` of the first `binary` of the first `let`.
#[track_caller]
pub fn assert_span(src: &str, path: &str, expected: &str) {
    let tree = parse_tree(src);
    let node = tree
        .find_path(path)
        .unwrap_or_else(|| panic!("no node at `{path}` in {}", tree.compact_print(true)));
    let span: Span = node
        .span()
        .unwrap_or_else(|| panic!("node `{path}` has no span"));
    assert_eq!(
        &src[span.start..span.end],
        expected,
        "span of `{path}` in `{src}`"
    );
}

/// `path` selects nothing.
#[track_caller]
pub fn assert_no_node(src: &str, path: &str) {
    let tree = parse_tree(src);
    if let Some(node) = tree.find_path(path) {
        panic!("unexpected node at `{path}`: {}", node.compact_print(true));
    }
}
