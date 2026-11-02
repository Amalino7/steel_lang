//! Stored spans: every node covers its full source text, keywords and delimiters included, and the
//! declarations' header spans stop at the signature.

use super::helpers::*;
use crate::parser::ast::Stmt;

#[test]
fn test_postfix_spans_run_to_their_last_token() {
    assert_span("let x = a!;", "let/unwrap", "a!");
    assert_span("let x = a?[0];", "let/index?", "a?[0]");
    assert_span("let x = a.b.c;", "let/field/field", "a.b");
    assert_span("let x = List.<number>;", "let/specialize", "List.<number>");
    assert_span("let x = a is B;", "let/is", "a is B");
}

#[test]
fn test_prefix_spans_start_at_their_keyword() {
    assert_span("let x = -a;", "let/unary", "-a");
    assert_span("let x = try f();", "let/try", "try f()");
    assert_span(
        "func f(): number { return 1 + 2; }",
        "func/body/return",
        "return 1 + 2",
    );
    assert_span("func f() { return; }", "func/body/return", "return");
}

#[test]
fn test_block_like_spans_include_their_braces() {
    let src = "let x = if a { 1 } else { 2 };";
    assert_span(src, "let/if", "if a { 1 } else { 2 }");
    assert_span(src, "let/if/block[1]", "{ 2 }");
    let src = "let x = match v { .A => 1, _ => 2 };";
    assert_span(src, "let/match", "match v { .A => 1, _ => 2 }");
    assert_span(src, "let/match/arm", ".A => 1");
    assert_span("let f = |a: number| a;", "let/lambda", "|a: number| a");
    assert_span("let f = |a: number| a;", "let/lambda/param", "a: number");
}

#[test]
fn test_collection_spans_include_their_delimiters() {
    assert_span("let x = [:];", "let/map", "[:]");
    assert_span("let x = [1: 2];", "let/map", "[1: 2]");
    assert_span("let x = [1: 2];", "let/map/entry", "1: 2");
    assert_span("let x = f(a: 1);", "let/call/arg", "a: 1");
}

#[test]
fn test_string_interpolation_parts_have_spans() {
    let src = r#"let s = "a${x}bc";"#;
    assert_span(src, "let/interp", r#""a${x}bc""#);
    assert_span(src, "let/interp/text", "a");
    assert_span(src, "let/interp/text[1]", "bc");
}

#[test]
fn test_type_spans_cover_their_syntax() {
    assert_span("let x: List<T>? = a;", "let/optional", "List<T>?");
    assert_span("let x: List<T>? = a;", "let/optional/type", "List<T>");
    assert_span("let x: (a, b) = c;", "let/tuple-type", "(a, b)");
    assert_span("let f: func(a): b = c;", "let/fn-type", "func(a): b");
    assert_span("let f: func?(a): b = c;", "let/optional", "func?(a): b");
}

#[test]
fn test_binding_spans_include_their_delimiters() {
    assert_span("let (a, b) = c;", "let/bind-tuple", "(a, b)");
    assert_span("let P(x: a, :y) = p;", "let/bind-struct", "P(x: a, :y)");
    assert_span(
        "let P(x: a, :y) = p;",
        "let/bind-struct/bind-field[1]",
        ":y",
    );
}

#[test]
fn test_declaration_parts_have_spans() {
    let src = "public struct P<T> { public x: T }";
    assert_span(src, "struct/generics", "<T>");
    assert_span(src, "struct/field", "public x: T");
    assert_span("enum E { A(number), B }", "enum/variant", "A(number)");
    assert_span("while a { }", "while/body", "{ }");
    assert_span("impl P: A<T> {}", "impl/interfaces/type", "A<T>");
    assert_span(
        "interface I { func f(self); }",
        "interface/method-sig",
        "func f(self);",
    );
    assert_span("impl P { func f(self) {} }", "impl/func/self", "self");
    assert_span("extern func f();", "extern-func", "extern func f();");
}

#[test]
fn test_wildcards_bind_nothing() {
    assert_parses("let _ = 1;", "(let _ ? 1)");
    assert_parses("let (_, b) = c;", "(let (bind-tuple _ (bind b)) ? c)");
    assert_expr(
        "match v { .A(_) => 1, _ => 2 }",
        "(match v (arm (pat .A _) 1) (arm _ 2))",
    );
}

/// The source text of `span` in `src`.
fn text(src: &str, span: crate::scanner::Span) -> &str {
    &src[span.to_range()]
}

#[test]
fn test_header_spans_stop_at_the_signature() {
    let src = "public func max<T>(a: T): T { return a; } \
               struct P<T> { x: T } \
               public enum E { A } \
               interface I<T> { func f(self); } \
               impl<T> P<T>: I<T>, J { }";
    let file = parse_file(src);
    let headers: Vec<_> = file
        .stmts
        .iter()
        .map(|stmt| match stmt {
            Stmt::Fn(decl) => decl.header_span(),
            Stmt::Struct(decl) => decl.header_span(),
            Stmt::Enum(decl) => decl.header_span(),
            Stmt::Interface(decl) => decl.header_span(),
            Stmt::Impl(decl) => decl.header_span(),
            other => panic!("unexpected statement {other:?}"),
        })
        .map(|span| text(src, span))
        .collect();
    assert_eq!(
        headers,
        [
            "public func max<T>(a: T): T",
            "struct P<T>",
            "public enum E",
            "interface I<T>",
            "impl<T> P<T>: I<T>, J",
        ]
    );
}

#[test]
fn test_function_header_without_return_type_ends_at_the_paren() {
    let src = "extern func f(a: number);";
    let file = parse_file(src);
    let Stmt::Fn(decl) = &file.stmts[0] else {
        panic!("expected a function");
    };
    assert_eq!(text(src, decl.header_span()), "extern func f(a: number)");
    assert_eq!(text(src, decl.sig.params.span()), "(a: number)");
}

#[test]
fn test_members_are_names_or_tuple_indices() {
    assert_span("let x = a.b;", "let/field/name", "b");
    assert_span("let x = a.12;", "let/field/tuple-index", "12");
    assert_span("a?.0 = 1;", "set-field?/tuple-index", "0");
}
