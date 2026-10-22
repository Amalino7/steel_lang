//! S-expression view of the parsed AST, used by `--emit ast` and by the parser tests.
//!
//! The AST is first lowered to a [`Node`] tree and then rendered. The tree keeps the spans as the AST reports them,
//! so tests can assert on structure, precedence and spans without depending on `Debug` output. It is a debugging
//! format, not a source printer: it does not round-trip.
//!
//! # Format
//!
//! A node is either an *atom* or a *list*. Atoms print as their text: identifiers, literals (`1`, `"a"`, `true`,
//! `nil`), and the modifier `public`. Lists print as `(head child…)` with children in source order. The head is the
//! node's kind (`let`, `call`, `if`), except for operator nodes, which print their operator: `(+ a b)`, `(! x)`,
//! `(and a b)`. The kind is kept either way, and is what test paths select on.
//!
//! Positions give atoms their meaning: bare identifiers in expression position are variable references, and
//! every binding site is tagged (`(bind x)`, `(param x T)`, `(field x T)`), so the two never look alike. Types are
//! always tagged (`(type number)`) because a bare type name would read as a variable. `?` stands for an omitted
//! type annotation. A field being read or written prints as `.field`. Names that can be confused with a plain identifier are kept as lists: `(void)`, `(text "a")`
//! for the literal parts of an interpolated string.

use crate::parser::ast::*;
use crate::scanner::{Span, Token};
use std::fmt::Write;

/// Lines longer than this are broken up by [`Node::pretty_print`].
const PRETTY_WIDTH: usize = 80;

/// One node of the printed tree.
#[derive(Debug, Clone)]
pub enum Node {
    Atom {
        kind: &'static str,
        text: String,
        span: Option<Span>,
    },
    List {
        kind: &'static str,
        /// Printed in place of `kind` when set (operators).
        head: Option<String>,
        span: Option<Span>,
        children: Vec<Node>,
    },
}

impl Node {
    fn atom(kind: &'static str, text: impl Into<String>, span: impl Into<Option<Span>>) -> Self {
        Node::Atom {
            kind,
            text: text.into(),
            span: span.into(),
        }
    }

    fn list(kind: &'static str, span: impl Into<Option<Span>>) -> Self {
        Node::List {
            kind,
            head: None,
            span: span.into(),
            children: vec![],
        }
    }

    /// An identifier that names something being declared or accessed (`name` atoms).
    fn name(token: &Token) -> Self {
        Node::atom("name", token.lexeme, token.span)
    }

    /// The `public` modifier atom, when present.
    fn public(is_public: bool) -> Option<Self> {
        is_public.then(|| Node::atom("public", "public", None))
    }

    fn head(mut self, text: impl Into<String>) -> Self {
        if let Node::List { head, .. } = &mut self {
            *head = Some(text.into());
        }
        self
    }

    fn child(mut self, child: Node) -> Self {
        self.children_mut().push(child);
        self
    }

    fn children(mut self, children: impl IntoIterator<Item = Node>) -> Self {
        self.children_mut().extend(children);
        self
    }

    fn children_mut(&mut self) -> &mut Vec<Node> {
        match self {
            Node::List { children, .. } => children,
            Node::Atom { .. } => unreachable!("atoms have no children"),
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Node::Atom { kind, .. } | Node::List { kind, .. } => kind,
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            Node::Atom { span, .. } | Node::List { span, .. } => *span,
        }
    }

    pub fn child_nodes(&self) -> &[Node] {
        match self {
            Node::List { children, .. } => children,
            Node::Atom { .. } => &[],
        }
    }

    /// Whether a path segment names this node: by kind, or for lists by their printed head.
    fn is_named(&self, name: &str) -> bool {
        name == "*"
            || self.kind() == name
            || matches!(self, Node::List { head: Some(h), .. } if h == name)
    }

    fn write_span(out: &mut String, span: Option<Span>, spans: bool) {
        if let (true, Some(span)) = (spans, span) {
            let _ = write!(out, "@{}..{}", span.start, span.end);
        }
    }

    /// Single-line rendering: `(+ 1 2)`. With `spans`, every node is followed by `@start..end`.
    pub fn compact_print(&self, spans: bool) -> String {
        let mut out = String::new();
        self.write_compact(&mut out, spans);
        out
    }

    fn write_compact(&self, out: &mut String, spans: bool) {
        match self {
            Node::Atom { text, span, .. } => {
                out.push_str(text);
                Self::write_span(out, *span, spans);
            }
            Node::List { children, .. } => {
                out.push('(');
                out.push_str(&self.head_text(spans));
                for child in children {
                    out.push(' ');
                    child.write_compact(out, spans);
                }
                out.push(')');
            }
        }
    }

    fn head_text(&self, spans: bool) -> String {
        match self {
            Node::List {
                kind, head, span, ..
            } => {
                let mut text = head.clone().unwrap_or_else(|| kind.to_string());
                Self::write_span(&mut text, *span, spans);
                text
            }
            Node::Atom { text, .. } => text.clone(),
        }
    }

    /// Indented rendering: a list stays on one line when it fits, otherwise each child gets its own line.
    pub fn pretty_print(&self, spans: bool) -> String {
        let mut out = String::new();
        self.write_pretty(&mut out, spans, 0);
        out
    }

    fn write_pretty(&self, out: &mut String, spans: bool, depth: usize) {
        let compact = self.compact_print(spans);
        let indent = "  ".repeat(depth);
        if self.child_nodes().is_empty() || indent.len() + compact.len() <= PRETTY_WIDTH {
            let _ = writeln!(out, "{indent}{compact}");
            return;
        }
        // Leading atoms (names, modifiers) stay on the head line; the rest of the children are indented below.
        let (atoms, rest) = self.child_nodes().split_at(
            self.child_nodes()
                .iter()
                .take_while(|c| matches!(c, Node::Atom { .. }))
                .count(),
        );
        let _ = write!(out, "{indent}({}", self.head_text(spans));
        for atom in atoms {
            let _ = write!(out, " {}", atom.compact_print(spans));
        }
        let _ = writeln!(out);
        for child in rest {
            child.write_pretty(out, spans, depth + 1);
        }
        let _ = writeln!(out, "{indent})");
    }

    /// Follows a `/`-separated path of direct children. A segment is a kind (`call`), a printed head (`+`) or `*` for
    /// any node, optionally followed by `[n]` to take the n-th (0-based) sibling that matches instead of the first:
    /// `"let/call/arg[1]"` is the second `arg` of the first `call` of the first `let`.
    pub fn find_path(&self, path: &str) -> Option<&Node> {
        path.split('/').try_fold(self, |node, segment| {
            let (name, index) = match segment.split_once('[') {
                Some((name, rest)) => (name, rest.strip_suffix(']')?.parse().ok()?),
                None => (segment, 0),
            };
            node.child_nodes()
                .iter()
                .filter(|child| child.is_named(name))
                .nth(index)
        })
    }
}

/// Lowers a whole file: `(file (import …)… stmt…)`.
pub fn file_node(file: &ParsedFile) -> Node {
    Node::list("file", None)
        .children(file.imports.iter().map(import_node))
        .children(file.body.iter().map(stmt_node))
}

pub fn file_to_string(file: &ParsedFile, spans: bool) -> String {
    file_node(file).pretty_print(spans)
}

pub fn import_node(import: &ImportStmt) -> Node {
    fn segment(seg: &ImportSegment) -> String {
        let mut s = String::new();
        for part in &seg.path {
            let _ = write!(s, "{}/", part.lexeme);
        }
        match &seg.import_type {
            ImportType::Simple { terminator } => s.push_str(terminator.lexeme),
            ImportType::Alias { terminator, alias } => {
                let _ = write!(s, "{} as {}", terminator.lexeme, alias.lexeme);
            }
            ImportType::Group { options } => {
                let parts: Vec<_> = options.iter().map(segment).collect();
                let _ = write!(s, "{{{}}}", parts.join(", "));
            }
            ImportType::Wildcard(_) => s.push('*'),
        }
        s
    }
    Node::list("import", import.span()).child(Node::atom("path", segment(&import.segment), None))
}

fn type_node(ty: &TypeAst) -> Node {
    match ty {
        TypeAst::Optional(inner) => Node::list("optional", ty.span()).child(type_node(inner)),
        TypeAst::Named(name, args) => Node::list("type", ty.span())
            .child(Node::name(name))
            .children(args.iter().map(type_node)),
        TypeAst::Function(_, sig) => fn_type_node(ty.span(), sig),
        TypeAst::Tuple(types) => {
            Node::list("tuple-type", ty.span()).children(types.iter().map(type_node))
        }
        // `Infer` has no source text, so it has no span to show.
        TypeAst::Infer => Node::atom("infer", "?", None),
    }
}

fn fn_type_node(span: Span, sig: &FunctionSig) -> Node {
    Node::list("fn-type", span)
        .children(sig.params.iter().map(|(_, ty)| type_node(ty)))
        .child(ret_node(&sig.return_type))
}

fn ret_node(ty: &TypeAst) -> Node {
    Node::list("ret", None).child(type_node(ty))
}

fn param_node(name: &Token, ty: &TypeAst) -> Node {
    Node::list("param", name.span)
        .child(Node::name(name))
        .child(type_node(ty))
}

fn generics_node(generics: &[Token]) -> Option<Node> {
    let (first, last) = (generics.first()?, generics.last()?);
    Some(
        Node::list("generics", first.span.merge(last.span))
            .children(generics.iter().map(Node::name)),
    )
}

fn sig_nodes(sig: &FunctionSig) -> Vec<Node> {
    let mut nodes: Vec<Node> = sig.params.iter().map(|(n, t)| param_node(n, t)).collect();
    nodes.push(ret_node(&sig.return_type));
    nodes
}

fn field_node(field: &StructField) -> Node {
    Node::list("field", field.name.span)
        .children(Node::public(field.is_public))
        .child(Node::name(&field.name))
        .child(type_node(&field.type_info))
}

fn binding_node(binding: &Binding) -> Node {
    match binding {
        Binding::Variable(name) => Node::list("bind", name.span).child(Node::name(name)),
        Binding::Tuple { fields } => {
            Node::list("bind-tuple", binding.span()).children(fields.iter().map(binding_node))
        }
        Binding::Struct { name, fields } => Node::list("bind-struct", binding.span())
            .child(Node::name(name))
            .children(fields.iter().map(|(field, inner)| {
                Node::list("bind-field", field.span.merge(inner.span()))
                    .child(Node::name(field))
                    .child(binding_node(inner))
            })),
    }
}

fn pattern_node(pattern: &Pattern) -> Node {
    match pattern {
        Pattern::Variable(name) => Node::list("pat-var", name.span).child(Node::name(name)),
        Pattern::Named {
            enum_name,
            variant_name,
            bind,
        } => {
            let (text, span) = match enum_name {
                Some(e) => (
                    format!("{}.{}", e.lexeme, variant_name.lexeme),
                    e.span.merge(variant_name.span),
                ),
                None => (format!(".{}", variant_name.lexeme), variant_name.span),
            };
            Node::list("pat", span)
                .child(Node::atom("name", text, span))
                .children(bind.iter().map(binding_node))
        }
    }
}

fn stmt_node(stmt: &Stmt) -> Node {
    let span = stmt.span();
    match stmt {
        Stmt::Expression(e) => expr_node(e),
        Stmt::Let {
            is_public,
            binding,
            value,
            type_info,
        } => Node::list("let", span)
            .children(Node::public(*is_public))
            .child(binding_node(binding))
            .child(type_node(type_info))
            .child(expr_node(value)),
        Stmt::Block { body, .. } => {
            Node::list("block-stmt", span).children(body.iter().map(stmt_node))
        }
        Stmt::While { condition, body } => Node::list("while", span)
            .child(expr_node(condition))
            .child(stmt_node(body)),
        Stmt::Function {
            is_public,
            name,
            body,
            generics,
            signature,
        } => Node::list("func", span)
            .children(Node::public(*is_public))
            .child(Node::name(name))
            .children(generics_node(generics))
            .children(sig_nodes(signature))
            .child(stmt_node(body)),
        Stmt::ExternFunction {
            is_public,
            name,
            generics,
            signature,
        } => Node::list("extern-func", span)
            .children(Node::public(*is_public))
            .child(Node::name(name))
            .children(generics_node(generics))
            .children(sig_nodes(signature)),
        Stmt::Struct {
            is_public,
            name,
            fields,
            generics,
        } => Node::list("struct", span)
            .children(Node::public(*is_public))
            .child(Node::name(name))
            .children(generics_node(generics))
            .children(fields.iter().map(field_node)),
        Stmt::Impl {
            interfaces,
            name,
            methods,
            generics,
        } => {
            let (target, args) = name;
            let target_span = args
                .last()
                .map_or(target.span, |a| target.span.merge(a.span()));
            Node::list("impl", span)
                .children(generics_node(generics))
                .child(
                    Node::list("type", target_span)
                        .child(Node::name(target))
                        .children(args.iter().map(type_node)),
                )
                .children((!interfaces.is_empty()).then(|| {
                    Node::list("interfaces", None).children(interfaces.iter().map(Node::name))
                }))
                .children(methods.iter().map(stmt_node))
        }
        Stmt::Interface {
            is_public,
            name,
            methods,
            generics,
        } => Node::list("interface", span)
            .children(Node::public(*is_public))
            .child(Node::name(name))
            .children(generics_node(generics))
            .children(methods.iter().map(|m| {
                Node::list("method-sig", m.name.span)
                    .child(Node::name(&m.name))
                    .children(generics_node(&m.generics))
                    .children(sig_nodes(&m.signature))
            })),
        Stmt::Enum {
            is_public,
            name,
            variants,
            generics,
        } => Node::list("enum", span)
            .children(Node::public(*is_public))
            .child(Node::name(name))
            .children(generics_node(generics))
            .children(variants.iter().map(|(variant, kind)| {
                let node = Node::list("variant", variant.span).child(Node::name(variant));
                match kind {
                    VariantType::Unit => node,
                    VariantType::Tuple(types) => node.children(types.iter().map(type_node)),
                    VariantType::Struct(fields) => node.children(fields.iter().map(field_node)),
                }
            })),
    }
}

/// A field being read or written prints as `.field`, so it is not mistaken for a variable next to the object.
fn field_access(field: &Token) -> Node {
    Node::atom("name", format!(".{}", field.lexeme), field.span)
}

/// Unlabeled arguments are just their expression; labeled ones print as `(label: expr)`.
fn arg_node(arg: &CallArg) -> Node {
    match &arg.label {
        Some(label) => Node::list("arg", arg.expr.span())
            .head(format!("{}:", label.lexeme))
            .child(expr_node(&arg.expr)),
        None => expr_node(&arg.expr),
    }
}

fn expr_node(expr: &Expr) -> Node {
    let span = expr.span();
    let safe_kind = |safe: bool, plain: &'static str, safe_name: &'static str| {
        if safe { safe_name } else { plain }
    };
    match expr {
        Expr::Error(span) => Node::list("error", *span),
        Expr::Unary {
            operator,
            expression,
        } => Node::list("unary", span)
            .head(operator.lexeme)
            .child(expr_node(expression)),
        Expr::Binary {
            operator,
            left,
            right,
        } => Node::list("binary", span)
            .head(operator.lexeme)
            .child(expr_node(left))
            .child(expr_node(right)),
        Expr::Logical {
            left,
            operator,
            right,
        } => Node::list("logical", span)
            .head(operator.lexeme)
            .child(expr_node(left))
            .child(expr_node(right)),
        Expr::Variable { name } => Node::atom("var", name.lexeme, span),
        Expr::Grouping { expression } => Node::list("group", span).child(expr_node(expression)),
        Expr::Literal { literal, .. } => match literal {
            Literal::Number(n) => Node::atom("num", n.to_string(), span),
            Literal::String(s) => Node::atom("str", format!("{s:?}"), span),
            Literal::Boolean(b) => Node::atom("bool", b.to_string(), span),
            Literal::Nil => Node::atom("nil", "nil", span),
            // `void` is an ordinary identifier, so the value of a bare `return;` stays a list.
            Literal::Void => Node::list("void", span),
        },
        Expr::Is {
            expression,
            type_name,
        } => Node::list("is", span)
            .child(expr_node(expression))
            .child(Node::name(type_name)),
        Expr::Assignment { identifier, value } => Node::list("assign", span)
            .child(Node::name(identifier))
            .child(expr_node(value)),
        Expr::TypeSpecialization { callee, generics } => Node::list("specialize", span)
            .child(expr_node(callee))
            .children(generics.iter().map(type_node)),
        Expr::Call {
            callee,
            arguments,
            safe,
        } => Node::list(safe_kind(*safe, "call", "call?"), span)
            .child(expr_node(callee))
            .children(arguments.iter().map(arg_node)),
        Expr::Get {
            safe,
            object,
            field,
        } => Node::list(safe_kind(*safe, "get", "get?"), span)
            .child(expr_node(object))
            .child(field_access(field)),
        Expr::Set {
            safe,
            object,
            field,
            value,
        } => Node::list(safe_kind(*safe, "set", "set?"), span)
            .child(expr_node(object))
            .child(field_access(field))
            .child(expr_node(value)),
        Expr::Tuple { elements } => {
            Node::list("tuple", span).children(elements.iter().map(expr_node))
        }
        Expr::List { elements, .. } => {
            Node::list("list", span).children(elements.iter().map(expr_node))
        }
        Expr::Map { kv_pairs, .. } => {
            Node::list("map", span).children(kv_pairs.iter().map(|(k, v)| {
                Node::list("entry", k.span().merge(v.span()))
                    .child(expr_node(k))
                    .child(expr_node(v))
            }))
        }
        Expr::GetIndex {
            safe,
            object,
            index,
        } => Node::list(safe_kind(*safe, "index", "index?"), span)
            .child(expr_node(object))
            .child(expr_node(index)),
        Expr::SetIndex {
            safe,
            object,
            index,
            value,
        } => Node::list(safe_kind(*safe, "set-index", "set-index?"), span)
            .child(expr_node(object))
            .child(expr_node(index))
            .child(expr_node(value)),
        Expr::ForceUnwrap { expression, .. } => {
            Node::list("unwrap", span).child(expr_node(expression))
        }
        // Literal parts stay wrapped: a bare `"a"` would read as a string literal inside the interpolation.
        Expr::StringInterp { parts, .. } => {
            Node::list("interp", span).children(parts.iter().map(|part| match part {
                StringPart::Literal(s) => {
                    Node::list("text", None).child(Node::atom("str", format!("{s:?}"), None))
                }
                StringPart::Expr(e) => expr_node(e),
            }))
        }
        Expr::Block { body, tail, .. } => Node::list("block", span)
            .children(body.iter().map(stmt_node))
            .children(
                tail.iter()
                    .map(|t| Node::list("tail", t.span()).child(expr_node(t))),
            ),
        Expr::IfExpr {
            condition,
            then_branch,
            else_branch,
        } => Node::list("if", span)
            .child(expr_node(condition))
            .child(expr_node(then_branch))
            .children(else_branch.iter().map(|e| expr_node(e))),
        Expr::MatchExpr { value, arms } => Node::list("match", span)
            .child(expr_node(value))
            .children(arms.iter().map(|arm| {
                Node::list("arm", None)
                    .child(pattern_node(&arm.pattern))
                    .child(expr_node(&arm.body))
            })),
        Expr::Lambda {
            params,
            return_type,
            body,
            ..
        } => Node::list("lambda", span)
            .children(params.iter().map(|(n, t)| param_node(n, t)))
            .children(return_type.iter().map(ret_node))
            .child(expr_node(body)),
        Expr::Return { value, .. } => Node::list("return", span).child(expr_node(value)),
    }
}
