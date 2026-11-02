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
//! type annotation, and an omitted return type prints nothing. A field being read or written prints as `.field`.
//! The wildcard `_` and a `self` parameter are atoms. The literal parts of an interpolated string are kept as
//! lists, `(text "a")`, since a bare `"a"` would read as a string literal.

use crate::parser::ast::*;
use crate::scanner::Span;
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
    fn name(ident: &Ident) -> Self {
        Node::atom("name", ident.name, ident.span)
    }

    /// The `public` modifier atom, when present.
    fn public(vis: Visibility) -> Option<Self> {
        match vis {
            Visibility::Public(span) => Some(Node::atom("public", "public", span)),
            Visibility::Private => None,
        }
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
        .children(file.stmts.iter().map(stmt_node))
}

pub fn file_to_string(file: &ParsedFile, spans: bool) -> String {
    file_node(file).pretty_print(spans)
}

pub fn import_node(import: &Import) -> Node {
    fn tree(node: &ImportTree) -> String {
        let mut s = String::new();
        for part in &node.path {
            let _ = write!(s, "{}/", part.name);
        }
        match &node.kind {
            ImportKind::Item { name } => s.push_str(name.name),
            ImportKind::Alias { name, alias } => {
                let _ = write!(s, "{} as {}", name.name, alias.name);
            }
            ImportKind::Group(group) => {
                let parts: Vec<_> = group.items.iter().map(tree).collect();
                let _ = write!(s, "{{{}}}", parts.join(", "));
            }
            ImportKind::Wildcard(_) => s.push('*'),
        }
        s
    }
    Node::list("import", import.span).child(Node::atom("path", tree(&import.tree), None))
}

/// `?` stands for an omitted annotation, which has no source text and so no span.
fn opt_type_node(ty: Option<&TypeAst>) -> Node {
    ty.map_or_else(|| Node::atom("infer", "?", None), type_node)
}

fn type_node(ty: &TypeAst) -> Node {
    match &ty.kind {
        TypeAstKind::Optional(inner) => Node::list("optional", ty.span).child(type_node(inner)),
        TypeAstKind::Named(named) => named_type_node(named),
        TypeAstKind::Fn { params, ret } => Node::list("fn-type", ty.span)
            .children(params.iter().map(type_node))
            .children(ret.as_deref().map(ret_node)),
        TypeAstKind::Tuple(types) => {
            Node::list("tuple-type", ty.span).children(types.iter().map(type_node))
        }
    }
}

fn named_type_node(named: &NamedType) -> Node {
    Node::list("type", named.span)
        .child(Node::name(&named.name))
        .children(named.args.iter().map(type_node))
}

fn ret_node(ty: &TypeAst) -> Node {
    Node::list("ret", ty.span).child(type_node(ty))
}

fn param_node(param: &Param) -> Node {
    match param {
        Param::SelfParam(span) => Node::atom("self", "self", *span),
        Param::Named { name, ty } => Node::list("param", param.span())
            .child(Node::name(name))
            .child(type_node(ty)),
    }
}

fn generics_node(generics: &Generics) -> Option<Node> {
    let list = generics.0.as_ref()?;
    Some(Node::list("generics", list.span()).children(list.items.iter().map(Node::name)))
}

fn sig_nodes(sig: &FnSig) -> Vec<Node> {
    let mut nodes = vec![Node::name(&sig.name)];
    nodes.extend(generics_node(&sig.generics));
    nodes.extend(sig.params.items.iter().map(param_node));
    nodes.extend(sig.ret.as_ref().map(ret_node));
    nodes
}

fn field_node(field: &FieldDecl) -> Node {
    Node::list("field", field.span)
        .children(Node::public(field.vis))
        .child(Node::name(&field.name))
        .child(type_node(&field.ty))
}

fn body_node(body: &Block) -> Node {
    Node::list("body", body.span).children(body.stmts.iter().map(stmt_node))
}

fn fn_node(decl: &FnDecl) -> Node {
    let kind = if decl.is_extern() {
        "extern-func"
    } else {
        "func"
    };
    Node::list(kind, decl.span)
        .children(Node::public(decl.vis))
        .children(sig_nodes(&decl.sig))
        .children(decl.body.as_ref().map(body_node))
}

fn binding_node(binding: &Binding) -> Node {
    let span = binding.span;
    match &binding.kind {
        BindingKind::Wildcard => Node::atom("wildcard", "_", span),
        BindingKind::Name(name) => Node::list("bind", span).child(Node::name(name)),
        BindingKind::Tuple(fields) => {
            Node::list("bind-tuple", span).children(fields.iter().map(binding_node))
        }
        BindingKind::Struct { name, fields } => Node::list("bind-struct", span)
            .child(Node::name(name))
            .children(fields.iter().map(|field| {
                Node::list("bind-field", field.span)
                    .child(Node::name(&field.field))
                    .child(binding_node(&field.binding))
            })),
    }
}

fn pattern_node(pattern: &Pattern) -> Node {
    let span = pattern.span;
    match &pattern.kind {
        PatternKind::Wildcard => Node::atom("wildcard", "_", span),
        PatternKind::Name(name) => Node::list("pat-name", span).child(Node::name(name)),
        PatternKind::Variant {
            enum_name,
            variant,
            payload,
        } => {
            let (text, name_span) = match enum_name {
                Some(e) => (
                    format!("{}.{}", e.name, variant.name),
                    e.span.merge(variant.span),
                ),
                None => (format!(".{}", variant.name), variant.span),
            };
            Node::list("pat", span)
                .child(Node::atom("name", text, name_span))
                .children(payload.iter().map(binding_node))
        }
    }
}

fn stmt_node(stmt: &Stmt) -> Node {
    match stmt {
        Stmt::Expr(e) => expr_node(e),
        Stmt::Let(decl) => Node::list("let", decl.span)
            .children(Node::public(decl.vis))
            .child(binding_node(&decl.binding))
            .child(opt_type_node(decl.ty.as_ref()))
            .child(expr_node(&decl.value)),
        Stmt::While(stmt) => Node::list("while", stmt.span)
            .child(expr_node(&stmt.condition))
            .child(body_node(&stmt.body)),
        Stmt::Fn(decl) => fn_node(decl),
        Stmt::Struct(decl) => Node::list("struct", decl.span)
            .children(Node::public(decl.vis))
            .child(Node::name(&decl.name))
            .children(generics_node(&decl.generics))
            .children(decl.fields.iter().map(field_node)),
        Stmt::Impl(decl) => Node::list("impl", decl.span)
            .children(generics_node(&decl.generics))
            .child(named_type_node(&decl.target))
            .children((!decl.interfaces.is_empty()).then(|| {
                Node::list("interfaces", None).children(decl.interfaces.iter().map(named_type_node))
            }))
            .children(decl.methods.iter().map(fn_node)),
        Stmt::Interface(decl) => Node::list("interface", decl.span)
            .children(Node::public(decl.vis))
            .child(Node::name(&decl.name))
            .children(generics_node(&decl.generics))
            .children(
                decl.methods
                    .iter()
                    .map(|m| Node::list("method-sig", m.span).children(sig_nodes(&m.sig))),
            ),
        Stmt::Enum(decl) => Node::list("enum", decl.span)
            .children(Node::public(decl.vis))
            .child(Node::name(&decl.name))
            .children(generics_node(&decl.generics))
            .children(decl.variants.iter().map(|variant| {
                let node = Node::list("variant", variant.span).child(Node::name(&variant.name));
                match &variant.payload {
                    VariantPayload::Unit => node,
                    VariantPayload::Tuple(types) => node.children(types.iter().map(type_node)),
                    VariantPayload::Struct(fields) => node.children(fields.iter().map(field_node)),
                }
            })),
    }
}

/// A field being read or written prints as `.field`, so it is not mistaken for a variable next to the object.
fn member_node(member: &Member) -> Node {
    let kind = match member {
        Member::Named(_) => "name",
        Member::Index(_) => "tuple-index",
    };
    Node::atom(kind, format!(".{member}"), member.span())
}

/// Unlabeled arguments are just their expression; labeled ones print as `(label: expr)`.
fn arg_node(arg: &CallArg) -> Node {
    match &arg.label {
        Some(label) => Node::list("arg", arg.span())
            .head(format!("{}:", label.name))
            .child(expr_node(&arg.value)),
        None => expr_node(&arg.value),
    }
}

/// The kind of a node that has a safe (`?`) form: `call` or `call?`.
fn safe_kind(safe: Option<Span>, plain: &'static str, safe_name: &'static str) -> &'static str {
    if safe.is_some() { safe_name } else { plain }
}

fn expr_node(expr: &Expr) -> Node {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Error => Node::list("error", span),
        ExprKind::Unary { op, operand } => Node::list("unary", span)
            .head(op.node.as_str())
            .child(expr_node(operand)),
        ExprKind::Try { operand, .. } => Node::list("try", span).child(expr_node(operand)),
        ExprKind::Binary { left, op, right } => Node::list("binary", span)
            .head(op.node.as_str())
            .child(expr_node(left))
            .child(expr_node(right)),
        ExprKind::Logical { left, op, right } => Node::list("logical", span)
            .head(op.node.as_str())
            .child(expr_node(left))
            .child(expr_node(right)),
        ExprKind::Coalesce { left, right, .. } => Node::list("coalesce", span)
            .head("??")
            .child(expr_node(left))
            .child(expr_node(right)),
        ExprKind::Variable(name) => Node::atom("var", name.name, span),
        ExprKind::Grouping(inner) => Node::list("group", span).child(expr_node(inner)),
        ExprKind::Literal(literal) => match literal {
            Literal::Number(n) => Node::atom("num", n.to_string(), span),
            Literal::String(s) => Node::atom("str", format!("{s:?}"), span),
            Literal::Boolean(b) => Node::atom("bool", b.to_string(), span),
            Literal::Nil => Node::atom("nil", "nil", span),
            // `void` is an ordinary identifier, so the literal stays a list.
            Literal::Void => Node::list("void", span),
        },
        ExprKind::Is { operand, variant } => Node::list("is", span)
            .child(expr_node(operand))
            .child(Node::name(variant)),
        ExprKind::Assign { target, value } => Node::list("assign", span)
            .child(Node::name(target))
            .child(expr_node(value)),
        ExprKind::TypeSpecialization { callee, type_args } => Node::list("specialize", span)
            .child(expr_node(callee))
            .children(type_args.items.iter().map(type_node)),
        ExprKind::Call { callee, safe, args } => {
            Node::list(safe_kind(*safe, "call", "call?"), span)
                .child(expr_node(callee))
                .children(args.items.iter().map(arg_node))
        }
        ExprKind::Field {
            object,
            safe,
            member,
        } => Node::list(safe_kind(*safe, "field", "field?"), span)
            .child(expr_node(object))
            .child(member_node(member)),
        ExprKind::SetField {
            object,
            safe,
            member,
            value,
        } => Node::list(safe_kind(*safe, "set-field", "set-field?"), span)
            .child(expr_node(object))
            .child(member_node(member))
            .child(expr_node(value)),
        ExprKind::Tuple(elements) => {
            Node::list("tuple", span).children(elements.iter().map(expr_node))
        }
        ExprKind::List(elements) => {
            Node::list("list", span).children(elements.iter().map(expr_node))
        }
        ExprKind::Map(entries) => Node::list("map", span).children(entries.iter().map(|entry| {
            Node::list("entry", entry.span())
                .child(expr_node(&entry.key))
                .child(expr_node(&entry.value))
        })),
        ExprKind::Index {
            object,
            safe,
            index,
        } => Node::list(safe_kind(*safe, "index", "index?"), span)
            .child(expr_node(object))
            .child(expr_node(index)),
        ExprKind::SetIndex {
            object,
            safe,
            index,
            value,
        } => Node::list(safe_kind(*safe, "set-index", "set-index?"), span)
            .child(expr_node(object))
            .child(expr_node(index))
            .child(expr_node(value)),
        ExprKind::ForceUnwrap { operand, .. } => {
            Node::list("unwrap", span).child(expr_node(operand))
        }
        // Literal parts stay wrapped: a bare `"a"` would read as a string literal inside the interpolation.
        ExprKind::StringInterp(parts) => {
            Node::list("interp", span).children(parts.iter().map(|part| match part {
                StringPart::Literal { value, span } => {
                    Node::list("text", *span).child(Node::atom("str", format!("{value:?}"), *span))
                }
                StringPart::Expr(e) => expr_node(e),
            }))
        }
        ExprKind::Block { stmts, tail } => Node::list("block", span)
            .children(stmts.iter().map(stmt_node))
            .children(
                tail.iter()
                    .map(|t| Node::list("tail", t.span).child(expr_node(t))),
            ),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => Node::list("if", span)
            .child(expr_node(condition))
            .child(expr_node(then_branch))
            .children(else_branch.iter().map(|e| expr_node(e))),
        ExprKind::Match { scrutinee, arms } => Node::list("match", span)
            .child(expr_node(scrutinee))
            .children(arms.iter().map(|arm| {
                Node::list("arm", arm.span)
                    .child(pattern_node(&arm.pattern))
                    .child(expr_node(&arm.body))
            })),
        ExprKind::Lambda { params, ret, body } => Node::list("lambda", span)
            .children(params.items.iter().map(|param| {
                Node::list("param", param.span())
                    .child(Node::name(&param.name))
                    .child(opt_type_node(param.ty.as_ref()))
            }))
            .children(ret.as_deref().map(ret_node))
            .child(expr_node(body)),
        ExprKind::Return(value) => {
            Node::list("return", span).children(value.iter().map(|v| expr_node(v)))
        }
    }
}
