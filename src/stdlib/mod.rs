mod core_natives;
mod list_natives;
mod map_natives;
mod number_natives;
mod string_natives;

use crate::typechecker::core::types::Type;
use crate::vm::value::NativeFn;

pub struct NativeDef {
    pub name: &'static str,
    /// Only needed for functions whose types can't be expressed in Steel syntax (e.g. varargs).
    /// For all other natives, use `extern func` declarations in the prelude `.steel` files.
    pub type_: Option<Type>,
    pub func: NativeFn,
}

/// Returns the full Steel prelude, assembled from `.steel` files at compile time.
pub fn get_prelude() -> &'static str {
    concat!(
        "\n",
        include_str!("steel/core.steel"),
        include_str!("steel/number.steel"),
        include_str!("steel/string.steel"),
        include_str!("steel/list.steel"),
        include_str!("steel/map.steel"),
        include_str!("steel/result.steel"),
    )
}

/// Returns all native function definitions.
pub fn get_natives() -> Vec<NativeDef> {
    let mut natives = core_natives::natives();
    natives.extend(number_natives::natives());
    natives.extend(string_natives::natives());
    natives.extend(list_natives::natives());
    natives.extend(map_natives::natives());
    natives
}

#[allow(dead_code)]
pub fn debug_size<T>() {
    println!(
        "Size of {}: {} bytes, alignment: {} bytes",
        std::any::type_name::<T>(),
        size_of::<T>(),
        align_of::<T>()
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::parser::ast::Stmt;
    use crate::scanner::{PRELUDE_FILE, Scanner};
    use std::collections::HashSet;

    /// Names of every `extern func` in the prelude; `impl` externs as `Type.method`, like the natives.
    fn declared_externs() -> HashSet<String> {
        let parsed = Parser::new(Scanner::new(get_prelude(), PRELUDE_FILE))
            .parse()
            .unwrap_or_else(|errs| panic!("the prelude must parse: {errs:#?}"));
        let mut names = HashSet::new();
        for stmt in &parsed.body {
            match stmt {
                Stmt::ExternFunction { name, .. } => {
                    names.insert(name.lexeme.to_string());
                }
                Stmt::Impl { name, methods, .. } => {
                    for method in methods {
                        if let Stmt::ExternFunction { name: method, .. } = method {
                            names.insert(format!("{}.{}", name.0.lexeme, method.lexeme));
                        }
                    }
                }
                _ => {}
            }
        }
        names
    }

    #[test]
    fn every_untyped_native_is_declared_by_an_extern() {
        let declared = declared_externs();
        for native in get_natives().iter().filter(|n| n.type_.is_none()) {
            assert!(
                declared.contains(native.name),
                "native '{}' has no `extern func` in the prelude, so no Steel code can reach it",
                native.name
            );
        }
    }

    #[test]
    fn every_extern_has_a_native() {
        let natives = get_natives();
        for name in declared_externs() {
            assert!(
                natives.iter().any(|n| n.name == name),
                "`extern func {name}` has no native"
            );
        }
    }
}
