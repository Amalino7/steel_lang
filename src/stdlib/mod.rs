mod core_natives;
mod list_natives;
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
        include_str!("steel/core.steel"),
        include_str!("steel/number.steel"),
        include_str!("steel/string.steel"),
        include_str!("steel/list.steel"),
        include_str!("steel/result.steel"),
    )
}

/// Returns all native function definitions.
pub fn get_natives() -> Vec<NativeDef> {
    let mut natives = core_natives::natives();
    natives.extend(number_natives::natives());
    natives.extend(string_natives::natives());
    natives.extend(list_natives::natives());
    natives
}

#[allow(dead_code)]
pub fn debug_size<T>() {
    println!(
        "Size of {}: {} bytes, alignment: {} bytes",
        std::any::type_name::<T>(),
        std::mem::size_of::<T>(),
        align_of::<T>()
    );
}
