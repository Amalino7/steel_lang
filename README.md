# Steel Lang

![CI Status](https://github.com/amalino7/steel_lang/actions/workflows/ci.yml/badge.svg)
![Release](https://github.com/amalino7/steel_lang/actions/workflows/release.yml/badge.svg)

**Steel** is a statically typed, interpreted scripting language written in Rust.
It features a C-style syntax, a custom bytecode virtual machine, and a hand-rolled mark-and-sweep garbage collector.

It offers the user-friendliness of a scripting language alongside the static type safety found in compiled languages.

## Key Features

* **Static Typing:** Catch errors at compile time, not runtime.
* **Memory Management:** Automatic garbage collection (Mark-and-Sweep).
* **Advanced Type System:**
    * **Generics:** Type-safe structs and functions (e.g., `struct Box<T>`).
    * **Enums & Pattern Matching:** Algebraic data types with `match` expressions.
    * **Interfaces:** Define behaviour contracts and implement them for structs or primitives.
* **Nil Safety:** Optional types (`T?`) and strict null checks with flow-sensitive refinement.
* **First-class Functions:** Closures and higher-order functions.

## Documentation

For detailed documentation, see the docs folder in the repository.

## Building & Running

Ensure you have Rust installed.

```bash
# Build the release binary
cargo build --release

# Run a script
./target/release/steel_lang path/to/script.steel