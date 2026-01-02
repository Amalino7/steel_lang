# Enums and Pattern Matching

Enums in Steel are powerful. They can be simple C-style enumerations or hold data like Rust enums.

## Defining Enums

```rust@formatter:off
// Simple Enum
enum Color { Red, Green, Blue }

// Data-carrying Enum (ADT)
enum Message {
    Quit,
    Move { x: number, y: number },
    Write(string)
}

```

## Pattern Matching (match)

The `match` expression allows you to destructure enums and execute code based on the variant.

```rust@formatter:off
let msg = Message.Move(x: 10, y: 20);

match msg {
    .Quit => {
        println("Quitting");
    }
    .Move(: x, : y) => {
        println("Moving to", x, y);
    }
    .Write(text) => {
        println("Message:", text);
    }
}

```

## Generics

Steel supports generic type parameters in Structs, Enums, and Functions.

```rust@formatter:off
enum Option<T> {
    Some(T),
    None
}

struct Box<T> {
    value: T
}

func identity<T>(val: T): T {
    return val;
}

```