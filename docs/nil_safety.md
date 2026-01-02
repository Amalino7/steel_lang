# Nil Safety

Steel is designed to be nil-safe. By default, types cannot be `nil`. You must explicitly opt-in using the `?` suffix.

## Optional Types

```rust@formatter:off
let a: number = 10; // Cannot be nil
let b: number? = nil; // Valid

```

## Safety Operators

### 1. Nil Coalescing (`??`)

Provide a default value if the variable is nil.

```rust@formatter:off
let x: number? = nil;
let y = x ?? 0; // y becomes 0

```

### 2. Force Unwrap (`!`)

If you are certain a value exists, you can force unwrap it. This will panic if the value is nil.

```rust@formatter:off
let x: number? = 5;
let y = x!;

```

### 3. Safe Navigation (`?.`)

Access fields or methods only if the object is not nil.

```rustrust@formatter:off
struct Node {
    next: Node?
}
let n = Node(next: nil);

// Returns nil immediately if 'next' is nil, rather than crashing
let val = n.next?.next;

```

## Flow-Sensitive Typing

The compiler is smart enough to know when you've checked for nil.

```rustrust@formatter:off
func process(val: number?) {
// 'val' is number? here

    if val != nil {
        // Compiler knows 'val' is definitely 'number' here
        // No unwrap needed!
        println(val + 10);
    }
}
```