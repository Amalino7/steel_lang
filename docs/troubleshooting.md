# Troubleshooting Guide

This guide covers common errors, their causes, and solutions when working with Steel Lang.

## Table of Contents

- [Common Errors](#common-errors)
    - [Syntax Errors](#syntax-errors)
    - [Type Errors](#type-errors)
    - [Runtime Errors](#runtime-errors)
- [Type System Issues](#type-system-issues)
- [Performance Issues](#performance-issues)[troubleshooting.md](troubleshooting.md)
- [FAQ](#faq)

---

## Common Errors

### Syntax Errors

#### Missing Semicolon

**Error Message:**

```
Syntax Error: Expected ';' after expression
```

**Cause:** Steel requires semicolons after statements.

**Solution:**

```steel
// ❌ Wrong
let x = 10

// ✅ Correct
let x = 10;
```

#### Missing Type Annotation

**Error Message:**

```
Syntax Error: Expected type annotation
```

**Cause:** Function parameters and return types must be explicitly typed.

**Solution:**

```steel
// ❌ Wrong
func add(a, b) {
    return a + b;
}

// ✅ Correct
func add(a: number, b: number): number {
    return a + b;
}
```

#### Mismatched Braces

**Error Message:**

```
Syntax Error: Unexpected token '}', expected expression
```

**Cause:** Unbalanced `{` and `}` braces.

**Solution:** Check that every opening brace has a matching closing brace. Use your editor's brace matching feature.

---

### Type Errors

#### Type Mismatch

**Error Message:**

```
Type Error: Expected type 'number', found 'string'
```

**Cause:** Trying to use a value of the wrong type.

**Example:**

```steel
let x: number = "hello";  // ❌ Wrong
```

**Solution:** Ensure the value matches the expected type:

```steel
let x: number = 42;       // ✅ Correct
let y: string = "hello";  // ✅ Correct
```

#### Cannot Infer Type

**Error Message:**

```
Type Error: Cannot infer type for variable
```

**Cause:** The type checker cannot determine the type from context.

**Solution:** Add an explicit type annotation:

```steel
// ❌ Wrong - might fail if type can't be inferred
let x = some_complex_function();

// ✅ Correct
let x: number = some_complex_function();
```

#### Optional Type Violation

**Error Message:**

```
Type Error: Cannot use optional type 'number?' as 'number'
```

**Cause:** Trying to use an optional value without handling the nil case.

**Example:**

```steel
let x: number? = nil;
let y = x + 5;  // ❌ Wrong - x might be nil
```

**Solutions:**

1. **Nil coalescing:**

```steel
let y = (x ?? 0) + 5;  // ✅ Use 0 if x is nil
```

2. **Nil checking:**

```steel
if x != nil {
    let y = x + 5;  // ✅ x is refined to number here
}
```

3. **Force unwrap (use with caution):**

```steel
let y = x! + 5;  // ✅ But panics if x is nil
```

#### Generic Type Mismatch

**Error Message:**

```
Type Error: Type mismatch in generic parameters
```

**Cause:** Generic types don't match expected constraints.

**Example:**

```steel
struct Box<T> { value: T }

let box1: Box<number> = Box(value: 5);
let box2: Box<string> = Box(value: "hi");
box1 = box2;  // ❌ Wrong - Box<number> ≠ Box<string>
```

**Solution:** Ensure generic types match:

```steel
let box1: Box<number> = Box(value: 5);
let box2: Box<number> = Box(value: 10);
box1 = box2;  // ✅ Correct
```

#### Interface Not Implemented

**Error Message:**

```
Type Error: Type 'Point' does not implement interface 'Drawable'
```

**Cause:** Trying to use a type as an interface it doesn't implement.

**Solution:** Implement all required methods:

```steel
interface Drawable {
    func draw(self): void;
}

struct Point { x: number, y: number }

// Implement the interface
impl Point : Drawable {
    func draw(self): void {
        println("Point(", self.x, ",", self.y, ")");
    }
}
```

#### Enum Variant Type Error

**Error Message:**

```
Type Error: Wrong types for enum variant
```

**Cause:** Enum variant constructed with wrong types.

**Example:**

```steel
enum Result<T, E> { Ok(T), Err(E) }

let res: Result<number, string> = Result.Ok("wrong");  // ❌ Wrong - expects number
```

**Solution:**

```steel
let res: Result<number, string> = Result.Ok(42);  // ✅ Correct
```

---

### Runtime Errors

#### Division by Zero

**Error:** Program panics with division by zero.

**Cause:** Attempting to divide by zero.

**Solution:** Check before dividing:

```steel
func safe_divide(a: number, b: number): number? {
    if b == 0 {
        return nil;
    }
    return a / b;
}
```

#### Unwrap on Nil

**Error:** `Panic: Attempted to unwrap nil value`

**Cause:** Using force unwrap `!` on a nil value.

**Example:**

```steel
let x: number? = nil;
let y = x!;  // ❌ Panics
```

**Solution:** Check before unwrapping:

```steel
let x: number? = nil;
if x != nil {
    let y = x!;  // ✅ Safe
}
// or
let y = x ?? 0;  // ✅ Use default value
```

#### List Index Out of Bounds

**Error:** `Panic: Index out of bounds`

**Cause:** Accessing a list at an invalid index.

**Solution:** Check list length first:

```steel
let list = [1, 2, 3];

// ❌ Wrong
let item = list[10];  // Panics

// ✅ Correct
if 10 < list.len() {
    let item = list[10];
}
```

#### Stack Overflow

**Error:** `Panic: Stack overflow`

**Cause:** Too much recursion without a base case.

**Example:**

```steel
func infinite(n: number): number {
    return infinite(n + 1);  // ❌ No base case
}
```

**Solution:** Always have a base case:

```steel
func factorial(n: number): number {
    if n <= 1 {
        return 1;  // ✅ Base case
    }
    return n * factorial(n - 1);
}
```

#### Panic

**Error:** `Panic: <custom message>`

**Cause:** Explicit call to `panic()` function.

**Solution:** Handle error conditions gracefully:

```steel
// ❌ Panics immediately
if something_wrong {
    panic("Error occurred");
}

// ✅ Return error value instead
enum Result<T, E> { Ok(T), Err(E) }
if something_wrong {
    return Result.Err("Error occurred");
}
```

---

## Type System Issues

### Flow-Sensitive Typing Not Working

**Issue:** Type checker doesn't recognize that you've checked for nil.

**Example:**

```steel
let x: number? = 10;
if x != nil {
    // Should work but might not in complex cases
    println(x + 5);
}
```

**Solutions:**

1. **Use local variable:**

```steel
let x: number? = 10;
if x != nil {
    let safe_x = x;  // Capture refined type
    println(safe_x + 5);
}
```

2. **Use force unwrap:**

```steel
let x: number? = 10;
if x != nil {
    println(x! + 5);  // Safe here because of check
}
```

### Pattern Matching Not Exhaustive

**Error:** `Type Error: Pattern matching is not exhaustive`

**Cause:** Not all enum variants are handled in match expression.

**Example:**

```steel
enum Color { Red, Green, Blue }

let c = Color.Red;
match c {
    Color.Red => { println("Red"); }
    Color.Green => { println("Green"); }
    // ❌ Missing Blue case
}
```

**Solution:** Handle all cases or use wildcard:

```steel
match c {
    Color.Red => { println("Red"); }
    Color.Green => { println("Green"); }
    Color.Blue => { println("Blue"); }
}

// Or use wildcard for remaining cases
match c {
    Color.Red => { println("Red"); }
    _ => { println("Not red"); }
}
```

### Shadowing Confusion

**Issue:** Variable shadowing creates confusion about types.

**Example:**

```steel
let x = 10;
{
    let x = "string";  // Shadows outer x
    println(x);        // Prints "string"
}
println(x);            // Prints 10
```

**Solution:** Use different variable names to avoid confusion:

```steel
let count = 10;
{
    let message = "string";
    println(message);
}
println(count);
```

---

## Performance Issues

### Slow Fibonacci

**Issue:** Recursive fibonacci is extremely slow for large numbers.

**Cause:** Exponential time complexity due to repeated calculations.

**Solution:** Use iterative approach:

```steel
// ❌ Slow
func fib_recursive(n: number): number {
    if n <= 1 { return n; }
    return fib_recursive(n - 1) + fib_recursive(n - 2);
}

// ✅ Fast
func fib_iterative(n: number): number {
    if n <= 1 { return n; }
    let a = 0;
    let b = 1;
    let i = 2;
    while i <= n {
        let temp = a + b;
        a = b;
        b = temp;
        i += 1;
    }
    return b;
}
```

### String Concatenation in Loop

**Issue:** Repeatedly concatenating strings in a loop is slow.

**Cause:** Each concatenation creates a new string.

**Example:**

```steel
// ❌ Slow
let result = "";
let i = 0;
while i < 1000 {
    result += "a";  // Creates new string each time
    i += 1;
}
```

**Solution:** For simple cases, this is currently the only way. Future versions may add string builders.

### Excessive Garbage Collection

**Issue:** Program pauses frequently for GC.

**Cause:** Creating too many temporary objects.

**Solution:**

- Reuse objects where possible
- Avoid creating unnecessary intermediate values
- Consider using primitive types (numbers) instead of objects when appropriate

---

## FAQ

### Q: How do I debug type errors?

**A:** Use the `check` mode to see type errors without running:

```bash
steel_lang script.steel check
```

For more detail, use debug mode:

```bash
steel_lang script.steel check -d
```

### Q: Can I see the AST or bytecode?

**A:** Yes, use different modes:

```bash
steel_lang script.steel parse -d   # Show AST
steel_lang script.steel run -d     # Show bytecode disassembly
```

### Q: Why does my code pass type checking but fail at runtime?

**A:** Some errors can only be detected at runtime:

- Division by zero
- Index out of bounds
- Unwrapping nil values
- User-triggered panics

### Q: How do I handle errors without panicking?

**A:** Use the Result pattern:

```steel
enum Result<T, E> { Ok(T), Err(E) }

func safe_operation(): Result<number, string> {
    if error_condition {
        return Result.Err("Error message");
    }
    return Result.Ok(42);
}

let result = safe_operation();
match result {
    .Ok(value) => { println("Success:", value); }
    .Err(error) => { println("Error:", error); }
}
```

### Q: What's the difference between nil and void?

**A:**

- `nil` is a value that represents "nothing" and requires optional types (`number?`)
- `void` is the type of functions that don't return a value
- `void` can also be used as a value (unit type)

```steel
let x: number? = nil;  // nil is a value

func do_something(): void {  // void is return type
    println("doing something");
    // No return statement needed
}

let nothing = do_something();  // nothing has type void
```

### Q: How do I check if a value is NaN?

**A:** Use the `is_nan()` built-in function:

```steel
let x = 0 / 0;
if is_nan(x) {
    println("x is NaN");
}
```

### Q: Can I have multiple return statements?

**A:** Yes, and it's common for early returns:

```steel
func process(x: number?): number {
    if x == nil {
        return 0;  // Early return
    }
    return x * 2;
}
```

### Q: How do I see what standard library is available?

**A:** Check the standard library prelude in `src/stdlib/steel/*`, or look at the list methods documentation:

- `println()`, `print()` - Output
- `assert()` - Testing
- `panic()` - Error handling
- `clock()` - Timing
- `to_str()` - Conversion
- `is_nan()` - NaN checking
- List methods: `.len()`, `.push()`, `.pop()`, `.map()`, `.filter()`, `.sum()`, etc.

---

## Still Having Issues?

If this guide doesn't solve your problem:

1. Check the example programs in `examples/` folder
2. Review other documentation files in `docs/`
3. Try running with debug flags: `-d` for detailed output
4. Report bugs at: https://github.com/anthropics/steel_lang/issues

## Related Documentation

- [Getting Started](getting_started.md) - Basic introduction
- [Syntax Guide](syntax_simple.md) - Complete syntax reference
- [Nil Safety](nil_safety.md) - Optional types in detail
- [CLI Usage](cli_usage.md) - Command-line options
