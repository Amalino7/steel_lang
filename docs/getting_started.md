# Getting Started with Steel Lang

Welcome to Steel Lang! This guide will help you get up and running with Steel, a statically-typed scripting language that combines the ease of scripting with the safety of static typing.

## What is Steel?

Steel is a modern programming language featuring:
- **Static typing** with powerful type inference
- **C-style syntax** that's familiar and easy to learn
- **Advanced features** like generics, pattern matching, and nil safety
- **Custom bytecode VM** for fast execution
- **Garbage collection** so you don't worry about memory management

## Installation

### Building from Source

1. Make sure you have Rust installed (https://rustup.rs/)
2. Clone the repository and build:

```bash
cargo build --release
```

3. The compiled binary will be at `./target/release/steel_lang`

4. (Optional) Add to your PATH:

```bash
# Add to your ~/.bashrc or ~/.zshrc
export PATH="$PATH:/path/to/steel_lang/target/release"
```

## Your First Steel Program

Let's write a classic "Hello, World!" program.

### 1. Create a file named `hello.steel`

```steel
func main() {
    println("Hello, World!");
}

main();
```

### 2. Run it

```bash
steel_lang hello.steel
```

### 3. Output

```
Hello, World!
```

## Basic Syntax Overview

### Variables

Variables are declared with `let` and must have a type (inferred or explicit):

```steel
let x = 10;           // Type inferred as number
let y: string = "hi"; // Explicit type annotation
let z: boolean = true;
```

### Functions

Functions are declared with `func` and require type annotations for parameters and return type:

```steel
func add(a: number, b: number): number {
    return a + b;
}

let result = add(5, 3);
println(result); // 8
```

### Control Flow

```steel
// If statements
if x > 10 {
    println("x is greater than 10");
} else if x == 10 {
    println("x is exactly 10");
} else {
    println("x is less than 10");
}

// While loops
let i = 0;
while i < 5 {
    println(i);
    i += 1;
}
```

## Example Programs

### Example 1: Fibonacci (Recursive)

```steel
func fib(n: number): number {
    if n == 1 or n == 2 {
        return 1;
    }
    return fib(n - 1) + fib(n - 2);
}

let result = fib(10);
println("fib(10) =", result);
assert(result, 55);
```

### Example 2: Structs and Methods

```steel
struct Point {
    x: number,
    y: number,
}

impl Point {
    func new(x: number, y: number): Point {
        return Point(x: x, y: y);
    }

    func distance_from_origin(self): number {
        return (self.x * self.x + self.y * self.y);
    }

    func add(self, other: Point): Point {
        return Point(x: self.x + other.x, y: self.y + other.y);
    }
}

let p1 = Point.new(3, 4);
let p2 = Point(x: 1, y: 2);
let p3 = p1.add(p2);

println("p3:", p3.x, ",", p3.y); // p3: 4 , 6
```

### Example 3: Closures

```steel
func make_adder(x: number): func(number): number {
    func adder(y: number): number {
        return x + y;
    }
    return adder;
}

let add_five = make_adder(5);
println(add_five(10)); // 15
println(add_five(20)); // 25
```

### Example 4: Enums and Pattern Matching

```steel
enum Result<T, E> {
    Ok(T),
    Err(E),
}

func divide(a: number, b: number): Result<number, string> {
    if b == 0 {
        return Result.Err("Division by zero");
    }
    return Result.Ok(a / b);
}

let result = divide(10, 2);
match result {
    .Ok(value) => { println("Result:", value); }
    .Err(error) => { println("Error:", error); }
}
```

### Example 5: Nil Safety

```steel
struct Node {
    value: number,
    next: Node?,  // Optional type (can be nil)
}

let head = Node(value: 1, next: nil);
let second = Node(value: 2, next: head);

// Safe navigation operator
println(head.next?.value);  // nil (doesn't crash)

// Nil coalescing
let value = head.next?.value ?? 0;
println(value);  // 0

// Force unwrap (panics if nil)
second.next = Node(value: 3, next: nil);
let unwrapped = second.next!;
println(unwrapped.value);  // 3
```

### Example 6: Generics

```steel
struct Box<T> {
    value: T,
}

impl<T> Box<T> {
    func new(value: T): Box<T> {
        return Box(value: value);
    }

    func unwrap(self): T {
        return self.value;
    }

    func map<U>(self, transform: func(T): U): Box<U> {
        return Box(value: transform(self.value));
    }
}

func double(x: number): number {
    return x * 2;
}

let box = Box.new(5);
let doubled = box.map(double);
println(doubled.unwrap());  // 10
```

### Example 7: Interfaces

```steel
interface Drawable {
    func draw(self): void;
}

struct Circle {
    radius: number,
}

impl Circle : Drawable {
    func draw(self): void {
        println("Drawing circle with radius", self.radius);
    }
}

struct Rectangle {
    width: number,
    height: number,
}

impl Rectangle : Drawable {
    func draw(self): void {
        println("Drawing rectangle", self.width, "x", self.height);
    }
}

func render(shape: Drawable) {
    shape.draw();
}

render(Circle(radius: 5));
render(Rectangle(width: 10, height: 20));
```

## Built-in Functions

Steel comes with several built-in functions:

```steel
// Printing
println("Hello", "World");  // Prints with newline
print("No newline");        // Prints without newline

// Assertions
assert(1 + 1, 2);  // Passes
// assert(1 + 1, 3);  // Would panic

// Time
let start = clock();
// ... some code ...
let end = clock();
println("Elapsed:", end - start);

// Conversion
let str = to_str(42);
println(str);  // "42"

// Math
let is_nan_value = is_nan(0 / 0);
println(is_nan_value);  // true

// Panic
// panic("Something went wrong!");  // Stops execution
```

## List Methods

Steel has built-in support for lists with useful methods:

```steel
let numbers = [1, 2, 3, 4, 5];

// Length
println(numbers.len());  // 5

// Add elements
numbers.push(6);
println(numbers);  // [1, 2, 3, 4, 5, 6]

// Remove last element
let last = numbers.pop();
println(last);  // 6

// Map
func double(x: number): number { return x * 2; }
let doubled = numbers.map(double);
println(doubled);  // [2, 4, 6, 8, 10]

// Filter
func is_even(x: number): boolean { return x % 2 == 0; }
let evens = numbers.filter(is_even);
println(evens);  // [2, 4]

// Sum (for number lists)
let total = numbers.sum();
println(total);  // 15

// Contains
println(numbers.contains(3));  // true
println(numbers.contains(10)); // false
```

## Common Pitfalls for New Users

### 1. Forgetting Return Types

```steel
// ❌ Wrong - missing return type
func add(a: number, b: number) {
    return a + b;
}

// ✅ Correct
func add(a: number, b: number): number {
    return a + b;
}
```

### 2. Type Mismatches

```steel
// ❌ Wrong - can't assign string to number
let x: number = "hello";

// ✅ Correct
let x: number = 42;
let y: string = "hello";
```

### 3. Forgetting to Call Functions

```steel
func greet() {
    println("Hello!");
}

// ❌ Wrong - just defines the function, doesn't call it
greet;

// ✅ Correct
greet();
```

### 4. Nil Safety Violations

```steel
let x: number? = nil;

// ❌ Wrong - can't use optional directly as number
let y = x + 5;

// ✅ Correct - unwrap first
let y = (x ?? 0) + 5;  // Using nil coalescing
// or
if x != nil {
    let y = x + 5;  // x is refined to number here
}
```

## Next Steps

Now that you've learned the basics, check out:

- **[Syntax Guide](syntax_simple.md)** - Detailed syntax reference
- **[Nil Safety](nil_safety.md)** - Understanding optional types
- **[Structs and Methods](structs_and_methods.md)** - Object-oriented programming
- **[Enums and Pattern Matching](enums_and_pattern_matching.md)** - Algebraic data types
- **[CLI Usage](cli_usage.md)** - Advanced command-line options
- **[Examples](../examples/)** - More example programs

## Getting Help

If you encounter issues:

1. Check the [Troubleshooting Guide](troubleshooting.md)
2. Review the documentation in the `docs/` folder
3. Look at example programs in the `examples/` folder
4. Report issues at: https://github.com/anthropics/steel_lang/issues

Happy coding with Steel!
