## Examples

**Fibonacci (Recursive)**

```rust@formatter:off
func fib(n: number): number {
    if n < = 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}
println(fib(20));

```

**Generics & Pattern Matching**

```rust@formatter:off
enum Result<T, E> { Ok(T), Err(E) }

func unwrap_or(res: Result<number, string>, default : number): number {
    match res {
        .Ok(val) => { return val; }
        .Err(_) => { return default; }
    }
}

```

# Structs and Methods

Steel supports defining custom data structures and attaching behavior to them via implementation blocks.

## Structs

Structs are declared with named fields.

```rust@formatter:off
struct Point {
    x: number,
    y: number,
}

// Instantiation
let p = Point(x: 10, y: 20);

// Access
println(p.x);
p.y = 50;

```

## Methods (impl)

You can define methods for both custom structs and primitive types using the `impl` keyword.

```rust@formatter:off
impl Point {
    // Static method (no 'self')
    func new(x: number, y: number): Point {
        return Point(x: x, y: y);
    }

    // Instance method (takes 'self')
    func add( self , other: Point): Point {
        return Point(x: self.x + other.x, y: self.y + other.y);
    }
}

let p1 = Point.new(1, 2);
let p2 = Point(x: 3, y: 4);
let p3 = p1.add(p2);

```

### Extending Primitives

You can even add methods to built-in types like `number` or `void`.

```rust@formatter:off
impl number {
    func squared( self ): number {
        return self * self;
    }
}

println(10.squared()); // Prints 100
```