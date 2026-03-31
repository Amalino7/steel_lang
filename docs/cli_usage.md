# Steel Lang CLI Usage

The Steel Lang command-line interface provides several modes and options for compiling, type-checking, and running your
Steel programs.

## Basic Syntax

```bash
steel_lang <source_file> [mode] [flags]
```

## Quick Start

```bash
# Run a program (default mode)
steel_lang script.steel

# Type-check without running
steel_lang script.steel check

# Debug mode with full output
steel_lang script.steel run -d
```

---

## Modes

Modes determine what Steel does with your source file.

### `run` (Default)

Compiles and executes the script.

**Usage:**

```bash
steel_lang script.steel run
# or simply
steel_lang script.steel
```

**Example:**

`hello.steel`:

```steel
func main() {
    println("Hello, World!");
}
main();
```

**Command:**

```bash
$ steel_lang hello.steel
```

**Output:**

```
Hello, World!
```

**What happens:**

1. Source code is scanned (lexical analysis)
2. Tokens are parsed into an AST
3. Type checking is performed
4. Code is compiled to bytecode
5. Bytecode is executed by the VM

---

### `check`

Performs type-checking only without executing the code.

**Usage:**

```bash
steel_lang script.steel check
```

**When to use:**

- Quick type validation
- CI/CD pipelines
- Before committing code
- When you want fast feedback without running expensive code

**Example:**

`math.steel`:

```steel
func add(a: number, b: number): number {
    return a + b;
}

let result = add(5, 10);
```

**Command:**

```bash
$ steel_lang math.steel check
```

**Output:**

```
Type checking has passed.
```

**Example with error:**

`error.steel`:

```steel
let x: number = "string";  // Type error
```

**Command:**

```bash
$ steel_lang error.steel check
```

**Output:**

```
Error: Type mismatch
  ┌─ error.steel:1:17
  │
1 │ let x: number = "string";
  │                 ^^^^^^^^ Expected type 'number', found 'string'
```

---

### `parse`

Outputs the Abstract Syntax Tree (AST) for debugging the parser.

**Usage:**

```bash
steel_lang script.steel parse
steel_lang script.steel parse -d  # With detailed AST output
```

**When to use:**

- Parser debugging
- Understanding how code is parsed
- Contributing to Steel development

**Example:**

`simple.steel`:

```steel
let x = 5 + 3;
```

**Command:**

```bash
$ steel_lang simple.steel parse -d
```

**Output:**

```
=== AST ===
Global {
    statements: [
        Let {
            name: "x",
            initializer: Binary {
                left: Number(5),
                operator: Plus,
                right: Number(3)
            }
        }
    ]
}
=============
```

---

## Flags

Flags modify the behavior of any mode.

### `-d` (Debug Mode)

Prints detailed debugging information including:

- AST (Abstract Syntax Tree) when using `parse` or `check`
- Bytecode disassembly when using `run`
- Type analysis results when using `check`

**Usage:**

```bash
steel_lang script.steel [mode] -d
```

**Examples:**

**With `run` mode:**

```bash
$ steel_lang script.steel run -d
```

Output includes bytecode disassembly:

```
=== Disassembly ===
0000    OP_CONSTANT     0 (5)
0002    OP_CONSTANT     1 (3)
0004    OP_ADD
0005    OP_RETURN
===================
```

**With `check` mode:**

```bash
$ steel_lang script.steel check -d
```

Output includes typed AST and type information.

**With `parse` mode:**

```bash
$ steel_lang script.steel parse -d
```

Output shows the complete AST structure.

---

### `-f` (Force Mode)

Bypasses error checks and panics on a failed step.

**Usage:**

```bash
steel_lang script.steel run -f
```

**Warning:** Using force mode:

- Used for internal testing and development.
- Might have been useful in CI/CD pipelines,
  however, compile time errors now return a non-zero exit code.

---

### `--version` (or `-V`)

Display version information.

**Usage:**

```bash
steel_lang --version
```

**Output:**

```
steel 0.6.2  # use --version to see the current version
```

---

### `--time` (or `-t`)

Show timing information for each compilation phase.

**Usage:**

```bash
steel_lang script.steel --time
```

**Output:**

```

=== Phase Timings ===
Scan + Parse:     0.5ms
Type checking:    2.1ms
Compilation:      0.8ms
Execution:        5.3ms
---------------------
Total:            9.9ms
=====================
```

**Use cases:**

- Performance profiling
- Identifying bottlenecks
- Optimization verification

---

## Combining Flags

You can combine multiple flags:

```bash
# Debug mode + force mode
steel_lang script.steel run -d -f

# Check mode + debug
steel_lang script.steel check -d

# Run with timing
steel_lang script.steel run --time

# Debug with timing
steel_lang script.steel run -d --time
```

---

## Exit Codes

Steel uses standard exit codes:

| Exit Code | Meaning                          |
|-----------|----------------------------------|
| 0         | Success                          |
| 1         | Syntax or type error encountered |
| 2         | Runtime error or panic           |

**Example usage in scripts:**

```bash
#!/bin/bash
steel_lang script.steel check
if [ $? -eq 0 ]; then
    echo "Type checking passed!"
    steel_lang script.steel run
else
    echo "Type checking failed!"
    exit 1
fi
```

---

## Common Usage Patterns

### Development Workflow

```bash
# 1. Quick type check while developing
steel_lang script.steel check

# 2. Run with timing to check performance
steel_lang script.steel --time

# 3. Debug if there are issues
steel_lang script.steel run -d
```

### CI/CD Integration

```bash
# Type check all Steel files
for file in src/*.steel; do
    steel_lang "$file" check || exit 1
done

# Run test files
for test in tests/*.steel; do
    steel_lang "$test" run || exit 1
done
```

### Performance Testing

```bash
# Compare timing with different approaches
steel_lang approach1.steel --time > timing1.txt
steel_lang approach2.steel --time > timing2.txt
diff timing1.txt timing2.txt
```

### Debugging Type Errors

```bash
# See exactly what types were inferred
steel_lang script.steel check -d > types.txt
less types.txt
```

---

## Standard Library Prelude

Steel automatically includes a standard library prelude before compiling your code. This provides:

- Built-in functions (`println`, `assert`, `clock`, etc.)
- List methods (`.map()`, `.filter()`, `.sum()`, etc.)
- Number methods (`.abs()`, `.squared()`)

You can disable the prelude with `--no-stdlib`.
**Warning:** Disabling the prelude might cause type errors and make the language unsusable.

---

## Examples

### Example 1: Simple Script

**File:** `hello.steel`

```steel
println("Hello from Steel!");
```

**Run:**

```bash
$ steel_lang hello.steel
Hello from Steel!
```

### Example 2: Type Checking

**File:** `math.steel`

```steel
func factorial(n: number): number {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}

let result = factorial(5);
assert(result, 120);
```

**Check types:**

```bash
$ steel_lang math.steel check
Type checking has passed.
```

**Run:**

```bash
$ steel_lang math.steel
[no output if assertion passes]
```

### Example 3: Debugging

**File:** `debug.steel`

```steel
let x = 10;
let y = 20;
let sum = x + y;
println(sum);
```

**Debug run:**

```bash
$ steel_lang debug.steel run -d
=== Disassembly ===
[bytecode output]
===================
30
```

### Example 4: Performance Analysis

**File:** `fib.steel`

```steel
func fib(n: number): number {
    if n <= 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}
let result = fib(20);
println(result);
```

**Time it:**

```bash
$ steel_lang fib.steel --time
6765

=== Phase Timings ===
Scan + Parse:     1.0ms
Type checking:    1.1ms
Compilation:      0.5ms
Execution:      125.4ms
---------------------
Total:          128.0ms
=====================
```

---

## Tips

1. **Use `check` mode frequently** during development for fast feedback
2. **Combine `-d` with `check`** to understand type inference
3. **Use `--time`** to identify performance bottlenecks
4. **Avoid `-f` in production** - fix errors instead of forcing execution
5. **Use descriptive file names** - Steel files should end with `.steel`

---

## Related Documentation

- [Getting Started](getting_started.md) - Introduction and first programs
- [Troubleshooting](troubleshooting.md) - Common errors and solutions
- [Syntax Guide](syntax_simple.md) - Language syntax reference
