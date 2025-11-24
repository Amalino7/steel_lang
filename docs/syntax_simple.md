### **Steel Language Syntax Guide**

**Steel** is a statically typed, C-style scripting language. It requires semicolons to terminate statements and uses
braces for scoping.

-----

### **1. Comments**

Steel supports both single-line and block comments.

* **Single-line:** Starts with `//` and continues to the end of the line.
* **Block:** Enclosed between `/*` and `*/`. These can be nested.

<!-- end list -->

```rust
// This is a comment
/* This is a 
   block comment */
```

-----

### **2. Data Types**

There are four primitive types defined in the AST.

* `number`: Represents 64-bit floating-point numbers (e.g., `10`, `5.5`).
* `string`: Text enclosed in double quotes (e.g., `"Hello"`).
* `boolean`: `true` or `false`.
* `void`: Represents the absence of a value (primarily for function returns).

-----

### **3. Variables**

Variables are declared using the `let` keyword. They can be implicitly typed (inferred) or explicitly typed.

**Syntax:**

```
let variableName = value;          // Type inferred
let variableName: type = value;    // Explicit type
```

**Examples:**

```
let a = 10;
let b: string = "Hello";
let c: boolean = true;
```

-----

### **4. Operators**

Steel uses standard arithmetic operators and keyword-based logical operators.

* **Arithmetic:** `+`, `-`, `*`, `/`
    * *Note:* The `+` operator can also concatenate two strings.
* **Comparison:** `==`, `!=`, `<`, `<=`, `>`, `>=`
* **Logical:** `and`, `or`, `!` (bang/not)

-----

### **5. Control Flow**

Control flow statements require braces `{}` around their bodies.

#### **If-Else**

The condition must evaluate to a `boolean`.

```
if a > 10 {
    // do something
} else {
    // do something else
}
```

#### **While Loop**

```
while a < 10 {
    a = a + 1;
}
```

-----

### **6. Functions**

Functions are declared using the `func` keyword. You must specify types for parameters. The return type is specified
after the parameter list (preceded by a colon). If no return type is specified, it defaults to `void`.

**Syntax:**

```
func name(param1:type, param2:type):returnType
{
    // body
    return value;
}
```

**Example:**

```
func add(a:number, b:number):number
{
    return a + b;
}

func printMessage(): void {
    let msg = "Hello";
    // implicit void return
}
```

**Function Calls:**
Arguments are passed in parentheses, separated by commas.

```
let result = add(5, 10);
```

-----

### **7. Program Structure**

A Steel program consists of a series of declarations and statements. The language supports global code execution (
scripts) as well as function definitions.

**Full Example:**

```
// Fibonacci Example
func fib(n: number): number
{
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

let start = 10;
let result = fib(start);
```