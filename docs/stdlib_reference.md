# Standard Library Reference

This document describes the methods and functions available in the Steel standard library.

## Core Functions

These functions are available globally.

- `panic(msg: any): never`: Stops execution with the given message.
- `assert(left: any, right: any): void`: Asserts that `left` and `right` are equal.
- `clock(): number`: Returns the current time in seconds.
- `to_str<T>(val: T): string`: Converts a value to its string representation.
- `is_nan(val: number): boolean`: Returns true if the number is NaN.
- `readline(): string`: Reads a line from standard input.
- `range(start: number, end_exclusive: number): List<number>`: Returns a list of numbers from `start` to
  `end_exclusive`.
- `Void(): void`: A function that does nothing and returns nothing.

## List<Val>

Methods available on `List<Val>`.

- `len(): number`: Returns the number of elements in the list.
- `is_empty(): boolean`: Returns true if the list is empty.
- `push(value: Val): void`: Adds an element to the end of the list.
- `pop(): Val?`: Removes and returns the last element of the list, or `nil` if empty.
- `slice(start: number, end_exclusive: number): List<Val>`: Returns a sub-list.
- `concat(other: List<Val>): List<Val>`: Returns a new list containing elements of both lists.
- `contains(value: Val): boolean`: Returns true if the list contains the given value.
- `fold<U>(initial: U, f: func(U, Val): U): U`: Reduces the list to a single value using the provided function.
- `each(f: func(Val)): void`: Calls the function for each element in the list.
- `map<M>(f: func(Val): M): List<M>`: Returns a new list with the results of applying the function to each element.
- `filter(f: func(Val): boolean): List<Val>`: Returns a new list with elements that satisfy the predicate.
- `first(): Val?`: Returns the first element of the list, or `nil` if empty.
- `last(): Val?`: Returns the last element of the list, or `nil` if empty.
- `reverse(): List<Val>`: Returns a new list with elements in reverse order.
- `any(f: func(Val): boolean): boolean`: Returns true if any element satisfies the predicate.
- `all(f: func(Val): boolean): boolean`: Returns true if all elements satisfy the predicate.
- `find(f: func(Val): boolean): Val?`: Returns the first element that satisfies the predicate, or `nil` if none found.
- `count(f: func(Val): boolean): number`: Returns the number of elements that satisfy the predicate.
- `index_of(value: Val): number?`: Returns the index of the first occurrence of the value, or `nil` if not found.
- `flat_map<M>(f: func(Val): List<M>): List<M>`: Applies a function that returns a list to each element and flattens the
  result.
- `sort_by(less_than: func(Val, Val): boolean): List<Val>`: Returns a new sorted list based on the provided comparison
  function.

### List<number>

- `sum(): number`: Returns the sum of all numbers in the list.
- `min(): number?`: Returns the minimum number in the list, or `nil` if empty.
- `max(): number?`: Returns the maximum number in the list, or `nil` if empty.

### List<string>

- `join(sep: string): string`: Joins the strings in the list with the given separator.

## Map<Key, Val>

Methods available on `Map<Key, Val>`.

- `len(): number`: Returns the number of entries in the map.
- `contains_key(key: Key): boolean`: Returns true if the map contains the given key.
- `remove(key: Key): Val?`: Removes and returns the value associated with the key, or `nil` if not found.

## number

Methods available on `number`.

- `abs(): number`: Returns the absolute value.
- `floor(): number`: Returns the largest integer less than or equal to the number.
- `ceil(): number`: Returns the smallest integer greater than or equal to the number.
- `round(): number`: Returns the nearest integer.
- `trunc(): number`: Returns the integer part of the number.
- `sqrt(): number`: Returns the square root.
- `pow(exp: number): number`: Returns the number raised to the power of `exp`.
- `log(): number`: Returns the natural logarithm.
- `log2(): number`: Returns the base-2 logarithm.
- `log10(): number`: Returns the base-10 logarithm.
- `sin(): number`: Returns the sine.
- `cos(): number`: Returns the cosine.
- `tan(): number`: Returns the tangent.
- `min(other: number): number`: Returns the minimum of two numbers.
- `max(other: number): number`: Returns the maximum of two numbers.

## string

Methods available on `string`.

- `length(): number`: Returns the number of characters in the string.
- `split(sep: string): List<string>`: Splits the string by the separator.
- `trim(): string`: Returns a string with leading and trailing whitespace removed.
- `trim_start(): string`: Returns a string with leading whitespace removed.
- `trim_end(): string`: Returns a string with trailing whitespace removed.
- `to_upper(): string`: Returns an uppercase version of the string.
- `to_lower(): string`: Returns a lowercase version of the string.
- `starts_with(prefix: string): boolean`: Returns true if the string starts with the prefix.
- `ends_with(suffix: string): boolean`: Returns true if the string ends with the suffix.
- `contains(substr: string): boolean`: Returns true if the string contains the substring.
- `replace(from: string, to: string): string`: Returns a new string with all occurrences of `from` replaced by `to`.
- `substring(start: number, end_exclusive: number): string`: Returns a substring.
- `index_of(substr: string): number?`: Returns the index of the first occurrence of the substring, or `nil` if not
  found.
- `chars(): List<string>`: Returns a list of characters in the string.
- `parse_number(): number?`: Parses the string as a number, or returns `nil` if invalid.
- `repeat(n: number): string`: Returns the string repeated `n` times.

## Result<Ok, Err>

An enum representing either success (`Ok`) or failure (`Err`).

### Variants

- `Ok(Ok)`
- `Err(Err)`

### Methods

- `unwrap(): Ok`: Returns the `Ok` value or panics if it's an `Err`.
- `map<NewOk>(f: func(Ok): NewOk): Result<NewOk, Err>`: Maps the `Ok` value.
- `map_err<NewErr>(f: func(Err): NewErr): Result<Ok, NewErr>`: Maps the `Err` value.
- `is_ok(): boolean`: Returns true if it's an `Ok` variant.
- `is_err(): boolean`: Returns true if it's an `Err` variant.

### Result<T, T>

- `merge(): T`: Returns the inner value regardless of whether it's `Ok` or `Err`.
