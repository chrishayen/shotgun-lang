# Shotgun Language Design Document

## Vision

A systems language for app developers that combines:
- Odin's simplicity and data-oriented design
- Zig's comptime and explicit allocators without the funky syntax
- Python's clean aesthetics and readability
- Rust's safety without the borrower fights and noisy syntax

Code should read like a well-formatted article with meaningful whitespace.

---

## Decided

### Target Audience
- App developers
- Need easy C interop

### Aesthetics
- Clean, minimal punctuation
- Whitespace-aware formatting
- Reads like prose
- File extension: `.bs`

---

## To Decide

### Memory Management
- [x] GC available but optional (chosen, exact method TBD)
- [x] Default: scope-based arenas, compiler figures it out (chosen)

### Null Handling
- [x] Optional types with `?` suffix: `str?` (chosen)
- [x] Unwrap with `or`: `value or default`, `value or return none`, `value or return` (chosen)

### Error Handling
- [x] Result types: `Config or Error` (chosen)
- [x] Error propagation: `or error ErrorType {}`, `or return error` (chosen)

### Type System
- [x] Static typing, no inference (chosen)
- [ ] Generics syntax (deferred)
- [x] Traits with explicit impl: `Name :: trait { }`, `Name :: impl Trait { }` (chosen)

### Syntax
- [x] Required braces (chosen)
- [x] Method syntax: `StructName methodName :: fn(self) ...` (chosen)
- [x] Struct initialization: `Name { field: value, ... }` with trailing comma allowed (chosen)
- [x] Named fields only for structs, no positional shorthand (chosen)
- [x] No semicolons, newlines end statements (chosen)
- [x] Explicit `return` keyword required (chosen)
- [x] Struct declaration: `Name :: struct { ... }` (chosen)
- [x] Function declaration: `fn name(...) { ... }` (chosen)
- [x] Method declaration: `Name :: methodName(self) { ... }` (chosen)
- [x] Variable declaration: `type name = value`, no type inference (chosen)
- [x] Lowercase primitive types: str, int, bool, f32, f64, u32, u64 (chosen)
- [x] Keywords for logic: `and`, `or`, `not` (chosen, may revisit)
- [x] String interpolation by default: `"Hello, {name}"`, escape with `\{` (chosen)

### Compilation Target
- [x] C (chosen for v1)
- [ ] LLVM (maybe later, for optimization)

### Concurrency Model
- [x] Goroutines/green threads with `go` keyword (chosen)
- [x] Channels: `chan int c = chan()`, `c.send(val)`, `c.recv()` (chosen)
- [x] Channel receive with fallback: `c.recv() or default`, `c.recv() or return` (chosen)

### Implementation Language
- [x] OCaml (chosen - pragmatic FP, fast compilation, great for compilers)

---

## Syntax Sketches

### Current Direction

```
Person :: struct {
    name  str
    age   int
    email str?
}

Person :: greet(self) str {
    return "Hello, {self.name}"
}

Person :: birthday(self) {
    self.age += 1
}

fn main {
    Person[] people = [
        Person { name: "Alice", age: 30, email: "alice@example.com" },
        Person { name: "Bob", age: 25, email: none },
    ]

    for p in people {
        print(p.greet())
    }
}

uses: std.io
```

### Import System

Short form for few imports:
```
uses: std.io, std.json
```

Long form for many imports:
```
uses:
  - std.io
  - std.json
  - myapp.utils
```

Project configuration in `shotgun.toml`:
```toml
name = "myapp"
```

Resolution:
- `myapp.utils` -> `./utils.bs` (project-relative)
- `std.io` -> standard library (TODO)
- Last segment becomes namespace: `utils.helper()`

### Concurrency Example

```
fn main {
    chan int results = chan()

    go fetch_data(results)
    go fetch_data(results)

    int a = results.recv()
    int b = results.recv() or wait(100) or 0

    print("got {a} and {b}")
}

fn fetch_data(chan int out) {
    out.send(42)
}
```

### Traits Example

```
Stringer :: trait {
    str(self) str
}

Person :: impl Stringer {
    str(self) str {
        return self.name
    }
}
```

### Error Handling Example

```
fn read_config(str path) Config or Error {
    str contents = read_file(path) or error FileNotFound { path: path }
    Config cfg = parse(contents) or error ParseError { line: 10 }
    return cfg
}

fn main {
    Config cfg = read_config("app.conf") or error StartupError {}
}
```

---

## Memory Management Brainstorming

### The Problem
GC pauses suck. Manual memory is error-prone. Rust lifetimes are noisy.

### Ideas Explored

**Garbage Recycling / Pooled Reuse**
- Zero memory on release, keep it in a pool
- Timer-based: if reused before expiry, fast allocation; otherwise actually free
- Pros: no GC pauses, cache-friendly reuse
- Cons: memory lingers, timer tuning

**Event Loop Controlled Cleanup**
- Dev hooks cleanup into their loop (after frame, between requests)
- Batched frees at known points
- Pros: predictable, dev controls when "pause" happens
- Cons: memory grows until cleanup, adds cognitive load

**App Pattern Analysis**
Most apps fit a few patterns:
- Idle: waiting, memory stable
- Crunching: batch work, free at end
- Request/response: allocate per request, free after
- Streaming: fixed buffers, reuse forever
- Game loop: per-frame allocation

### Current Direction

Simple defaults, no syntax overhead:

- **Stack** - local values, automatic
- **Arena** - anything that escapes, freed when scope ends

Compiler figures it out:

```
fn handle_request(Request req) Response {
    Person p = Person { ... }  // arena, freed when function returns
    int x = 5                  // stack
    return build_response(p)
}
```

Mental model: everything dies at end of scope unless you put it somewhere that outlives the scope.

```
Person[] cache = []  // lives until you clear it
```

For long-lived stuff, you explicitly hold it somewhere.

---

## Quick Reference

### Variables
```
int x = 5
str name = "Alice"
bool flag = true
f64 pi = 3.14159
```

### Functions
```
fn greet(str name) {
    print("Hello, {name}")
}

fn add(int a, int b) int {
    return a + b
}

fn main {
    greet("world")
}
```

### Structs & Methods
```
Person :: struct {
    name str
    age  int
}

Person :: greet(self) str {
    return "Hi, I'm {self.name}"
}

fn main {
    Person p = Person { name: "Alice", age: 30 }
    print(p.greet())
}
```

### If/Else
```
if x > 10 {
    print("big")
} else if x > 5 {
    print("medium")
} else {
    print("small")
}
```

### For Loops
```
int[] nums = [1, 2, 3, 4, 5]
for n in nums {
    print(n)
}
```

### Optionals
```
str? maybe_name = none
str? actual_name = "Bob"

str result = maybe_name or "default"
str name = actual_name or return
```

### Logical Operators
```
if x > 0 and y > 0 {
    print("both positive")
}

if x == 0 or y == 0 {
    print("at least one zero")
}

if not done {
    print("still working")
}
```

### Comparison
```
x == y    // equal
x != y    // not equal
x < y     // less than
x > y     // greater than
x <= y    // less or equal
x >= y    // greater or equal
```

### Arrays
```
int[] numbers = [1, 2, 3]
str[] names = ["Alice", "Bob"]
```

### Channels (concurrency)
```
chan int c = chan()
go some_func(c)
int val = c.recv()
c.send(42)
```

---

## Open Questions

1. How do we handle C interop syntax?
2. What's the standard library scope for v1?
3. Self-hosting: goal or non-goal?

---

## References

- [Vale's Generational References](https://verdagon.dev/blog/generational-references)
- [Odin Language](https://odin-lang.org)
- [Zig Language](https://ziglang.org)
