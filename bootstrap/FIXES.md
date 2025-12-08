# Codegen Fixes Plan

## 1. Pass type info to codegen

Add a typed AST or type environment that codegen can query.

**Option A**: Annotate AST with types during semantic pass
- Add `typ option` to expression nodes
- Semantic pass fills them in
- Codegen reads them

**Option B**: Pass symbol table to codegen
- Codegen queries it when needed
- Simpler, less intrusive

Going with **Option B** - simpler.

### Changes:
- `semantic.ml`: Return the env along with Ok/Error
- `codegen.ml`: Accept env, query for variable types
- `main.ml`: Thread env through

---

## 2. Fix method call codegen

Current: `p.greet()` -> `_greet(&p)`
Needed: `p.greet()` -> `Person_greet(&p)`

### Changes in `codegen.ml`:
```ocaml
(* In gen_call, when handling EMember method calls *)
| EMember (obj, method_name) ->
  (* Look up obj's type in env *)
  let obj_type = infer_type env obj in
  match obj_type with
  | Some (TUser type_name) ->
    emit type_name;
    emit "_";
    emit method_name;
    emit "(";
    gen_expr env indent obj;  (* pass obj as first arg *)
    (* ... rest of args *)
  | _ -> (* fallback *)
```

---

## 3. Fix array literal types

Current: `[Person{...}]` -> `((int64_t[]){...})`
Needed: `[Person{...}]` -> proper Array_Person initialization

### Changes:
- Infer element type from first element
- Generate proper array construction

```ocaml
| EArrayLit (e :: _ as elems) ->
  let elem_type = infer_type env e in
  (* Generate typed array *)
```

---

## 4. Fix `self.x` in nested expressions

Current works: `self.name` -> `self->name`
Broken: `self.address.city` -> `self.address->city` (wrong)

Actually, re-reading: the current code does:
```ocaml
| EMember (obj, field) ->
  gen_expr indent obj;
  (match obj with
   | EIdent "self" -> emit "->"
   | _ -> emit ".");
  emit field
```

This is correct for `self.name`. For `self.address.city`:
- First `EMember(EMember(self, "address"), "city")`
- Inner: `self->address`
- Outer: needs `.city` (address is a value, not pointer)

So this is actually fine for value types. The bug is only if `address` was also a pointer. For v1, assuming value semantics for struct fields, this is OK.

---

## 5. Fix `go` statement

Current: `go fetch(x)` -> `pthread_create(&_thread, NULL, (void*(*)(void*))fetch(x), NULL);`
This calls `fetch(x)` immediately and casts result to function pointer. Broken.

Proper fix needs a wrapper. For v1, just generate a TODO comment or simple stub.

### Changes:
```ocaml
| SGo e ->
  emitln "/* TODO: goroutine */";
  emit "/* go */ ";
  gen_expr indent e;
  emitln ";"
```

---

## 6. Fix string interpolation format specifiers

Current: Always uses `%s`
Needed: `%lld` for int, `%f` for float, etc.

### Changes:
- Infer type of interpolated expression
- Use appropriate format specifier

---

## Priority Order

1. Pass type env to codegen (enables everything else)
2. Fix method calls (most visible bug)
3. Fix array literals
4. Fix string interpolation formats
5. Stub out `go` properly
6. (Optional) Better error codegen

---

## Testing

After each fix:
```bash
dune build
dune exec shotgun -- emit-c ../examples/demo.bs > test.c
gcc -Wall -Wextra -std=c99 -pthread test.c -o test
```

Target: `test.c` compiles with no warnings.
