(* C Code Generation for Shotgun compiler *)

open Ast

(* Buffer for output *)
let buf = Buffer.create 4096

let emit s = Buffer.add_string buf s
let emitln s = Buffer.add_string buf s; Buffer.add_char buf '\n'
let emit_indent n = for _ = 1 to n do Buffer.add_string buf "    " done

(* Convert Shotgun type to C type *)
let rec c_type = function
  | TInt -> "int64_t"
  | TStr -> "char*"
  | TBool -> "bool"
  | TF32 -> "float"
  | TF64 -> "double"
  | TU32 -> "uint32_t"
  | TU64 -> "uint64_t"
  | TOptional t -> c_type t  (* Optional uses same type, NULL = none *)
  | TArray t -> Printf.sprintf "Array_%s*" (c_type_name t)
  | TChan t -> Printf.sprintf "Chan_%s*" (c_type_name t)
  | TUser name -> name  (* Stack allocated structs *)
  | TResult (t, _) -> c_type t  (* Simplified: just return the value type *)
  | TVoid -> "void"

(* Get a safe C identifier name for a type *)
and c_type_name = function
  | TInt -> "int64"
  | TStr -> "str"
  | TBool -> "bool"
  | TF32 -> "f32"
  | TF64 -> "f64"
  | TU32 -> "u32"
  | TU64 -> "u64"
  | TOptional t -> "opt_" ^ c_type_name t
  | TArray t -> "arr_" ^ c_type_name t
  | TChan t -> "chan_" ^ c_type_name t
  | TUser name -> name
  | TResult (t, _) -> c_type_name t
  | TVoid -> "void"

(* Convert binary operator *)
let c_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"

(* Convert assignment operator *)
let c_assignop = function
  | Assign -> "="
  | AddAssign -> "+="
  | SubAssign -> "-="
  | MulAssign -> "*="
  | DivAssign -> "/="

(* Get format specifier for a type *)
let format_spec = function
  | TInt -> "%\" PRId64 \""
  | TStr -> "%s"
  | TBool -> "%d"
  | TF32 -> "%f"
  | TF64 -> "%f"
  | TU32 -> "%\" PRIu32 \""
  | TU64 -> "%\" PRIu64 \""
  | _ -> "%s"  (* Default to string *)

(* Code generation context *)
type ctx = {
  env: Semantic.env;
  locals: (string, typ) Hashtbl.t;
  in_method: bool;
  current_type: string option;
}

let create_ctx env = {
  env;
  locals = Hashtbl.create 32;
  in_method = false;
  current_type = None;
}

let with_method_ctx ctx type_name = {
  ctx with
  in_method = true;
  current_type = Some type_name;
  locals = Hashtbl.copy ctx.locals;
}

let add_local ctx name typ =
  Hashtbl.replace ctx.locals name typ

(* Get type of expression using semantic's helper *)
let get_type ctx expr =
  Semantic.get_expr_type ctx.env ctx.locals expr

(* Get function params from env *)
let get_func_params ctx name =
  match Hashtbl.find_opt ctx.env.Semantic.symbols name with
  | Some (Semantic.SFunc (params, _)) -> Some params
  | _ -> None

(* Generate expression *)
let rec gen_expr ctx indent expr =
  match expr with
  | EInt i -> emit (string_of_int i)
  | EFloat f -> emit (string_of_float f)
  | EString parts -> gen_string_parts ctx parts
  | EBool true -> emit "true"
  | EBool false -> emit "false"
  | ENone -> emit "NULL"
  | EIdent "self" -> emit "self"
  | EIdent name -> emit name
  | EBinary (op, l, r) ->
    emit "(";
    gen_expr ctx indent l;
    emit " ";
    emit (c_binop op);
    emit " ";
    gen_expr ctx indent r;
    emit ")"
  | EUnary (Not, e) ->
    emit "(!";
    gen_expr ctx indent e;
    emit ")"
  | ECall (callee, args) ->
    gen_call ctx indent callee args
  | EMember (obj, field) ->
    gen_expr ctx indent obj;
    (* Use -> for pointers (self, or pointer types) *)
    let use_arrow = match obj with
      | EIdent "self" -> true
      | _ ->
        match get_type ctx obj with
        | Some (TOptional _) -> true  (* Pointers *)
        | Some (TArray _) -> true
        | Some (TChan _) -> true
        | _ -> false
    in
    emit (if use_arrow then "->" else ".");
    emit field
  | EIndex (arr, idx) ->
    gen_expr ctx indent arr;
    emit "->data[";
    gen_expr ctx indent idx;
    emit "]"
  | EOr (e, clause) ->
    gen_or ctx indent e clause
  | EStructLit (name, fields) ->
    emit "(";
    emit name;
    emit "){ ";
    let first = ref true in
    List.iter (fun (fname, fexpr) ->
      if not !first then emit ", ";
      first := false;
      emit ".";
      emit fname;
      emit " = ";
      gen_expr ctx indent fexpr
    ) fields;
    emit " }"
  | EArrayLit elems ->
    gen_array_literal ctx indent elems
  | EChan ->
    emit "chan_create()"
  | EParen e ->
    emit "(";
    gen_expr ctx indent e;
    emit ")"
  | EAssign (op, lhs, rhs) ->
    gen_expr ctx indent lhs;
    emit " ";
    emit (c_assignop op);
    emit " ";
    gen_expr ctx indent rhs

(* Generate array literal with proper type *)
and gen_array_literal ctx indent elems =
  match elems with
  | [] -> emit "NULL"
  | (first_elem :: _) ->
    let elem_type = get_type ctx first_elem in
    let type_name = match elem_type with
      | Some t -> c_type_name t
      | None -> "int64"  (* fallback *)
    in
    let c_elem_type = match elem_type with
      | Some t -> c_type t
      | None -> "int64_t"
    in
    (* Generate inline array initialization *)
    emit "(Array_";
    emit type_name;
    emit "*)(& (Array_";
    emit type_name;
    emit "){ .data = (";
    emit c_elem_type;
    emit "[]){";
    let first = ref true in
    List.iter (fun e ->
      if not !first then emit ", ";
      first := false;
      gen_expr ctx indent e
    ) elems;
    emit "}, .len = ";
    emit (string_of_int (List.length elems));
    emit ", .cap = ";
    emit (string_of_int (List.length elems));
    emit " })"

(* Generate function/method call *)
and gen_call ctx indent callee args =
  match callee with
  | EMember (obj, method_name) ->
    (* Method call: obj.method(args) -> Type_method(&obj, args) *)
    (match method_name with
     | "send" ->
       emit "chan_send(";
       gen_expr ctx indent obj;
       emit ", ";
       (match args with
        | [arg] -> gen_expr ctx indent arg
        | _ -> emit "/* invalid args */");
       emit ")"
     | "recv" ->
       emit "chan_recv(";
       gen_expr ctx indent obj;
       emit ")"
     | _ ->
       (* Get the type of obj to generate proper method name *)
       let type_name = match get_type ctx obj with
         | Some (TUser name) -> name
         | _ -> "_Unknown"
       in
       emit type_name;
       emit "_";
       emit method_name;
       emit "(";
       (* Pass obj as first argument (pointer to self) *)
       emit "&";
       gen_expr ctx indent obj;
       List.iter (fun arg ->
         emit ", ";
         gen_expr ctx indent arg
       ) args;
       emit ")")
  | EIdent "print" ->
    (* Built-in print function *)
    (match args with
     | [arg] ->
       let arg_type = get_type ctx arg in
       (match arg_type with
        | Some TInt ->
          emit "printf(\"%\" PRId64 \"\\n\", ";
          gen_expr ctx indent arg;
          emit ")"
        | Some TBool ->
          emit "printf(\"%d\\n\", ";
          gen_expr ctx indent arg;
          emit ")"
        | Some TF32 | Some TF64 ->
          emit "printf(\"%f\\n\", ";
          gen_expr ctx indent arg;
          emit ")"
        | _ ->
          emit "printf(\"%s\\n\", ";
          gen_expr ctx indent arg;
          emit ")")
     | _ ->
       emit "printf(\"\\n\")")
  | EIdent name ->
    emit name;
    emit "(";
    let first = ref true in
    List.iter (fun arg ->
      if not !first then emit ", ";
      first := false;
      gen_expr ctx indent arg
    ) args;
    (* Pad missing optional params with NULL *)
    (match get_func_params ctx name with
     | Some params ->
       let num_args = List.length args in
       let named_params = List.filter (fun p -> p <> Ast.PSelf) params in
       List.iteri (fun i p ->
         if i >= num_args then begin
           if not !first then emit ", ";
           first := false;
           emit "NULL"
         end;
         ignore p
       ) named_params
     | None -> ());
    emit ")"
  | _ ->
    emit "/* complex call */";
    gen_expr ctx indent callee;
    emit "(";
    let first = ref true in
    List.iter (fun arg ->
      if not !first then emit ", ";
      first := false;
      gen_expr ctx indent arg
    ) args;
    emit ")"

(* Generate or expression *)
and gen_or ctx indent e clause =
  match clause with
  | OrExpr default_e ->
    emit "((";
    gen_expr ctx indent e;
    emit ") ? (";
    gen_expr ctx indent e;
    emit ") : (";
    gen_expr ctx indent default_e;
    emit "))"
  | OrReturn None ->
    (* Simplified: just evaluate and check *)
    emit "(";
    gen_expr ctx indent e;
    emit ")"
  | OrReturn (Some ret_e) ->
    emit "((";
    gen_expr ctx indent e;
    emit ") ? (";
    gen_expr ctx indent e;
    emit ") : (";
    gen_expr ctx indent ret_e;
    emit "))"
  | OrError (err_type, _fields) ->
    emit "(/* or error ";
    emit err_type;
    emit " */ ";
    gen_expr ctx indent e;
    emit ")"
  | OrWait timeout_e ->
    (* Simplified: just call with a note about the timeout *)
    emit "(/* or wait ";
    gen_expr ctx indent timeout_e;
    emit "ms */ ";
    gen_expr ctx indent e;
    emit ")"

(* Generate C for string parts (interpolation) *)
and gen_string_parts ctx parts =
  match parts with
  | [SLiteral s] ->
    (* Simple string, just escape and quote *)
    emit "\"";
    String.iter (fun c ->
      match c with
      | '\n' -> emit "\\n"
      | '\r' -> emit "\\r"
      | '\t' -> emit "\\t"
      | '"' -> emit "\\\""
      | '\\' -> emit "\\\\"
      | c -> emit (String.make 1 c)
    ) s;
    emit "\""
  | _ ->
    (* Interpolated string - use shotgun_format *)
    let format_buf = Buffer.create 64 in
    let exprs = ref [] in
    List.iter (function
      | SLiteral s ->
        String.iter (fun c ->
          match c with
          | '\n' -> Buffer.add_string format_buf "\\n"
          | '\r' -> Buffer.add_string format_buf "\\r"
          | '\t' -> Buffer.add_string format_buf "\\t"
          | '"' -> Buffer.add_string format_buf "\\\""
          | '\\' -> Buffer.add_string format_buf "\\\\"
          | '%' -> Buffer.add_string format_buf "%%"
          | c -> Buffer.add_char format_buf c
        ) s
      | SInterp e ->
        (* Get type to determine format specifier *)
        let spec = match get_type ctx e with
          | Some t -> format_spec t
          | None -> "%s"
        in
        Buffer.add_string format_buf spec;
        exprs := e :: !exprs
    ) parts;
    emit "shotgun_format(\"";
    emit (Buffer.contents format_buf);
    emit "\"";
    List.iter (fun e ->
      emit ", ";
      gen_expr ctx 0 e
    ) (List.rev !exprs);
    emit ")"

(* Generate statement *)
let rec gen_stmt ctx indent stmt =
  emit_indent indent;
  match stmt with
  | SVarDecl (typ, name, expr) ->
    add_local ctx name typ;
    emit (c_type typ);
    emit " ";
    emit name;
    emit " = ";
    gen_expr ctx indent expr;
    emitln ";"
  | SConstDecl (name, expr) ->
    (* Infer type from expression *)
    let typ = match get_type ctx expr with
      | Some t -> t
      | None -> TInt  (* fallback *)
    in
    add_local ctx name typ;
    emit "const ";
    emit (c_type typ);
    emit " ";
    emit name;
    emit " = ";
    gen_expr ctx indent expr;
    emitln ";"
  | SReturn None ->
    emitln "return;"
  | SReturn (Some e) ->
    emit "return ";
    gen_expr ctx indent e;
    emitln ";"
  | SIf (cond, then_stmts, else_stmts) ->
    emit "if (";
    gen_expr ctx indent cond;
    emitln ") {";
    List.iter (gen_stmt ctx (indent + 1)) then_stmts;
    emit_indent indent;
    (match else_stmts with
     | None -> emitln "}"
     | Some stmts ->
       emitln "} else {";
       List.iter (gen_stmt ctx (indent + 1)) stmts;
       emit_indent indent;
       emitln "}")
  | SFor (var, iter, body) ->
    (* Infer element type from iterator *)
    let elem_type = match get_type ctx iter with
      | Some (TArray t) -> t
      | _ -> TInt  (* fallback *)
    in
    add_local ctx var elem_type;
    emit "for (size_t _i = 0; _i < ";
    gen_expr ctx indent iter;
    emit "->len; _i++) {";
    emitln "";
    emit_indent (indent + 1);
    emit (c_type elem_type);
    emit " ";
    emit var;
    emit " = ";
    gen_expr ctx indent iter;
    emitln "->data[_i];";
    List.iter (gen_stmt ctx (indent + 1)) body;
    emit_indent indent;
    emitln "}"
  | SMatch (e, arms) ->
    emit "/* match */ {";
    emitln "";
    emit_indent (indent + 1);
    emit "typeof(";
    gen_expr ctx indent e;
    emit ") _match_val = ";
    gen_expr ctx indent e;
    emitln ";";
    List.iter (fun (pat, stmts) ->
      emit_indent (indent + 1);
      (match pat with
       | PIdent name ->
         emit "/* pattern: ";
         emit name;
         emitln " */"
       | PConstructor (name, binding) ->
         emit "/* pattern: ";
         emit name;
         (match binding with Some b -> emit " "; emit b | None -> ());
         emitln " */");
      List.iter (gen_stmt ctx (indent + 2)) stmts
    ) arms;
    emit_indent indent;
    emitln "}"
  | SGo e ->
    (* For now, just call the function directly with a TODO comment *)
    emitln "/* TODO: spawn goroutine */";
    emit_indent indent;
    gen_expr ctx indent e;
    emitln ";"
  | SExpr e ->
    gen_expr ctx indent e;
    emitln ";"

(* Generate struct definition *)
let gen_struct name fields =
  emitln "";
  emit "typedef struct ";
  emit name;
  emitln " {";
  List.iter (fun f ->
    emit_indent 1;
    emit (c_type f.field_type);
    emit " ";
    emit f.field_name;
    emitln ";"
  ) fields;
  emit "} ";
  emit name;
  emitln ";"

(* Generate error struct *)
let gen_error name fields =
  gen_struct name fields

(* Generate trait - just a comment since C doesn't have traits *)
let gen_trait name methods =
  emitln "";
  emit "/* trait ";
  emit name;
  emitln " */";
  List.iter (fun m ->
    emit "/*   ";
    emit m.tm_name;
    emitln " */";
  ) methods

(* Generate method *)
let gen_method ctx type_name method_name params ret body =
  let method_ctx = with_method_ctx ctx type_name in
  add_local method_ctx "self" (TUser type_name);
  emitln "";
  (match ret with
   | Some t -> emit (c_type t)
   | None -> emit "void");
  emit " ";
  emit type_name;
  emit "_";
  emit method_name;
  emit "(const ";
  emit type_name;
  emit "* self";
  List.iter (function
    | PSelf -> ()  (* Already handled *)
    | PNamed (t, name) ->
      add_local method_ctx name t;
      emit ", ";
      emit (c_type t);
      emit " ";
      emit name
  ) params;
  emitln ") {";
  List.iter (gen_stmt method_ctx 1) body;
  emitln "}"

(* Generate impl block *)
let gen_impl ctx type_name _trait_name methods =
  emitln "";
  emit "/* impl for ";
  emit type_name;
  emitln " */";
  List.iter (fun m ->
    gen_method ctx type_name m.im_name m.im_params m.im_return m.im_body
  ) methods

(* Generate function *)
let gen_function ctx name params ret body =
  let func_ctx = { ctx with locals = Hashtbl.copy ctx.locals } in
  emitln "";
  (* main returns int in C *)
  if name = "main" then
    emit "int"
  else
    (match ret with
     | Some t -> emit (c_type t)
     | None -> emit "void");
  emit " ";
  emit name;
  emit "(";
  let first = ref true in
  List.iter (function
    | PSelf -> ()  (* Not valid in functions *)
    | PNamed (t, pname) ->
      add_local func_ctx pname t;
      if not !first then emit ", ";
      first := false;
      emit (c_type t);
      emit " ";
      emit pname
  ) params;
  if !first then emit "void";
  emitln ") {";
  List.iter (gen_stmt func_ctx 1) body;
  if name = "main" then begin
    emit_indent 1;
    emitln "return 0;"
  end;
  emitln "}"

(* Generate the standard prelude *)
let gen_prelude () =
  emitln "/* Generated by Shotgun compiler */";
  emitln "#define _POSIX_C_SOURCE 200809L";
  emitln "#include <inttypes.h>";
  emitln "#include <stdio.h>";
  emitln "#include <stdlib.h>";
  emitln "#include <stdint.h>";
  emitln "#include <stdbool.h>";
  emitln "#include <string.h>";
  emitln "#include <stdarg.h>";
  emitln "#include <unistd.h>";
  emitln "#include <pthread.h>";
  emitln "";
  emitln "/* Runtime support */";
  emitln "static char* shotgun_format(const char* fmt, ...) {";
  emitln "    va_list args;";
  emitln "    va_start(args, fmt);";
  emitln "    int len = vsnprintf(NULL, 0, fmt, args);";
  emitln "    va_end(args);";
  emitln "    char* buf = malloc(len + 1);";
  emitln "    va_start(args, fmt);";
  emitln "    vsnprintf(buf, len + 1, fmt, args);";
  emitln "    va_end(args);";
  emitln "    return buf;";
  emitln "}";
  emitln "";
  emitln "/* Array runtime */";
  emitln "typedef struct { int64_t* data; size_t len; size_t cap; } Array_int64;";
  emitln "typedef struct { char** data; size_t len; size_t cap; } Array_str;";
  emitln "typedef struct { bool* data; size_t len; size_t cap; } Array_bool;";
  emitln "typedef struct { float* data; size_t len; size_t cap; } Array_f32;";
  emitln "typedef struct { double* data; size_t len; size_t cap; } Array_f64;";
  emitln "";
  emitln "/* Channel stubs */";
  emitln "typedef struct { void* data; } Chan_int64;";
  emitln "static Chan_int64* chan_create(void) { return calloc(1, sizeof(Chan_int64)); }";
  emitln "static void chan_send(Chan_int64* c, int64_t v) { (void)c; (void)v; /* TODO */ }";
  emitln "static int64_t chan_recv(Chan_int64* c) { (void)c; return 0; /* TODO */ }";
  emitln ""

(* Generate forward declaration for function *)
let gen_function_decl name params ret =
  if name = "main" then
    emit "int"
  else
    (match ret with
     | Some t -> emit (c_type t)
     | None -> emit "void");
  emit " ";
  emit name;
  emit "(";
  let first = ref true in
  List.iter (function
    | PSelf -> ()
    | PNamed (t, pname) ->
      if not !first then emit ", ";
      first := false;
      emit (c_type t);
      emit " ";
      emit pname
  ) params;
  if !first then emit "void";
  emitln ");"

(* Generate forward declaration for method *)
let gen_method_decl type_name method_name params ret =
  (match ret with
   | Some t -> emit (c_type t)
   | None -> emit "void");
  emit " ";
  emit type_name;
  emit "_";
  emit method_name;
  emit "(const ";
  emit type_name;
  emit "* self";
  List.iter (function
    | PSelf -> ()
    | PNamed (t, name) ->
      emit ", ";
      emit (c_type t);
      emit " ";
      emit name
  ) params;
  emitln ");"

(* Generate program *)
let generate env program =
  Buffer.clear buf;
  let ctx = create_ctx env in
  gen_prelude ();

  (* Forward declarations for structs *)
  List.iter (function
    | IStruct (name, _) ->
      emit "typedef struct ";
      emit name;
      emit " ";
      emit name;
      emitln ";"
    | IError (name, _) ->
      emit "typedef struct ";
      emit name;
      emit " ";
      emit name;
      emitln ";"
    | IUses _ -> ()  (* Imports handled elsewhere *)
    | _ -> ()
  ) program;

  (* Collect struct types used in arrays for DECLARE_ARRAY *)
  let user_array_types = ref [] in
  let collect_array_types = function
    | IStruct (name, _) -> user_array_types := name :: !user_array_types
    | IUses _ -> ()  (* Skip imports *)
    | _ -> ()
  in
  List.iter collect_array_types program;

  (* Declare arrays for user types *)
  List.iter (fun name ->
    emit "typedef struct { ";
    emit name;
    emit "* data; size_t len; size_t cap; } Array_";
    emit name;
    emitln ";"
  ) !user_array_types;

  (* Generate struct definitions first *)
  List.iter (function
    | IStruct (name, fields) -> gen_struct name fields
    | IError (name, fields) -> gen_error name fields
    | _ -> ()
  ) program;

  (* Forward declarations for all functions and methods *)
  emitln "";
  emitln "/* Forward declarations */";
  List.iter (function
    | IFunction (name, params, ret, _) ->
      gen_function_decl name params ret
    | IMethod (type_name, method_name, params, ret, _) ->
      gen_method_decl type_name method_name params ret
    | IImpl (type_name, _, methods) ->
      List.iter (fun m ->
        gen_method_decl type_name m.im_name m.im_params m.im_return
      ) methods
    | IUses _ -> ()  (* Skip imports *)
    | _ -> ()
  ) program;

  (* Generate remaining items *)
  List.iter (function
    | IStruct _ | IError _ -> ()  (* Already generated *)
    | ITrait (name, methods) -> gen_trait name methods
    | IImpl (type_name, trait_name, methods) -> gen_impl ctx type_name trait_name methods
    | IMethod (type_name, method_name, params, ret, body) ->
      gen_method ctx type_name method_name params ret body
    | IFunction (name, params, ret, body) ->
      gen_function ctx name params ret body
    | IUses _ -> ()  (* Imports handled in multi-file compilation *)
  ) program;

  Buffer.contents buf
