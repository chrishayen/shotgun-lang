(* C Code Generation for Shotgun compiler *)

open Ast

(* Buffer for output *)
let buf = Buffer.create 4096

let emit s = Buffer.add_string buf s
let emitln s = Buffer.add_string buf s; Buffer.add_char buf '\n'
let emit_indent n = for _ = 1 to n do Buffer.add_string buf "    " done

(* Goroutine wrapper tracking *)
type go_info = {
  go_id: int;
  go_func: string;
  go_arg_types: typ list;
}

let go_counter = ref 0
let go_wrappers : go_info list ref = ref []

let reset_go_state () =
  go_counter := 0;
  go_wrappers := []

let next_go_id () =
  let id = !go_counter in
  go_counter := id + 1;
  id

(* Check if a type directly references a given type name (for recursive types) *)
let rec type_references_name name = function
  | TUser n -> n = name
  | TOptional t -> type_references_name name t
  | TArray _ -> false  (* Arrays are already pointers *)
  | _ -> false

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
  | TParam name -> name  (* Type parameter - should be substituted before codegen *)
  | TApply ("Map", _) -> "ShotgunMap*"  (* Maps use generic runtime *)
  | TApply (name, args) -> mangle_generic_name name args

(* Convert type to C, using pointers for recursive references to parent_name *)
and c_type_in_parent parent_name t =
  if type_references_name parent_name t then
    c_type t ^ "*"  (* Use pointer for recursive reference *)
  else
    c_type t

(* Mangle a generic type name: List<int> -> List_int *)
and mangle_generic_name name args =
  name ^ "_" ^ String.concat "_" (List.map c_type_name args)

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
  | TParam name -> name
  | TApply (name, args) -> mangle_generic_name name args

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
  mutable match_counter: int;
}

(* Track generic instantiations for monomorphization *)
module StringSet = Set.Make(String)
let generic_instantiations = ref StringSet.empty

(* Substitute type parameters in a type *)
let rec substitute_type subst = function
  | TUser name ->
    (match List.assoc_opt name subst with
     | Some t -> t
     | None -> TUser name)
  | TArray t -> TArray (substitute_type subst t)
  | TOptional t -> TOptional (substitute_type subst t)
  | TChan t -> TChan (substitute_type subst t)
  | TResult (t, e) -> TResult (substitute_type subst t, e)
  | TApply (name, args) -> TApply (name, List.map (substitute_type subst) args)
  | t -> t

(* Collect generic instantiation from a type *)
let rec collect_type_instantiation = function
  | TApply (name, args) ->
    let mangled = mangle_generic_name name args in
    generic_instantiations := StringSet.add mangled !generic_instantiations;
    List.iter collect_type_instantiation args
  | TArray t -> collect_type_instantiation t
  | TOptional t -> collect_type_instantiation t
  | TChan t -> collect_type_instantiation t
  | TResult (t, _) -> collect_type_instantiation t
  | _ -> ()

(* Collect instantiations from an expression *)
let rec collect_expr_instantiations = function
  | EStructLit (_, type_args, fields) ->
    List.iter collect_type_instantiation type_args;
    List.iter (fun (_, e) -> collect_expr_instantiations e) fields
  | EEnumVariant (_, type_args, _, fields) ->
    List.iter collect_type_instantiation type_args;
    List.iter (fun (_, e) -> collect_expr_instantiations e) fields
  | ECall (callee, type_args, args) ->
    collect_expr_instantiations callee;
    List.iter collect_type_instantiation type_args;
    List.iter collect_expr_instantiations args
  | EBinary (_, l, r) ->
    collect_expr_instantiations l;
    collect_expr_instantiations r
  | EUnary (_, e) -> collect_expr_instantiations e
  | EMember (e, _) -> collect_expr_instantiations e
  | EIndex (e, idx) ->
    collect_expr_instantiations e;
    collect_expr_instantiations idx
  | EOr (e, clause) ->
    collect_expr_instantiations e;
    (match clause with
     | OrExpr e2 -> collect_expr_instantiations e2
     | OrReturn (Some e2) -> collect_expr_instantiations e2
     | OrReturn None -> ()
     | OrError (_, fields) -> List.iter (fun (_, e2) -> collect_expr_instantiations e2) fields
     | OrWait e2 -> collect_expr_instantiations e2)
  | EArrayLit elems -> List.iter collect_expr_instantiations elems
  | EParen e -> collect_expr_instantiations e
  | EAssign (_, l, r) ->
    collect_expr_instantiations l;
    collect_expr_instantiations r
  | EString parts ->
    List.iter (function SLiteral _ -> () | SInterp e -> collect_expr_instantiations e) parts
  | EMatch (exprs, using_type, arms) ->
    List.iter collect_expr_instantiations exprs;
    (match using_type with Some t -> collect_type_instantiation t | None -> ());
    List.iter (fun (_, e) -> collect_expr_instantiations e) arms
  | _ -> ()

(* Collect instantiations from a statement *)
let rec collect_stmt_instantiations = function
  | SVarDecl (t, _, e) ->
    collect_type_instantiation t;
    collect_expr_instantiations e
  | SConstDecl (_, e) -> collect_expr_instantiations e
  | SReturn (Some e) -> collect_expr_instantiations e
  | SReturn None -> ()
  | SExpr e -> collect_expr_instantiations e
  | SIf (cond, then_stmts, else_stmts) ->
    collect_expr_instantiations cond;
    List.iter collect_stmt_instantiations then_stmts;
    (match else_stmts with Some stmts -> List.iter collect_stmt_instantiations stmts | None -> ())
  | SFor (_, iter, body) ->
    collect_expr_instantiations iter;
    List.iter collect_stmt_instantiations body
  | SGo e -> collect_expr_instantiations e

(* Collect all instantiations from the program *)
let collect_all_instantiations program =
  generic_instantiations := StringSet.empty;
  List.iter (function
    | IFunction (_, _type_params, params, ret, body) ->
      List.iter (function PSelf -> () | PNamed (t, _) -> collect_type_instantiation t) params;
      (match ret with Some t -> collect_type_instantiation t | None -> ());
      List.iter collect_stmt_instantiations body
    | IMethod (_, _, _type_params, params, ret, body) ->
      List.iter (function PSelf -> () | PNamed (t, _) -> collect_type_instantiation t) params;
      (match ret with Some t -> collect_type_instantiation t | None -> ());
      List.iter collect_stmt_instantiations body
    | IImpl (_, _, methods) ->
      List.iter (fun m ->
        List.iter (function PSelf -> () | PNamed (t, _) -> collect_type_instantiation t) m.im_params;
        (match m.im_return with Some t -> collect_type_instantiation t | None -> ());
        List.iter collect_stmt_instantiations m.im_body
      ) methods
    | _ -> ()
  ) program

let create_ctx env = {
  env;
  locals = Hashtbl.create 32;
  in_method = false;
  current_type = None;
  match_counter = 0;
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
  | ECall (callee, _type_args, args) ->
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
    (match get_type ctx arr with
     | Some (TApply ("Map", [_k; v])) ->
       (* Map indexing: dereference void pointer from shotgun_map_get *)
       emit "*(";
       emit (c_type v);
       emit "*)shotgun_map_get(";
       gen_expr ctx indent arr;
       emit ", ";
       gen_expr ctx indent idx;
       emit ")"
     | _ ->
       (* Array indexing *)
       gen_expr ctx indent arr;
       emit "->data[";
       gen_expr ctx indent idx;
       emit "]")
  | EOr (e, clause) ->
    gen_or ctx indent e clause
  | EStructLit ("Map", type_args, _fields) ->
    (* Map<K, V>{} creates an empty map *)
    (match type_args with
     | [_k; v] ->
       emit "shotgun_map_create(sizeof(";
       emit (c_type v);
       emit "))"
     | _ -> emit "shotgun_map_create(sizeof(int64_t))")
  | EStructLit (name, type_args, fields) ->
    let mangled = if type_args = [] then name else mangle_generic_name name type_args in
    emit "(";
    emit mangled;
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
  | EEnumVariant (enum_name, type_args, variant_name, fields) ->
    let mangled = if type_args = [] then enum_name else mangle_generic_name enum_name type_args in
    (* Look up variant field types to detect recursive fields *)
    let field_types =
      match Hashtbl.find_opt ctx.env.Semantic.enums enum_name with
      | Some variants ->
        (match List.find_opt (fun v -> v.variant_name = variant_name) variants with
         | Some v -> List.map (fun f -> (f.field_name, f.field_type)) v.variant_fields
         | None -> [])
      | None -> []
    in
    emit "(";
    emit mangled;
    emit "){ .tag = ";
    emit mangled;
    emit "_";
    emit variant_name;
    if fields <> [] then begin
      emit ", .data.";
      emit variant_name;
      emit " = (";
      emit mangled;
      emit "_";
      emit variant_name;
      emit "_data){ ";
      let first = ref true in
      List.iter (fun (fname, fexpr) ->
        if not !first then emit ", ";
        first := false;
        emit ".";
        emit fname;
        emit " = ";
        (* Check if this field is recursive (references parent enum) *)
        let field_type = List.assoc_opt fname field_types in
        let is_recursive = match field_type with
          | Some ft -> type_references_name enum_name ft
          | None -> false
        in
        if is_recursive then begin
          (* Allocate memory for recursive field *)
          (* For simple identifiers, take address directly *)
          (match fexpr with
           | EIdent _ ->
             emit "shotgun_alloc_copy(sizeof(";
             emit mangled;
             emit "), &";
             gen_expr ctx indent fexpr;
             emit ")"
           | _ ->
             (* For complex expressions, use statement expression to create addressable temp *)
             emit "({ ";
             emit mangled;
             emit " _tmp = ";
             gen_expr ctx indent fexpr;
             emit "; shotgun_alloc_copy(sizeof(";
             emit mangled;
             emit "), &_tmp); })")
        end else
          gen_expr ctx indent fexpr
      ) fields;
      emit " }"
    end;
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
    (match lhs with
     | EIndex (map_expr, key_expr) ->
       (match get_type ctx map_expr with
        | Some (TApply ("Map", [_k; v])) when op = Assign ->
          (* Map assignment: shotgun_map_set(m, key, &(V){value}) *)
          emit "shotgun_map_set(";
          gen_expr ctx indent map_expr;
          emit ", ";
          gen_expr ctx indent key_expr;
          emit ", &(";
          emit (c_type v);
          emit "){";
          gen_expr ctx indent rhs;
          emit "})"
        | _ ->
          gen_expr ctx indent lhs;
          emit " ";
          emit (c_assignop op);
          emit " ";
          gen_expr ctx indent rhs)
     | _ ->
       gen_expr ctx indent lhs;
       emit " ";
       emit (c_assignop op);
       emit " ";
       gen_expr ctx indent rhs)
  | EMatch (exprs, using_type, arms) ->
    gen_match_expr ctx indent exprs using_type arms

(* Get binding types from a pattern, given the using_type for variants *)
and get_pattern_binding_types ctx using_type pat =
  match pat with
  | PWildcard | PLiteral _ -> []
  | PIdent name ->
    (* Single identifier pattern - type is the match expression type *)
    (match using_type with Some t -> [(name, t)] | None -> [])
  | PVariant (enum_type_opt, variant_name, bindings) ->
    let enum_type = match enum_type_opt with
      | Some t -> Some t
      | None -> using_type
    in
    (match enum_type with
     | Some (TApply (enum_name, type_args)) ->
       (* Look up the variant's fields in the generic enum definition *)
       (match Hashtbl.find_opt ctx.env.Semantic.enums enum_name with
        | Some variant_list ->
          (* Get type params from the generic_enum_params hashtable *)
          let type_params = match Hashtbl.find_opt ctx.env.Semantic.generic_enum_params enum_name with
            | Some params -> params
            | None -> []
          in
          (* Find the variant *)
          (match List.find_opt (fun v -> v.Ast.variant_name = variant_name) variant_list with
           | Some v ->
             (* For each binding, find the field type and substitute *)
             List.filter_map (fun (field_name, binding_name) ->
               match List.find_opt (fun f -> f.Ast.field_name = field_name) v.Ast.variant_fields with
               | Some f ->
                 let field_type = f.Ast.field_type in
                 (* Substitute type parameters: if field_type is TUser "T" and type_args is [TInt], use TInt *)
                 let subst_type =
                   match field_type with
                   | TUser param_name ->
                     (* Find which type param this is and get corresponding type arg *)
                     (try
                       let idx = List.mapi (fun i p -> (i, p)) type_params |>
                                 List.find (fun (_, p) -> p = param_name) |> fst in
                       List.nth type_args idx
                     with _ -> field_type)
                   | _ -> field_type
                 in
                 Some (binding_name, subst_type)
               | None -> None
             ) bindings
           | None -> [])
        | None -> [])
     | Some (TUser enum_name) ->
       (* Non-generic enum - just look up field types directly *)
       (match Hashtbl.find_opt ctx.env.Semantic.enums enum_name with
        | Some variant_list ->
          (match List.find_opt (fun v -> v.Ast.variant_name = variant_name) variant_list with
           | Some v ->
             List.filter_map (fun (field_name, binding_name) ->
               match List.find_opt (fun f -> f.Ast.field_name = field_name) v.Ast.variant_fields with
               | Some f -> Some (binding_name, f.Ast.field_type)
               | None -> None
             ) bindings
           | None -> [])
        | None -> [])
     | _ -> [])
  | PTuple pats ->
    List.concat_map (fun p -> get_pattern_binding_types ctx using_type p) pats

(* Generate match expression *)
and gen_match_expr ctx indent exprs using_type arms =
  (* For now, generate a simple if-else chain for patterns *)
  (* TODO: Optimize to switch for single enum match *)
  let match_id = ctx.match_counter in
  ctx.match_counter <- ctx.match_counter + 1;

  (* Generate GCC statement expression: ({ ... result; }) *)
  emit "({";
  emitln "";

  (* Store matched values in temp variables *)
  List.iteri (fun i e ->
    emit_indent (indent + 1);
    emit "typeof(";
    gen_expr ctx indent e;
    emit ") _match";
    emit (string_of_int match_id);
    emit "_";
    emit (string_of_int i);
    emit " = ";
    gen_expr ctx indent e;
    emitln ";"
  ) exprs;

  (* Declare result variable - we'll determine type from first arm *)
  emit_indent (indent + 1);
  (match arms with
   | [] -> emit "int _match_result = 0"
   | (first_pat, first_result) :: _ ->
     (* Add pattern bindings to ctx.locals temporarily to get correct type *)
     let bindings = get_pattern_binding_types ctx using_type first_pat in
     List.iter (fun (name, typ) -> add_local ctx name typ) bindings;
     let result_type = get_type ctx first_result in
     let c_t = match result_type with
       | Some t -> c_type t
       | None -> "int64_t"
     in
     emit c_t;
     emit " _match_result";
     emit (string_of_int match_id));
  emitln ";";

  (* Generate pattern matching logic *)
  let rec gen_arms first_arm = function
    | [] -> ()
    | (pat, result) :: rest ->
      emit_indent (indent + 1);
      if not first_arm then emit "else ";
      gen_pattern_condition ctx indent match_id exprs using_type pat;
      emit " {";
      emitln "";
      (* Bind pattern variables *)
      gen_pattern_bindings ctx (indent + 2) match_id exprs using_type pat;
      emit_indent (indent + 2);
      emit "_match_result";
      emit (string_of_int match_id);
      emit " = ";
      gen_expr ctx (indent + 2) result;
      emitln ";";
      emit_indent (indent + 1);
      emitln "}";
      gen_arms false rest
  in
  gen_arms true arms;

  (* Return the result *)
  emit_indent (indent + 1);
  emit "_match_result";
  emit (string_of_int match_id);
  emitln ";";
  emit_indent indent;
  emit "})"

(* Get mangled enum name from pattern type or using type *)
and get_enum_c_name = function
  | TUser name -> name
  | TApply (name, args) -> mangle_generic_name name args
  | _ -> "UNKNOWN"

(* Generate pattern matching condition *)
and gen_pattern_condition ctx indent match_id _exprs using_type pat =
  match pat with
  | PWildcard -> emit "if (1)"  (* Always matches *)
  | PIdent _ -> emit "if (1)"  (* Identifier always matches, just binds *)
  | PLiteral lit ->
    emit "if (_match";
    emit (string_of_int match_id);
    emit "_0 == ";
    gen_expr ctx indent lit;
    emit ")"
  | PVariant (enum_type_opt, variant_name, _bindings) ->
    let enum_name = match enum_type_opt with
      | Some t -> get_enum_c_name t
      | None -> (match using_type with Some t -> get_enum_c_name t | None -> "UNKNOWN")
    in
    emit "if (_match";
    emit (string_of_int match_id);
    emit "_0.tag == ";
    emit enum_name;
    emit "_";
    emit variant_name;
    emit ")"
  | PTuple patterns ->
    emit "if (";
    let rec gen_tuple_conds i = function
      | [] -> ()
      | p :: rest ->
        if i > 0 then emit " && ";
        (match p with
         | PWildcard -> emit "1"
         | PIdent _ -> emit "1"
         | PLiteral lit ->
           emit "_match";
           emit (string_of_int match_id);
           emit "_";
           emit (string_of_int i);
           emit " == ";
           gen_expr ctx indent lit
         | PVariant (enum_type_opt, variant_name, _) ->
           let enum_name = match enum_type_opt with Some t -> get_enum_c_name t | None -> (match using_type with Some t -> get_enum_c_name t | None -> "UNKNOWN") in
           emit "_match";
           emit (string_of_int match_id);
           emit "_";
           emit (string_of_int i);
           emit ".tag == ";
           emit enum_name;
           emit "_";
           emit variant_name
         | PTuple _ -> emit "1"  (* Nested tuples not yet supported *)
        );
        gen_tuple_conds (i + 1) rest
    in
    gen_tuple_conds 0 patterns;
    emit ")"

(* Generate pattern variable bindings *)
and gen_pattern_bindings _ctx indent match_id _exprs using_type pat =
  match pat with
  | PWildcard | PLiteral _ -> ()
  | PIdent name ->
    emit_indent indent;
    emit "typeof(_match";
    emit (string_of_int match_id);
    emit "_0) ";
    emit name;
    emit " = _match";
    emit (string_of_int match_id);
    emitln "_0;"
  | PVariant (enum_type_opt, variant_name, bindings) ->
    let _enum_name = match enum_type_opt with Some t -> get_enum_c_name t | None -> (match using_type with Some t -> get_enum_c_name t | None -> "UNKNOWN") in
    List.iter (fun (field_name, binding_name) ->
      emit_indent indent;
      emit "typeof(_match";
      emit (string_of_int match_id);
      emit "_0.data.";
      emit variant_name;
      emit ".";
      emit field_name;
      emit ") ";
      emit binding_name;
      emit " = _match";
      emit (string_of_int match_id);
      emit "_0.data.";
      emit variant_name;
      emit ".";
      emit field_name;
      emitln ";"
    ) bindings
  | PTuple patterns ->
    List.iteri (fun i p ->
      match p with
      | PWildcard | PLiteral _ -> ()
      | PIdent name ->
        emit_indent indent;
        emit "typeof(_match";
        emit (string_of_int match_id);
        emit "_";
        emit (string_of_int i);
        emit ") ";
        emit name;
        emit " = _match";
        emit (string_of_int match_id);
        emit "_";
        emit (string_of_int i);
        emitln ";"
      | PVariant (enum_type_opt, variant_name, bindings) ->
        let _enum_name = match enum_type_opt with Some t -> get_enum_c_name t | None -> (match using_type with Some t -> get_enum_c_name t | None -> "UNKNOWN") in
        List.iter (fun (field_name, binding_name) ->
          emit_indent indent;
          emit "typeof(_match";
          emit (string_of_int match_id);
          emit "_";
          emit (string_of_int i);
          emit ".data.";
          emit variant_name;
          emit ".";
          emit field_name;
          emit ") ";
          emit binding_name;
          emit " = _match";
          emit (string_of_int match_id);
          emit "_";
          emit (string_of_int i);
          emit ".data.";
          emit variant_name;
          emit ".";
          emit field_name;
          emitln ";"
        ) bindings
      | PTuple _ -> ()  (* Nested tuples not supported yet *)
    ) patterns

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
       let elem_type_name = match get_type ctx obj with
         | Some (TChan t) -> c_type_name t
         | _ -> "int64"  (* fallback *)
       in
       emit "chan_send_";
       emit elem_type_name;
       emit "(";
       gen_expr ctx indent obj;
       emit ", ";
       (match args with
        | [arg] -> gen_expr ctx indent arg
        | _ -> emit "/* invalid args */");
       emit ")"
     | "recv" ->
       let elem_type_name = match get_type ctx obj with
         | Some (TChan t) -> c_type_name t
         | _ -> "int64"  (* fallback *)
       in
       emit "chan_recv_";
       emit elem_type_name;
       emit "(";
       gen_expr ctx indent obj;
       emit ")"
     (* Map methods *)
     | "has" ->
       (match get_type ctx obj with
        | Some (TApply ("Map", _)) ->
          emit "shotgun_map_has(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ")"
        | _ -> emit "/* has on non-map */")
     | "len" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "(int64_t)strlen(";
          gen_expr ctx indent obj;
          emit ")"
        | Some (TApply ("Map", _)) ->
          emit "(int64_t)shotgun_map_len(";
          gen_expr ctx indent obj;
          emit ")"
        | Some (TArray _) ->
          emit "(int64_t)";
          gen_expr ctx indent obj;
          emit "->len"
        | _ -> emit "/* len on unknown type */")
     | "delete" ->
       (match get_type ctx obj with
        | Some (TApply ("Map", _)) ->
          emit "shotgun_map_delete(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ")"
        | _ -> emit "/* delete on non-map */")
     | "get" ->
       (match get_type ctx obj with
        | Some (TApply ("Map", [_k; v])) ->
          emit "*(";
          emit (c_type v);
          emit "*)shotgun_map_get(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ")"
        | _ -> emit "/* get on non-map */")
     | "set" ->
       (match get_type ctx obj with
        | Some (TApply ("Map", [_k; v])) ->
          emit "shotgun_map_set(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with
           | [key; value] ->
             gen_expr ctx indent key;
             emit ", &(";
             emit (c_type v);
             emit "){";
             gen_expr ctx indent value;
             emit "})"
           | _ -> emit "/* invalid set args */)")
        | _ -> emit "/* set on non-map */")
     (* String methods *)
     | "at" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "(int64_t)(";
          gen_expr ctx indent obj;
          emit ")[";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit "]"
        | _ -> emit "/* at on non-string */")
     | "contains" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "(strstr(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ") != NULL)"
        | _ -> emit "/* contains on non-string */")
     | "starts_with" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "(strncmp(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ", strlen(";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ")) == 0)"
        | _ -> emit "/* starts_with on non-string */")
     | "ends_with" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_ends_with(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ")"
        | _ -> emit "/* ends_with on non-string */")
     | "find" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_find(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ")"
        | _ -> emit "/* find on non-string */")
     | "slice" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_slice(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with
           | [start; end_] ->
             gen_expr ctx indent start;
             emit ", ";
             gen_expr ctx indent end_
           | _ -> ());
          emit ")"
        | _ -> emit "/* slice on non-string */")
     | "split" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_split(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with [arg] -> gen_expr ctx indent arg | _ -> ());
          emit ")"
        | _ -> emit "/* split on non-string */")
     | "trim" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_trim(";
          gen_expr ctx indent obj;
          emit ")"
        | _ -> emit "/* trim on non-string */")
     | "replace" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_replace(";
          gen_expr ctx indent obj;
          emit ", ";
          (match args with
           | [old_s; new_s] ->
             gen_expr ctx indent old_s;
             emit ", ";
             gen_expr ctx indent new_s
           | _ -> ());
          emit ")"
        | _ -> emit "/* replace on non-string */")
     | "to_upper" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_to_upper(";
          gen_expr ctx indent obj;
          emit ")"
        | _ -> emit "/* to_upper on non-string */")
     | "to_lower" ->
       (match get_type ctx obj with
        | Some TStr ->
          emit "shotgun_str_to_lower(";
          gen_expr ctx indent obj;
          emit ")"
        | _ -> emit "/* to_lower on non-string */")
     | _ ->
       (* Get the type of obj to generate proper method name *)
       let type_name = match get_type ctx obj with
         | Some (TUser name) -> name
         | Some (TApply (name, type_args)) -> mangle_generic_name name type_args
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
          emit "printf(\"%s\\n\", (";
          gen_expr ctx indent arg;
          emit ") ? \"true\" : \"false\")"
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
    (* Special case: typed channel creation *)
    (match typ, expr with
     | TChan elem_typ, EChan ->
       emit "chan_create_";
       emit (c_type_name elem_typ);
       emit "()"
     | _ -> gen_expr ctx indent expr);
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
  | SGo e ->
    (* Spawn a goroutine using pthreads *)
    (match e with
     | ECall (EIdent _, _type_args, args) ->
       (* Use the pre-scanned go_id (must match scan order) *)
       let go_id = next_go_id () in

       (* Emit pthread spawn code *)
       emitln "{";
       emit_indent (indent + 1);
       emit "_go_";
       emit (string_of_int go_id);
       emit "_args* _args = malloc(sizeof(_go_";
       emit (string_of_int go_id);
       emitln "_args));";

       (* Assign arguments *)
       List.iteri (fun i arg ->
         emit_indent (indent + 1);
         emit "_args->arg";
         emit (string_of_int i);
         emit " = ";
         gen_expr ctx indent arg;
         emitln ";"
       ) args;

       emit_indent (indent + 1);
       emitln "pthread_t _t;";
       emit_indent (indent + 1);
       emit "pthread_create(&_t, NULL, _go_";
       emit (string_of_int go_id);
       emitln "_wrapper, _args);";
       emit_indent (indent + 1);
       emitln "pthread_detach(_t);";
       emit_indent indent;
       emitln "}"
     | _ ->
       (* Fallback for complex expressions *)
       emitln "/* unsupported go expression */";
       emit_indent indent;
       gen_expr ctx indent e;
       emitln ";")
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

(* Generate enum as C tagged union *)
let gen_enum name variants =
  emitln "";
  (* Generate tag enum *)
  emit "typedef enum { ";
  let first = ref true in
  List.iter (fun v ->
    if not !first then emit ", ";
    first := false;
    emit name;
    emit "_";
    emit v.variant_name
  ) variants;
  emit " } ";
  emit name;
  emitln "_tag;";
  emitln "";

  (* Generate data struct for each variant with fields *)
  List.iter (fun v ->
    if v.variant_fields <> [] then begin
      emit "typedef struct { ";
      List.iter (fun f ->
        emit (c_type_in_parent name f.field_type);
        emit " ";
        emit f.field_name;
        emit "; "
      ) v.variant_fields;
      emit "} ";
      emit name;
      emit "_";
      emit v.variant_name;
      emitln "_data;"
    end
  ) variants;

  (* Generate the main enum struct with union *)
  emit "typedef struct ";
  emit name;
  emitln " {";
  emit_indent 1;
  emit name;
  emitln "_tag tag;";

  (* Only generate union if there are variants with data *)
  let has_data = List.exists (fun v -> v.variant_fields <> []) variants in
  if has_data then begin
    emit_indent 1;
    emitln "union {";
    List.iter (fun v ->
      if v.variant_fields <> [] then begin
        emit_indent 2;
        emit name;
        emit "_";
        emit v.variant_name;
        emit "_data ";
        emit v.variant_name;
        emitln ";"
      end
    ) variants;
    emit_indent 1;
    emitln "} data;"
  end;

  emit "} ";
  emit name;
  emitln ";"

(* Generate monomorphized struct: Box<int> -> Box_int64 with T->int64_t *)
let gen_monomorphized_struct base_name type_params type_args fields =
  let mangled = mangle_generic_name base_name type_args in
  let subst = List.combine type_params type_args in
  emitln "";
  emit "typedef struct ";
  emit mangled;
  emitln " {";
  List.iter (fun f ->
    emit_indent 1;
    let substituted_type = substitute_type subst f.field_type in
    emit (c_type substituted_type);
    emit " ";
    emit f.field_name;
    emitln ";"
  ) fields;
  emit "} ";
  emit mangled;
  emitln ";";
  (* Also generate array type for this monomorphized struct *)
  emit "typedef struct { ";
  emit mangled;
  emit "* data; size_t len; size_t cap; } Array_";
  emit mangled;
  emitln ";"

(* Generate monomorphized enum: Option<int> -> Option_int64 with T->int64_t *)
let gen_monomorphized_enum base_name type_params type_args variants =
  let mangled = mangle_generic_name base_name type_args in
  let subst = List.combine type_params type_args in
  emitln "";
  (* Generate tag enum *)
  emit "typedef enum { ";
  let first = ref true in
  List.iter (fun v ->
    if not !first then emit ", ";
    first := false;
    emit mangled;
    emit "_";
    emit v.variant_name
  ) variants;
  emit " } ";
  emit mangled;
  emitln "_tag;";
  emitln "";

  (* Generate data struct for each variant with fields *)
  List.iter (fun v ->
    if v.variant_fields <> [] then begin
      emit "typedef struct { ";
      List.iter (fun f ->
        let substituted_type = substitute_type subst f.field_type in
        emit (c_type substituted_type);
        emit " ";
        emit f.field_name;
        emit "; "
      ) v.variant_fields;
      emit "} ";
      emit mangled;
      emit "_";
      emit v.variant_name;
      emitln "_data;"
    end
  ) variants;

  (* Generate the main enum struct with union *)
  emit "typedef struct ";
  emit mangled;
  emitln " {";
  emit_indent 1;
  emit mangled;
  emitln "_tag tag;";

  (* Only generate union if there are variants with data *)
  let has_data = List.exists (fun v -> v.variant_fields <> []) variants in
  if has_data then begin
    emit_indent 1;
    emitln "union {";
    List.iter (fun v ->
      if v.variant_fields <> [] then begin
        emit_indent 2;
        emit mangled;
        emit "_";
        emit v.variant_name;
        emit "_data ";
        emit v.variant_name;
        emitln ";"
      end
    ) variants;
    emit_indent 1;
    emitln "} data;"
  end;

  emit "} ";
  emit mangled;
  emitln ";"

(* Generate a monomorphized method *)
let gen_monomorphized_method ctx base_name type_params type_args method_name params ret body =
  let mangled = mangle_generic_name base_name type_args in
  let subst = List.combine type_params type_args in
  let method_ctx = with_method_ctx ctx mangled in
  add_local method_ctx "self" (TApply (base_name, type_args));
  emitln "";
  (match ret with
   | Some t -> emit (c_type (substitute_type subst t))
   | None -> emit "void");
  emit " ";
  emit mangled;
  emit "_";
  emit method_name;
  emit "(const ";
  emit mangled;
  emit "* self";
  List.iter (function
    | PSelf -> ()
    | PNamed (t, name) ->
      let subst_t = substitute_type subst t in
      add_local method_ctx name subst_t;
      emit ", ";
      emit (c_type subst_t);
      emit " ";
      emit name
  ) params;
  emitln ") {";
  List.iter (gen_stmt method_ctx 1) body;
  emitln "}"

(* Generate all monomorphized types based on collected instantiations *)
let gen_monomorphized_types program =
  (* Build a map of generic definitions *)
  let generic_structs = Hashtbl.create 16 in
  let generic_enums = Hashtbl.create 16 in
  List.iter (function
    | IStruct (name, type_params, fields) when type_params <> [] ->
      Hashtbl.add generic_structs name (type_params, fields)
    | IEnum (name, type_params, variants) when type_params <> [] ->
      Hashtbl.add generic_enums name (type_params, variants)
    | _ -> ()
  ) program;

  (* Iterate over program and find TApply usages to generate monomorphized types *)
  let generated = Hashtbl.create 16 in
  let rec gen_for_type = function
    | TApply (name, type_args) ->
      let mangled = mangle_generic_name name type_args in
      if not (Hashtbl.mem generated mangled) then begin
        Hashtbl.add generated mangled ();
        (* Recursively generate for nested type args *)
        List.iter gen_for_type type_args;
        (* Generate the monomorphized type *)
        (match Hashtbl.find_opt generic_structs name with
         | Some (type_params, fields) ->
           (* Forward declaration *)
           emit "typedef struct "; emit mangled; emit " "; emit mangled; emitln ";";
           gen_monomorphized_struct name type_params type_args fields
         | None ->
           match Hashtbl.find_opt generic_enums name with
           | Some (type_params, variants) ->
             emit "typedef struct "; emit mangled; emit " "; emit mangled; emitln ";";
             gen_monomorphized_enum name type_params type_args variants
           | None -> ())
      end
    | TArray t -> gen_for_type t
    | TOptional t -> gen_for_type t
    | TChan t -> gen_for_type t
    | TResult (t, _) -> gen_for_type t
    | _ -> ()
  in
  let scan_type t = gen_for_type t in
  let rec scan_expr = function
    | EStructLit (_, type_args, fields) ->
      List.iter gen_for_type type_args;
      List.iter (fun (_, e) -> scan_expr e) fields
    | EEnumVariant (_, type_args, _, fields) ->
      List.iter gen_for_type type_args;
      List.iter (fun (_, e) -> scan_expr e) fields
    | ECall (callee, type_args, args) ->
      scan_expr callee;
      List.iter gen_for_type type_args;
      List.iter scan_expr args
    | EBinary (_, l, r) -> scan_expr l; scan_expr r
    | EUnary (_, e) -> scan_expr e
    | EMember (e, _) -> scan_expr e
    | EIndex (e, idx) -> scan_expr e; scan_expr idx
    | EOr (e, clause) ->
      scan_expr e;
      (match clause with
       | OrExpr e2 -> scan_expr e2
       | OrReturn (Some e2) -> scan_expr e2
       | OrReturn None -> ()
       | OrError (_, fields) -> List.iter (fun (_, e2) -> scan_expr e2) fields
       | OrWait e2 -> scan_expr e2)
    | EArrayLit elems -> List.iter scan_expr elems
    | EParen e -> scan_expr e
    | EAssign (_, l, r) -> scan_expr l; scan_expr r
    | EString parts ->
      List.iter (function SLiteral _ -> () | SInterp e -> scan_expr e) parts
    | EMatch (exprs, using_type, arms) ->
      List.iter scan_expr exprs;
      (match using_type with Some t -> scan_type t | None -> ());
      List.iter (fun (_, e) -> scan_expr e) arms
    | _ -> ()
  in
  let rec scan_stmt = function
    | SVarDecl (t, _, e) -> scan_type t; scan_expr e
    | SConstDecl (_, e) -> scan_expr e
    | SReturn (Some e) -> scan_expr e
    | SReturn None -> ()
    | SExpr e -> scan_expr e
    | SIf (cond, then_stmts, else_stmts) ->
      scan_expr cond;
      List.iter scan_stmt then_stmts;
      (match else_stmts with Some stmts -> List.iter scan_stmt stmts | None -> ())
    | SFor (_, iter, body) -> scan_expr iter; List.iter scan_stmt body
    | SGo e -> scan_expr e
  in
  List.iter (function
    | IStruct (_, type_params, fields) when type_params = [] ->
      (* Scan non-generic struct fields for generic type references *)
      List.iter (fun f -> scan_type f.field_type) fields
    | IEnum (_, type_params, variants) when type_params = [] ->
      (* Scan non-generic enum variant fields for generic type references *)
      List.iter (fun v ->
        List.iter (fun f -> scan_type f.field_type) v.variant_fields
      ) variants
    | IFunction (_, _type_params, params, ret, body) ->
      List.iter (function PSelf -> () | PNamed (t, _) -> scan_type t) params;
      (match ret with Some t -> scan_type t | None -> ());
      List.iter scan_stmt body
    | IMethod (_, _, _type_params, params, ret, body) ->
      List.iter (function PSelf -> () | PNamed (t, _) -> scan_type t) params;
      (match ret with Some t -> scan_type t | None -> ());
      List.iter scan_stmt body
    | IImpl (_, _, methods) ->
      List.iter (fun m ->
        List.iter (function PSelf -> () | PNamed (t, _) -> scan_type t) m.im_params;
        (match m.im_return with Some t -> scan_type t | None -> ());
        List.iter scan_stmt m.im_body
      ) methods
    | _ -> ()
  ) program

(* Helper to generate monomorphized method forward declaration *)
let gen_monomorphized_method_decl base_name type_params type_args method_name params ret =
  let mangled = mangle_generic_name base_name type_args in
  let subst = List.combine type_params type_args in
  (match ret with
   | Some t -> emit (c_type (substitute_type subst t))
   | None -> emit "void");
  emit " ";
  emit mangled;
  emit "_";
  emit method_name;
  emit "(const ";
  emit mangled;
  emit "* self";
  List.iter (function
    | PSelf -> ()
    | PNamed (t, _) ->
      let subst_t = substitute_type subst t in
      emit ", ";
      emit (c_type subst_t)
  ) params;
  emitln ");"

(* Generate all monomorphized methods based on collected instantiations *)
let gen_monomorphized_methods ?(decl_only=false) ctx program =
  (* Build a map of generic methods: type_name -> [(method_name, type_params, params, ret, body)] *)
  let generic_methods = Hashtbl.create 16 in
  List.iter (function
    | IMethod (type_name, method_name, type_params, params, ret, body) when type_params <> [] ->
      let methods = try Hashtbl.find generic_methods type_name with Not_found -> [] in
      Hashtbl.replace generic_methods type_name ((method_name, type_params, params, ret, body) :: methods)
    | _ -> ()
  ) program;

  (* Track which methods we've already generated *)
  let generated = Hashtbl.create 16 in

  (* Scan for TApply usages and generate methods *)
  let rec gen_for_type = function
    | TApply (name, type_args) ->
      let mangled = mangle_generic_name name type_args in
      let key = mangled in
      if not (Hashtbl.mem generated key) then begin
        Hashtbl.add generated key ();
        (* Generate methods for this type *)
        (match Hashtbl.find_opt generic_methods name with
         | Some methods ->
           List.iter (fun (method_name, type_params, params, ret, body) ->
             if decl_only then
               gen_monomorphized_method_decl name type_params type_args method_name params ret
             else
               gen_monomorphized_method ctx name type_params type_args method_name params ret body
           ) methods
         | None -> ())
      end
    | TArray t -> gen_for_type t
    | TOptional t -> gen_for_type t
    | TChan t -> gen_for_type t
    | TResult (t, _) -> gen_for_type t
    | _ -> ()
  in
  let scan_type t = gen_for_type t in
  let rec scan_expr = function
    | EStructLit (_, type_args, fields) ->
      List.iter gen_for_type type_args;
      List.iter (fun (_, e) -> scan_expr e) fields
    | EEnumVariant (_, type_args, _, fields) ->
      List.iter gen_for_type type_args;
      List.iter (fun (_, e) -> scan_expr e) fields
    | ECall (callee, type_args, args) ->
      scan_expr callee;
      List.iter gen_for_type type_args;
      List.iter scan_expr args
    | EBinary (_, l, r) -> scan_expr l; scan_expr r
    | EUnary (_, e) -> scan_expr e
    | EMember (e, _) -> scan_expr e
    | EIndex (e, idx) -> scan_expr e; scan_expr idx
    | EOr (e, clause) ->
      scan_expr e;
      (match clause with
       | OrExpr e2 -> scan_expr e2
       | OrReturn (Some e2) -> scan_expr e2
       | OrReturn None -> ()
       | OrError (_, fields) -> List.iter (fun (_, e2) -> scan_expr e2) fields
       | OrWait e2 -> scan_expr e2)
    | EArrayLit elems -> List.iter scan_expr elems
    | EParen e -> scan_expr e
    | EAssign (_, l, r) -> scan_expr l; scan_expr r
    | EString parts ->
      List.iter (function SLiteral _ -> () | SInterp e -> scan_expr e) parts
    | EMatch (exprs, using_type, arms) ->
      List.iter scan_expr exprs;
      (match using_type with Some t -> scan_type t | None -> ());
      List.iter (fun (_, e) -> scan_expr e) arms
    | _ -> ()
  in
  let rec scan_stmt = function
    | SVarDecl (t, _, e) -> scan_type t; scan_expr e
    | SConstDecl (_, e) -> scan_expr e
    | SReturn (Some e) -> scan_expr e
    | SReturn None -> ()
    | SExpr e -> scan_expr e
    | SIf (cond, then_stmts, else_stmts) ->
      scan_expr cond;
      List.iter scan_stmt then_stmts;
      (match else_stmts with Some stmts -> List.iter scan_stmt stmts | None -> ())
    | SFor (_, iter, body) -> scan_expr iter; List.iter scan_stmt body
    | SGo e -> scan_expr e
  in
  List.iter (function
    | IFunction (_, _type_params, params, ret, body) ->
      List.iter (function PSelf -> () | PNamed (t, _) -> scan_type t) params;
      (match ret with Some t -> scan_type t | None -> ());
      List.iter scan_stmt body
    | IMethod (_, _, _type_params, params, ret, body) ->
      List.iter (function PSelf -> () | PNamed (t, _) -> scan_type t) params;
      (match ret with Some t -> scan_type t | None -> ());
      List.iter scan_stmt body
    | IImpl (_, _, methods) ->
      List.iter (fun m ->
        List.iter (function PSelf -> () | PNamed (t, _) -> scan_type t) m.im_params;
        (match m.im_return with Some t -> scan_type t | None -> ());
        List.iter scan_stmt m.im_body
      ) methods
    | _ -> ()
  ) program

(* Generate trait definition - emits a comment for documentation.
   Trait implementations (IImpl) generate the actual method code. *)
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
  emitln "/* Allocation helper for recursive types */";
  emitln "static void* shotgun_alloc_copy(size_t size, void* src) {";
  emitln "    void* p = malloc(size);";
  emitln "    memcpy(p, src, size);";
  emitln "    return p;";
  emitln "}";
  emitln "";
  emitln "/* String runtime */";
  emitln "static char* shotgun_str_slice(char* s, int64_t start, int64_t end) {";
  emitln "    int64_t len = end - start;";
  emitln "    char* result = malloc(len + 1);";
  emitln "    memcpy(result, s + start, len);";
  emitln "    result[len] = '\\0';";
  emitln "    return result;";
  emitln "}";
  emitln "";
  emitln "static int64_t shotgun_str_find(char* s, char* substr) {";
  emitln "    char* pos = strstr(s, substr);";
  emitln "    return pos ? (int64_t)(pos - s) : -1;";
  emitln "}";
  emitln "";
  emitln "static bool shotgun_str_ends_with(char* s, char* suffix) {";
  emitln "    size_t slen = strlen(s);";
  emitln "    size_t suffixlen = strlen(suffix);";
  emitln "    if (suffixlen > slen) return false;";
  emitln "    return strcmp(s + slen - suffixlen, suffix) == 0;";
  emitln "}";
  emitln "";
  emitln "static char* shotgun_str_trim(char* s) {";
  emitln "    while (*s && (*s == ' ' || *s == '\\t' || *s == '\\n' || *s == '\\r')) s++;";
  emitln "    if (*s == 0) return strdup(\"\");";
  emitln "    char* end = s + strlen(s) - 1;";
  emitln "    while (end > s && (*end == ' ' || *end == '\\t' || *end == '\\n' || *end == '\\r')) end--;";
  emitln "    size_t len = end - s + 1;";
  emitln "    char* result = malloc(len + 1);";
  emitln "    memcpy(result, s, len);";
  emitln "    result[len] = '\\0';";
  emitln "    return result;";
  emitln "}";
  emitln "";
  emitln "static char* shotgun_str_replace(char* s, char* old, char* new_s) {";
  emitln "    size_t old_len = strlen(old);";
  emitln "    size_t new_len = strlen(new_s);";
  emitln "    size_t count = 0;";
  emitln "    char* tmp = s;";
  emitln "    while ((tmp = strstr(tmp, old))) { count++; tmp += old_len; }";
  emitln "    size_t result_len = strlen(s) + count * (new_len - old_len);";
  emitln "    char* result = malloc(result_len + 1);";
  emitln "    char* dst = result;";
  emitln "    while (*s) {";
  emitln "        if (strncmp(s, old, old_len) == 0) {";
  emitln "            memcpy(dst, new_s, new_len);";
  emitln "            dst += new_len;";
  emitln "            s += old_len;";
  emitln "        } else {";
  emitln "            *dst++ = *s++;";
  emitln "        }";
  emitln "    }";
  emitln "    *dst = '\\0';";
  emitln "    return result;";
  emitln "}";
  emitln "";
  emitln "static char* shotgun_str_to_upper(char* s) {";
  emitln "    size_t len = strlen(s);";
  emitln "    char* result = malloc(len + 1);";
  emitln "    for (size_t i = 0; i <= len; i++) {";
  emitln "        result[i] = (s[i] >= 'a' && s[i] <= 'z') ? s[i] - 32 : s[i];";
  emitln "    }";
  emitln "    return result;";
  emitln "}";
  emitln "";
  emitln "static char* shotgun_str_to_lower(char* s) {";
  emitln "    size_t len = strlen(s);";
  emitln "    char* result = malloc(len + 1);";
  emitln "    for (size_t i = 0; i <= len; i++) {";
  emitln "        result[i] = (s[i] >= 'A' && s[i] <= 'Z') ? s[i] + 32 : s[i];";
  emitln "    }";
  emitln "    return result;";
  emitln "}";
  emitln "";
  emitln "/* Array runtime */";
  emitln "typedef struct { char** data; size_t len; size_t cap; } Array_str;";
  emitln "";
  emitln "static Array_str* shotgun_str_split(char* s, char* delim) {";
  emitln "    size_t delim_len = strlen(delim);";
  emitln "    size_t count = 1;";
  emitln "    char* tmp = s;";
  emitln "    while ((tmp = strstr(tmp, delim))) { count++; tmp += delim_len; }";
  emitln "    Array_str* arr = malloc(sizeof(Array_str));";
  emitln "    arr->data = malloc(sizeof(char*) * count);";
  emitln "    arr->len = count;";
  emitln "    arr->cap = count;";
  emitln "    size_t i = 0;";
  emitln "    char* start = s;";
  emitln "    while ((tmp = strstr(start, delim))) {";
  emitln "        size_t part_len = tmp - start;";
  emitln "        arr->data[i] = malloc(part_len + 1);";
  emitln "        memcpy(arr->data[i], start, part_len);";
  emitln "        arr->data[i][part_len] = '\\0';";
  emitln "        i++;";
  emitln "        start = tmp + delim_len;";
  emitln "    }";
  emitln "    arr->data[i] = strdup(start);";
  emitln "    return arr;";
  emitln "}";
  emitln "";
  emitln "typedef struct { int64_t* data; size_t len; size_t cap; } Array_int64;";
  emitln "typedef struct { bool* data; size_t len; size_t cap; } Array_bool;";
  emitln "typedef struct { float* data; size_t len; size_t cap; } Array_f32;";
  emitln "typedef struct { double* data; size_t len; size_t cap; } Array_f64;";
  emitln "";
  emitln "/* Channel runtime */";
  emitln "typedef struct {";
  emitln "    pthread_mutex_t mutex;";
  emitln "    pthread_cond_t not_empty;";
  emitln "    pthread_cond_t not_full;";
  emitln "    void* buffer;";
  emitln "    size_t elem_size;";
  emitln "    size_t capacity;";
  emitln "    size_t size;";
  emitln "    size_t head;";
  emitln "    size_t tail;";
  emitln "} ShotgunChan;";
  emitln "";
  emitln "static ShotgunChan* shotgun_chan_create(size_t elem_size) {";
  emitln "    ShotgunChan* c = malloc(sizeof(ShotgunChan));";
  emitln "    pthread_mutex_init(&c->mutex, NULL);";
  emitln "    pthread_cond_init(&c->not_empty, NULL);";
  emitln "    pthread_cond_init(&c->not_full, NULL);";
  emitln "    c->elem_size = elem_size;";
  emitln "    c->capacity = 1;";
  emitln "    c->buffer = malloc(elem_size * c->capacity);";
  emitln "    c->size = c->head = c->tail = 0;";
  emitln "    return c;";
  emitln "}";
  emitln "";
  emitln "static void shotgun_chan_send(ShotgunChan* c, void* val) {";
  emitln "    pthread_mutex_lock(&c->mutex);";
  emitln "    while (c->size == c->capacity) {";
  emitln "        pthread_cond_wait(&c->not_full, &c->mutex);";
  emitln "    }";
  emitln "    memcpy((char*)c->buffer + c->tail * c->elem_size, val, c->elem_size);";
  emitln "    c->tail = (c->tail + 1) % c->capacity;";
  emitln "    c->size++;";
  emitln "    pthread_cond_signal(&c->not_empty);";
  emitln "    pthread_mutex_unlock(&c->mutex);";
  emitln "}";
  emitln "";
  emitln "static void shotgun_chan_recv(ShotgunChan* c, void* out) {";
  emitln "    pthread_mutex_lock(&c->mutex);";
  emitln "    while (c->size == 0) {";
  emitln "        pthread_cond_wait(&c->not_empty, &c->mutex);";
  emitln "    }";
  emitln "    memcpy(out, (char*)c->buffer + c->head * c->elem_size, c->elem_size);";
  emitln "    c->head = (c->head + 1) % c->capacity;";
  emitln "    c->size--;";
  emitln "    pthread_cond_signal(&c->not_full);";
  emitln "    pthread_mutex_unlock(&c->mutex);";
  emitln "}";
  emitln "";
  emitln "/* Type-safe channel wrappers */";
  emitln "typedef ShotgunChan Chan_int64;";
  emitln "static inline Chan_int64* chan_create_int64(void) { return shotgun_chan_create(sizeof(int64_t)); }";
  emitln "static inline void chan_send_int64(Chan_int64* c, int64_t v) { shotgun_chan_send(c, &v); }";
  emitln "static inline int64_t chan_recv_int64(Chan_int64* c) { int64_t v; shotgun_chan_recv(c, &v); return v; }";
  emitln "";
  emitln "typedef ShotgunChan Chan_str;";
  emitln "static inline Chan_str* chan_create_str(void) { return shotgun_chan_create(sizeof(char*)); }";
  emitln "static inline void chan_send_str(Chan_str* c, char* v) { shotgun_chan_send(c, &v); }";
  emitln "static inline char* chan_recv_str(Chan_str* c) { char* v; shotgun_chan_recv(c, &v); return v; }";
  emitln "";
  emitln "typedef ShotgunChan Chan_bool;";
  emitln "static inline Chan_bool* chan_create_bool(void) { return shotgun_chan_create(sizeof(bool)); }";
  emitln "static inline void chan_send_bool(Chan_bool* c, bool v) { shotgun_chan_send(c, &v); }";
  emitln "static inline bool chan_recv_bool(Chan_bool* c) { bool v; shotgun_chan_recv(c, &v); return v; }";
  emitln "";
  emitln "typedef ShotgunChan Chan_f64;";
  emitln "static inline Chan_f64* chan_create_f64(void) { return shotgun_chan_create(sizeof(double)); }";
  emitln "static inline void chan_send_f64(Chan_f64* c, double v) { shotgun_chan_send(c, &v); }";
  emitln "static inline double chan_recv_f64(Chan_f64* c) { double v; shotgun_chan_recv(c, &v); return v; }";
  emitln "";
  (* Hash map runtime *)
  emitln "/* Hash map runtime */";
  emitln "typedef struct {";
  emitln "    char* key;";
  emitln "    void* value;";
  emitln "    bool occupied;";
  emitln "    bool deleted;";
  emitln "} ShotgunMapEntry;";
  emitln "";
  emitln "typedef struct {";
  emitln "    ShotgunMapEntry* buckets;";
  emitln "    size_t capacity;";
  emitln "    size_t size;";
  emitln "    size_t value_size;";
  emitln "} ShotgunMap;";
  emitln "";
  emitln "static uint64_t shotgun_hash_str(const char* s) {";
  emitln "    uint64_t h = 14695981039346656037ULL;";
  emitln "    while (*s) { h ^= (uint64_t)*s++; h *= 1099511628211ULL; }";
  emitln "    return h;";
  emitln "}";
  emitln "";
  emitln "static ShotgunMap* shotgun_map_create(size_t value_size) {";
  emitln "    ShotgunMap* m = malloc(sizeof(ShotgunMap));";
  emitln "    m->capacity = 16;";
  emitln "    m->size = 0;";
  emitln "    m->value_size = value_size;";
  emitln "    m->buckets = calloc(m->capacity, sizeof(ShotgunMapEntry));";
  emitln "    return m;";
  emitln "}";
  emitln "";
  emitln "static void shotgun_map_resize(ShotgunMap* m) {";
  emitln "    size_t old_cap = m->capacity;";
  emitln "    ShotgunMapEntry* old_buckets = m->buckets;";
  emitln "    m->capacity *= 2;";
  emitln "    m->buckets = calloc(m->capacity, sizeof(ShotgunMapEntry));";
  emitln "    m->size = 0;";
  emitln "    for (size_t i = 0; i < old_cap; i++) {";
  emitln "        if (old_buckets[i].occupied && !old_buckets[i].deleted) {";
  emitln "            uint64_t h = shotgun_hash_str(old_buckets[i].key) % m->capacity;";
  emitln "            while (m->buckets[h].occupied) h = (h + 1) % m->capacity;";
  emitln "            m->buckets[h] = old_buckets[i];";
  emitln "            m->size++;";
  emitln "        }";
  emitln "    }";
  emitln "    free(old_buckets);";
  emitln "}";
  emitln "";
  emitln "static void shotgun_map_set(ShotgunMap* m, char* key, void* value) {";
  emitln "    if (m->size * 2 >= m->capacity) shotgun_map_resize(m);";
  emitln "    uint64_t h = shotgun_hash_str(key) % m->capacity;";
  emitln "    while (m->buckets[h].occupied && !m->buckets[h].deleted && strcmp(m->buckets[h].key, key) != 0) {";
  emitln "        h = (h + 1) % m->capacity;";
  emitln "    }";
  emitln "    if (!m->buckets[h].occupied || m->buckets[h].deleted) {";
  emitln "        m->buckets[h].key = strdup(key);";
  emitln "        m->buckets[h].value = malloc(m->value_size);";
  emitln "        m->buckets[h].occupied = true;";
  emitln "        m->buckets[h].deleted = false;";
  emitln "        m->size++;";
  emitln "    }";
  emitln "    memcpy(m->buckets[h].value, value, m->value_size);";
  emitln "}";
  emitln "";
  emitln "static void* shotgun_map_get(ShotgunMap* m, char* key) {";
  emitln "    uint64_t h = shotgun_hash_str(key) % m->capacity;";
  emitln "    while (m->buckets[h].occupied) {";
  emitln "        if (!m->buckets[h].deleted && strcmp(m->buckets[h].key, key) == 0) {";
  emitln "            return m->buckets[h].value;";
  emitln "        }";
  emitln "        h = (h + 1) % m->capacity;";
  emitln "    }";
  emitln "    return NULL;";
  emitln "}";
  emitln "";
  emitln "static bool shotgun_map_has(ShotgunMap* m, char* key) {";
  emitln "    return shotgun_map_get(m, key) != NULL;";
  emitln "}";
  emitln "";
  emitln "static void shotgun_map_delete(ShotgunMap* m, char* key) {";
  emitln "    uint64_t h = shotgun_hash_str(key) % m->capacity;";
  emitln "    while (m->buckets[h].occupied) {";
  emitln "        if (!m->buckets[h].deleted && strcmp(m->buckets[h].key, key) == 0) {";
  emitln "            m->buckets[h].deleted = true;";
  emitln "            m->size--;";
  emitln "            return;";
  emitln "        }";
  emitln "        h = (h + 1) % m->capacity;";
  emitln "    }";
  emitln "}";
  emitln "";
  emitln "static size_t shotgun_map_len(ShotgunMap* m) { return m->size; }";
  emitln ""

(* Generate goroutine wrapper struct and function *)
let gen_go_wrapper info =
  let id = info.go_id in
  let func_name = info.go_func in
  let arg_types = info.go_arg_types in

  (* Generate wrapper struct *)
  emitln "";
  emit "typedef struct { ";
  List.iteri (fun i t ->
    emit (c_type t);
    emit " arg";
    emit (string_of_int i);
    emit "; "
  ) arg_types;
  emit "} _go_";
  emit (string_of_int id);
  emitln "_args;";

  (* Generate wrapper function *)
  emit "static void* _go_";
  emit (string_of_int id);
  emitln "_wrapper(void* _raw) {";
  emit "    _go_";
  emit (string_of_int id);
  emit "_args* _args = (_go_";
  emit (string_of_int id);
  emitln "_args*)_raw;";
  emit "    ";
  emit func_name;
  emit "(";
  List.iteri (fun i _ ->
    if i > 0 then emit ", ";
    emit "_args->arg";
    emit (string_of_int i)
  ) arg_types;
  emitln ");";
  emitln "    free(_args);";
  emitln "    return NULL;";
  emitln "}"

(* Generate all collected goroutine wrappers *)
let gen_go_wrappers () =
  List.iter gen_go_wrapper (List.rev !go_wrappers)

(* Scan statements for go calls to collect wrapper info (first pass) *)
let rec scan_stmt_for_go ctx stmt =
  match stmt with
  | SVarDecl (typ, name, _) ->
    add_local ctx name typ
  | SConstDecl (name, expr) ->
    (match get_type ctx expr with
     | Some t -> add_local ctx name t
     | None -> ())
  | SGo e ->
    (match e with
     | ECall (EIdent func_name, _type_args, args) ->
       let go_id = next_go_id () in
       let arg_types = List.filter_map (fun arg -> get_type ctx arg) args in
       go_wrappers := { go_id; go_func = func_name; go_arg_types = arg_types } :: !go_wrappers
     | _ -> ())
  | SIf (_, then_stmts, else_stmts) ->
    List.iter (scan_stmt_for_go ctx) then_stmts;
    (match else_stmts with Some stmts -> List.iter (scan_stmt_for_go ctx) stmts | None -> ())
  | SFor (var, iter, body) ->
    (* Add loop variable to scope *)
    (match get_type ctx iter with
     | Some (TArray t) -> add_local ctx var t
     | _ -> ());
    List.iter (scan_stmt_for_go ctx) body
  | _ -> ()

let scan_function_for_go ctx params body =
  let func_ctx = { ctx with locals = Hashtbl.copy ctx.locals } in
  List.iter (function
    | PNamed (t, pname) -> Hashtbl.replace func_ctx.locals pname t
    | PSelf -> ()
  ) params;
  List.iter (scan_stmt_for_go func_ctx) body

let scan_program_for_go ctx program =
  List.iter (function
    | IFunction (_, _type_params, params, _, body) ->
      scan_function_for_go ctx params body
    | IMethod (_, _, _type_params, params, _, body) ->
      scan_function_for_go ctx params body
    | IImpl (_, _, methods) ->
      List.iter (fun m -> scan_function_for_go ctx m.im_params m.im_body) methods
    | _ -> ()
  ) program

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
  reset_go_state ();
  let ctx = create_ctx env in
  gen_prelude ();

  (* Forward declarations for structs and enums - only non-generic ones *)
  List.iter (function
    | IStruct (name, type_params, _) when type_params = [] ->
      emit "typedef struct ";
      emit name;
      emit " ";
      emit name;
      emitln ";"
    | IEnum (name, type_params, _) when type_params = [] ->
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
    | IStruct (name, type_params, _) when type_params = [] -> user_array_types := name :: !user_array_types
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

  (* Generate monomorphized types FIRST - non-generic structs may reference them *)
  gen_monomorphized_types program;

  (* Generate struct and enum definitions - only non-generic ones *)
  List.iter (function
    | IStruct (name, type_params, fields) when type_params = [] -> gen_struct name fields
    | IEnum (name, type_params, variants) when type_params = [] -> gen_enum name variants
    | IError (name, fields) -> gen_error name fields
    | _ -> ()
  ) program;

  (* Forward declarations for all functions and methods - only non-generic ones *)
  emitln "";
  emitln "/* Forward declarations */";
  List.iter (function
    | IFunction (name, type_params, params, ret, _) when type_params = [] ->
      gen_function_decl name params ret
    | IMethod (type_name, method_name, type_params, params, ret, _) when type_params = [] ->
      gen_method_decl type_name method_name params ret
    | IImpl (type_name, _, methods) ->
      List.iter (fun m ->
        gen_method_decl type_name m.im_name m.im_params m.im_return
      ) methods
    | IUses _ -> ()  (* Skip imports *)
    | _ -> ()
  ) program;

  (* Forward declarations for monomorphized methods *)
  gen_monomorphized_methods ~decl_only:true ctx program;

  (* Scan for go statements and generate wrappers *)
  scan_program_for_go ctx program;
  gen_go_wrappers ();

  (* Reset go counter for actual codegen (must match scan order) *)
  go_counter := 0;

  (* Generate remaining items - only non-generic ones *)
  List.iter (function
    | IStruct _ | IEnum _ | IError _ -> ()  (* Already generated *)
    | ITrait (name, methods) -> gen_trait name methods
    | IImpl (type_name, trait_name, methods) -> gen_impl ctx type_name trait_name methods
    | IMethod (type_name, method_name, type_params, params, ret, body) when type_params = [] ->
      gen_method ctx type_name method_name params ret body
    | IFunction (name, type_params, params, ret, body) when type_params = [] ->
      gen_function ctx name params ret body
    | IUses _ -> ()  (* Imports handled in multi-file compilation *)
    | _ -> ()  (* Skip generic definitions - they get monomorphized *)
  ) program;

  (* Generate monomorphized methods for generic types *)
  gen_monomorphized_methods ctx program;

  Buffer.contents buf
