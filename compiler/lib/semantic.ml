(* Semantic analysis for Shotgun compiler *)

open Ast

(* Symbol table entry *)
type symbol =
  | SVar of typ * bool  (* type, is_const *)
  | SFunc of param list * typ option
  | SMethod of string * param list * typ option  (* type name, params, return *)
  | SStruct of field list
  | STrait of trait_method list
  | SError of field list

(* Environment for semantic analysis *)
type env = {
  symbols: (string, symbol) Hashtbl.t;
  structs: (string, field list) Hashtbl.t;
  enums: (string, enum_variant list) Hashtbl.t;
  traits: (string, trait_method list) Hashtbl.t;
  errors: (string, field list) Hashtbl.t;
  methods: (string * string, param list * typ option) Hashtbl.t;  (* (type, method) -> params, return *)
  impls: (string * string, impl_method list) Hashtbl.t;  (* (type, trait) -> methods *)
  generic_enum_params: (string, string list) Hashtbl.t;  (* enum name -> type params *)
  generic_struct_params: (string, string list) Hashtbl.t;  (* struct name -> type params *)
  in_method: bool;
  current_type: string option;
  errors_list: string list ref;  (* Use ref so it's shared across scopes *)
}

let create_env () = {
  symbols = Hashtbl.create 64;
  structs = Hashtbl.create 32;
  enums = Hashtbl.create 32;
  traits = Hashtbl.create 16;
  errors = Hashtbl.create 16;
  methods = Hashtbl.create 64;
  impls = Hashtbl.create 32;
  generic_enum_params = Hashtbl.create 16;
  generic_struct_params = Hashtbl.create 16;
  in_method = false;
  current_type = None;
  errors_list = ref [];
}

let add_error env msg =
  env.errors_list := msg :: !(env.errors_list)

let push_scope env =
  (* Create a new env with a copy of symbols for scoping *)
  { env with symbols = Hashtbl.copy env.symbols }

(* Check that optional params come after required params *)
let check_param_order env params =
  let rec check seen_optional = function
    | [] -> ()
    | PSelf :: rest -> check seen_optional rest
    | PNamed (typ, name) :: rest ->
      let is_optional = match typ with TOptional _ -> true | _ -> false in
      if seen_optional && not is_optional then
        add_error env (Printf.sprintf "Required parameter '%s' cannot follow optional parameter" name)
      else
        check (seen_optional || is_optional) rest
  in
  check false params

(* Register all top-level declarations first *)
let register_item env = function
  | IStruct (name, type_params, fields) ->
    Hashtbl.replace env.structs name fields;
    if type_params <> [] then Hashtbl.replace env.generic_struct_params name type_params
  | IEnum (name, type_params, variants) ->
    Hashtbl.replace env.enums name variants;
    if type_params <> [] then Hashtbl.replace env.generic_enum_params name type_params
  | ITrait (name, methods) ->
    Hashtbl.replace env.traits name methods
  | IError (name, fields) ->
    Hashtbl.replace env.errors name fields
  | IMethod (type_name, method_name, _type_params, params, ret, _body) ->
    Hashtbl.replace env.methods (type_name, method_name) (params, ret)
  | IImpl (type_name, trait_name, methods) ->
    Hashtbl.replace env.impls (type_name, trait_name) methods
  | IFunction (name, _type_params, params, ret, _body) ->
    Hashtbl.replace env.symbols name (SFunc (params, ret))
  | IUses _paths ->
    (* TODO: resolve imports and merge symbols *)
    ()

(* Type equality check *)
let rec types_equal t1 t2 =
  match t1, t2 with
  | TInt, TInt -> true
  | TStr, TStr -> true
  | TBool, TBool -> true
  | TF32, TF32 -> true
  | TF64, TF64 -> true
  | TU32, TU32 -> true
  | TU64, TU64 -> true
  | TVoid, TVoid -> true
  | TOptional a, TOptional b -> types_equal a b
  | TArray a, TArray b -> types_equal a b
  | TChan a, TChan b -> types_equal a b
  | TUser a, TUser b -> a = b
  | TResult (a1, e1), TResult (a2, e2) -> types_equal a1 a2 && e1 = e2
  | _ -> false

(* Check if a type is defined *)
let rec type_exists env typ =
  match typ with
  | TInt | TStr | TBool | TF32 | TF64 | TU32 | TU64 | TVoid -> true
  | TOptional t -> type_exists env t
  | TArray t -> type_exists env t
  | TChan t -> type_exists env t
  | TUser name -> Hashtbl.mem env.structs name || Hashtbl.mem env.enums name || Hashtbl.mem env.errors name
  | TResult (t, e) -> type_exists env t && (e = "Error" || Hashtbl.mem env.errors e)
  | TParam _ -> true  (* Type parameters are always valid within their scope *)
  | TApply (name, args) ->
    (* Map<K, V> is a built-in generic type *)
    (name = "Map" || Hashtbl.mem env.structs name || Hashtbl.mem env.enums name) &&
    List.for_all (type_exists env) args

(* Get type of expression - simplified version *)
let rec infer_expr_type env expr =
  match expr with
  | EInt _ -> Some TInt
  | EFloat _ -> Some TF64
  | EString _ -> Some TStr
  | EBool _ -> Some TBool
  | ENone -> None  (* Can be any optional type *)
  | EIdent name ->
    (match Hashtbl.find_opt env.symbols name with
     | Some (SVar (t, _)) -> Some t
     | _ -> None)
  | EBinary (op, _l, _r) ->
    (match op with
     | Add | Sub | Mul | Div | Mod -> Some TInt
     | Eq | Neq | Lt | Gt | Lte | Gte | And | Or -> Some TBool)
  | EUnary (Not, _) -> Some TBool
  | ECall (callee, _type_args, _args) ->
    (match callee with
     | EIdent name ->
       (match Hashtbl.find_opt env.symbols name with
        | Some (SFunc (_, ret)) -> ret
        | _ -> None)
     | EMember (obj, method_name) ->
       (match infer_expr_type env obj with
        | Some (TUser type_name) ->
          (match Hashtbl.find_opt env.methods (type_name, method_name) with
           | Some (_, ret) -> ret
           | None -> None)
        | Some (TChan t) when method_name = "recv" ->
          Some t
        (* String built-in methods *)
        | Some TStr ->
          (match method_name with
           | "len" | "at" | "find" -> Some TInt
           | "contains" | "starts_with" | "ends_with" -> Some TBool
           | "slice" | "trim" | "replace" | "to_upper" | "to_lower" -> Some TStr
           | "split" -> Some (TArray TStr)
           | _ -> None)
        (* Array built-in methods *)
        | Some (TArray _) when method_name = "len" -> Some TInt
        (* Map built-in methods *)
        | Some (TApply ("Map", _)) when method_name = "len" -> Some TInt
        | Some (TApply ("Map", _)) when method_name = "has" -> Some TBool
        | Some (TApply ("Map", [_k; v])) when method_name = "get" -> Some v
        | _ -> None)
     | _ -> None)
  | EMember (obj, field_name) ->
    (match infer_expr_type env obj with
     | Some (TUser type_name) ->
       (match Hashtbl.find_opt env.structs type_name with
        | Some fields ->
          List.find_opt (fun f -> f.field_name = field_name) fields
          |> Option.map (fun f -> f.field_type)
        | None -> None)
     | _ -> None)
  | EIndex (arr, _idx) ->
    (match infer_expr_type env arr with
     | Some (TArray t) -> Some t
     | Some (TApply ("Map", [_k; v])) -> Some v  (* Map indexing returns value type *)
     | _ -> None)
  | EOr (e, _clause) -> infer_expr_type env e
  | EStructLit (name, type_args, _fields) ->
    if type_args = [] then Some (TUser name)
    else Some (TApply (name, type_args))
  | EEnumVariant (enum_name, type_args, _variant_name, _fields) ->
    if type_args = [] then Some (TUser enum_name)
    else Some (TApply (enum_name, type_args))
  | EArrayLit [] -> None
  | EArrayLit (e :: _) ->
    infer_expr_type env e |> Option.map (fun t -> TArray t)
  | EChan -> None  (* Type inferred from context *)
  | EParen e -> infer_expr_type env e
  | EAssign (_, e, _) -> infer_expr_type env e
  | EMatch (_, _, arms) ->
    (* Type of match is the type of the first arm's result - assume all arms have same type *)
    (match arms with
     | [] -> None
     | (_, first_result) :: _ -> infer_expr_type env first_result)

(* Extract the base enum name from a type (handles TUser and TApply) *)
and get_enum_name_from_type = function
  | TUser name -> Some name
  | TApply (name, _) -> Some name
  | _ -> None

(* Bind pattern variables to the environment *)
and bind_pattern_vars env pat expr_types using_type =
  let get_matched_type idx =
    if idx < List.length expr_types then List.nth expr_types idx else None
  in
  match pat with
  | PWildcard | PLiteral _ -> ()
  | PIdent name ->
    (* Bind identifier to type of matched expression *)
    let typ = match get_matched_type 0 with Some t -> t | None -> TInt in
    Hashtbl.replace env.symbols name (SVar (typ, false))
  | PVariant (enum_type_opt, variant_name, bindings) ->
    let enum_name = match enum_type_opt with
      | Some t -> get_enum_name_from_type t
      | None -> Option.bind using_type get_enum_name_from_type
    in
    let enum_name = match enum_name with Some n -> n | None -> "" in
    (* Validate enum exists *)
    (match Hashtbl.find_opt env.enums enum_name with
     | None ->
       if enum_name <> "" then
         add_error env (Printf.sprintf "Unknown enum type: %s" enum_name)
     | Some variants ->
       (* Validate variant exists *)
       (match List.find_opt (fun v -> v.variant_name = variant_name) variants with
        | None ->
          add_error env (Printf.sprintf "Unknown variant %s in enum %s" variant_name enum_name)
        | Some variant ->
          (* Validate field names and bind variables *)
          List.iter (fun (field_name, binding_name) ->
            match List.find_opt (fun f -> f.field_name = field_name) variant.variant_fields with
            | None ->
              add_error env (Printf.sprintf "Unknown field %s in variant %s.%s" field_name enum_name variant_name)
            | Some field ->
              Hashtbl.replace env.symbols binding_name (SVar (field.field_type, false))
          ) bindings))
  | PTuple patterns ->
    List.iteri (fun i p ->
      let sub_expr_types = [get_matched_type i] in
      bind_pattern_vars env p sub_expr_types using_type
    ) patterns

(* Check expression - add variable to scope, check types *)
let rec check_expr env expr =
  match expr with
  | EIdent "self" when not env.in_method ->
    add_error env "'self' used outside of method"
  | EIdent name when not (Hashtbl.mem env.symbols name) && name <> "self" && name <> "print" ->
    add_error env (Printf.sprintf "Undefined variable: %s" name)
  | EBinary (_, l, r) ->
    check_expr env l;
    check_expr env r
  | EUnary (_, e) ->
    check_expr env e
  | ECall (callee, _type_args, args) ->
    check_expr env callee;
    List.iter (check_expr env) args
  | EMember (obj, _) ->
    check_expr env obj
  | EIndex (arr, idx) ->
    check_expr env arr;
    check_expr env idx
  | EOr (e, clause) ->
    check_expr env e;
    (match clause with
     | OrExpr e2 -> check_expr env e2
     | OrReturn (Some e2) -> check_expr env e2
     | OrReturn None -> ()
     | OrError (_, fields) -> List.iter (fun (_, e2) -> check_expr env e2) fields
     | OrWait e2 -> check_expr env e2)
  | EStructLit (name, _type_args, fields) ->
    (* Map is a built-in type, not a user struct *)
    if name <> "Map" && not (Hashtbl.mem env.structs name) then
      add_error env (Printf.sprintf "Unknown struct type: %s" name);
    List.iter (fun (_, e) -> check_expr env e) fields
  | EEnumVariant (enum_name, _type_args, variant_name, fields) ->
    (match Hashtbl.find_opt env.enums enum_name with
     | None ->
       add_error env (Printf.sprintf "Unknown enum type: %s" enum_name)
     | Some variants ->
       if not (List.exists (fun v -> v.variant_name = variant_name) variants) then
         add_error env (Printf.sprintf "Unknown variant %s in enum %s" variant_name enum_name));
    List.iter (fun (_, e) -> check_expr env e) fields
  | EArrayLit elems ->
    List.iter (check_expr env) elems
  | EParen e -> check_expr env e
  | EAssign (_, lhs, rhs) ->
    check_expr env lhs;
    check_expr env rhs;
    (* Check for const reassignment *)
    (match lhs with
     | EIdent name ->
       (match Hashtbl.find_opt env.symbols name with
        | Some (SVar (_, is_const)) ->
          if is_const then
            add_error env (Printf.sprintf "Cannot reassign const variable '%s'" name)
        | _ -> ())
     | _ -> ())
  | EString parts ->
    List.iter (function
      | SLiteral _ -> ()
      | SInterp e -> check_expr env e
    ) parts
  | EMatch (exprs, using_type, arms) ->
    (* Check matched expressions *)
    List.iter (check_expr env) exprs;
    (* Infer types of matched expressions for binding *)
    let expr_types = List.map (infer_expr_type env) exprs in
    (* Check each arm *)
    List.iter (fun (pat, result_expr) ->
      let arm_env = push_scope env in
      bind_pattern_vars arm_env pat expr_types using_type;
      check_expr arm_env result_expr
    ) arms
  | _ -> ()

(* Check statement *)
let rec check_stmt env stmt =
  match stmt with
  | SVarDecl (typ, name, expr) ->
    if not (type_exists env typ) then
      add_error env (Printf.sprintf "Unknown type in declaration of %s" name);
    check_expr env expr;
    Hashtbl.replace env.symbols name (SVar (typ, false))
  | SConstDecl (name, expr) ->
    check_expr env expr;
    (* Infer type from expression *)
    let typ = match infer_expr_type env expr with
      | Some t -> t
      | None -> TUser "unknown"  (* fallback *)
    in
    Hashtbl.replace env.symbols name (SVar (typ, true))
  | SReturn (Some e) ->
    check_expr env e
  | SReturn None -> ()
  | SIf (cond, then_stmts, else_stmts) ->
    check_expr env cond;
    let then_env = push_scope env in
    List.iter (check_stmt then_env) then_stmts;
    (match else_stmts with
     | Some stmts ->
       let else_env = push_scope env in
       List.iter (check_stmt else_env) stmts
     | None -> ())
  | SFor (var, iter, body) ->
    check_expr env iter;
    let body_env = push_scope env in
    (* Infer type of loop variable from iterator *)
    let var_type = match infer_expr_type env iter with
      | Some (TArray t) -> t
      | _ -> TUser "unknown"
    in
    Hashtbl.replace body_env.symbols var (SVar (var_type, false));
    List.iter (check_stmt body_env) body
  | SGo e ->
    check_expr env e
  | SExpr e ->
    check_expr env e

(* Check a function body *)
let check_function env params body =
  check_param_order env params;
  let func_env = push_scope env in
  List.iter (function
    | PSelf -> ()  (* self handled separately *)
    | PNamed (t, name) -> Hashtbl.replace func_env.symbols name (SVar (t, false))
  ) params;
  List.iter (check_stmt func_env) body

(* Check a method body *)
let check_method env type_name params body =
  check_param_order env params;
  let method_env = { (push_scope env) with
    in_method = true;
    current_type = Some type_name
  } in
  Hashtbl.replace method_env.symbols "self" (SVar (TUser type_name, false));
  List.iter (function
    | PSelf -> ()
    | PNamed (t, name) -> Hashtbl.replace method_env.symbols name (SVar (t, false))
  ) params;
  List.iter (check_stmt method_env) body

(* Check if a type is valid given a list of type parameters in scope *)
let rec type_exists_with_params env type_params typ =
  match typ with
  | TUser name ->
    List.mem name type_params || type_exists env typ
  | TArray t -> type_exists_with_params env type_params t
  | TOptional t -> type_exists_with_params env type_params t
  | TChan t -> type_exists_with_params env type_params t
  | TResult (t, e) -> type_exists_with_params env type_params t && (e = "Error" || Hashtbl.mem env.errors e)
  | TApply (name, args) ->
    (* Map<K, V> is a built-in generic type *)
    (name = "Map" || Hashtbl.mem env.structs name || Hashtbl.mem env.enums name) &&
    List.for_all (type_exists_with_params env type_params) args
  | _ -> type_exists env typ

(* Check an item *)
let check_item env = function
  | IStruct (name, type_params, fields) ->
    List.iter (fun f ->
      if not (type_exists_with_params env type_params f.field_type) then
        add_error env (Printf.sprintf "Unknown type for field %s in struct %s" f.field_name name)
    ) fields
  | IEnum (name, type_params, variants) ->
    List.iter (fun v ->
      List.iter (fun f ->
        if not (type_exists_with_params env type_params f.field_type) then
          add_error env (Printf.sprintf "Unknown type for field %s in variant %s of enum %s" f.field_name v.variant_name name)
      ) v.variant_fields
    ) variants
  | ITrait (_name, _methods) ->
    ()  (* Just declarations, no bodies to check *)
  | IImpl (_type_name, trait_name, methods) ->
    if not (Hashtbl.mem env.traits trait_name) then
      add_error env (Printf.sprintf "Unknown trait: %s" trait_name);
    List.iter (fun m ->
      check_method env _type_name m.im_params m.im_body
    ) methods
  | IMethod (type_name, _method_name, _type_params, params, _ret, body) ->
    if not (Hashtbl.mem env.structs type_name) then
      add_error env (Printf.sprintf "Method defined for unknown type: %s" type_name);
    check_method env type_name params body
  | IFunction (_name, _type_params, params, _ret, body) ->
    check_function env params body
  | IError (_name, _fields) ->
    ()  (* Just a type definition *)
  | IUses _paths ->
    ()  (* TODO: validate imports exist *)

(* Run semantic analysis on a program *)
let analyze program =
  let env = create_env () in
  (* First pass: register all declarations *)
  List.iter (register_item env) program;
  (* Second pass: check all items *)
  List.iter (check_item env) program;
  if !(env.errors_list) = [] then
    Ok env
  else
    Error (List.rev !(env.errors_list))

(* Run semantic analysis and return env with warnings *)
let analyze_with_warnings program =
  let env = create_env () in
  List.iter (register_item env) program;
  List.iter (check_item env) program;
  (env, List.rev !(env.errors_list))

(* Query type of an expression given an env and local variable types *)
let rec get_expr_type env locals expr =
  match expr with
  | EInt _ -> Some TInt
  | EFloat _ -> Some TF64
  | EString _ -> Some TStr
  | EBool _ -> Some TBool
  | ENone -> None
  | EIdent "self" ->
    (match Hashtbl.find_opt locals "self" with
     | Some t -> Some t
     | None -> None)
  | EIdent name ->
    (match Hashtbl.find_opt locals name with
     | Some t -> Some t
     | None ->
       match Hashtbl.find_opt env.symbols name with
       | Some (SVar (t, _)) -> Some t
       | _ -> None)
  | EBinary (op, _, _) ->
    (match op with
     | Add | Sub | Mul | Div | Mod -> Some TInt
     | Eq | Neq | Lt | Gt | Lte | Gte | And | Or -> Some TBool)
  | EUnary (Not, _) -> Some TBool
  | ECall (callee, _type_args, _) ->
    (match callee with
     | EIdent name ->
       (match Hashtbl.find_opt env.symbols name with
        | Some (SFunc (_, ret)) -> ret
        | _ -> None)
     | EMember (obj, method_name) ->
       (match get_expr_type env locals obj with
        | Some (TUser type_name) ->
          (match Hashtbl.find_opt env.methods (type_name, method_name) with
           | Some (_, ret) -> ret
           | None -> None)
        | Some (TChan t) when method_name = "recv" -> Some t
        (* String built-in methods *)
        | Some TStr ->
          (match method_name with
           | "len" | "at" | "find" -> Some TInt
           | "contains" | "starts_with" | "ends_with" -> Some TBool
           | "slice" | "trim" | "replace" | "to_upper" | "to_lower" -> Some TStr
           | "split" -> Some (TArray TStr)
           | _ -> None)
        (* Array built-in methods *)
        | Some (TArray _) when method_name = "len" -> Some TInt
        (* Map built-in methods *)
        | Some (TApply ("Map", _)) when method_name = "len" -> Some TInt
        | Some (TApply ("Map", _)) when method_name = "has" -> Some TBool
        | Some (TApply ("Map", [_k; v])) when method_name = "get" -> Some v
        | _ -> None)
     | _ -> None)
  | EMember (obj, field_name) ->
    (match get_expr_type env locals obj with
     | Some (TUser type_name) ->
       (match Hashtbl.find_opt env.structs type_name with
        | Some fields ->
          List.find_opt (fun f -> f.field_name = field_name) fields
          |> Option.map (fun f -> f.field_type)
        | None -> None)
     | _ -> None)
  | EIndex (arr, _) ->
    (match get_expr_type env locals arr with
     | Some (TArray t) -> Some t
     | Some (TApply ("Map", [_k; v])) -> Some v  (* Map indexing returns value type *)
     | _ -> None)
  | EOr (e, _) -> get_expr_type env locals e
  | EStructLit (name, type_args, _) ->
    if type_args = [] then Some (TUser name)
    else Some (TApply (name, type_args))
  | EEnumVariant (enum_name, type_args, _, _) ->
    if type_args = [] then Some (TUser enum_name)
    else Some (TApply (enum_name, type_args))
  | EArrayLit [] -> None
  | EArrayLit (e :: _) ->
    get_expr_type env locals e |> Option.map (fun t -> TArray t)
  | EChan -> None
  | EParen e -> get_expr_type env locals e
  | EAssign (_, lhs, _) -> get_expr_type env locals lhs
  | EMatch (_, _, arms) ->
    (match arms with
     | [] -> None
     | (_, first_result) :: _ -> get_expr_type env locals first_result)
