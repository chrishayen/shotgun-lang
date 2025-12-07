(* Module resolution for Shotgun compiler *)

open Ast

(* Cache for parsed and analyzed modules *)
type module_cache = {
  parsed: (string, program) Hashtbl.t;
  envs: (string, Semantic.env) Hashtbl.t;
}

let create_cache () = {
  parsed = Hashtbl.create 16;
  envs = Hashtbl.create 16;
}

(* Parse a file - caller must have checked file exists *)
let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  Lexer.reset_nesting ();
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    Ok ast
  with
  | Lexer.Lexer_error msg ->
    close_in ic;
    let pos = lexbuf.Lexing.lex_curr_p in
    Error (Printf.sprintf "Lexer error at %s:%d:%d: %s"
             pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg)
  | Parser.Error ->
    close_in ic;
    let pos = lexbuf.Lexing.lex_curr_p in
    Error (Printf.sprintf "Parse error at %s:%d:%d"
             pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol))

(* Extract uses from a program *)
let get_uses program =
  List.filter_map (function
    | IUses paths -> Some paths
    | _ -> None
  ) program
  |> List.flatten

(* Merge symbols from one env into another, recording collisions *)
let merge_env ~into ~from ~namespace ~errors =
  (* Copy structs with namespace prefix *)
  Hashtbl.iter (fun name fields ->
    let qualified_name = namespace ^ "_" ^ name in
    if Hashtbl.mem into.Semantic.structs qualified_name then
      errors := Printf.sprintf "Import collision: struct %s already defined" qualified_name :: !errors;
    Hashtbl.replace into.Semantic.structs qualified_name fields;
    if Hashtbl.mem into.Semantic.structs name then
      errors := Printf.sprintf "Import collision: struct %s already defined" name :: !errors;
    Hashtbl.replace into.Semantic.structs name fields
  ) from.Semantic.structs;

  (* Copy enums *)
  Hashtbl.iter (fun name variants ->
    let qualified_name = namespace ^ "_" ^ name in
    if Hashtbl.mem into.Semantic.enums qualified_name then
      errors := Printf.sprintf "Import collision: enum %s already defined" qualified_name :: !errors;
    Hashtbl.replace into.Semantic.enums qualified_name variants;
    if Hashtbl.mem into.Semantic.enums name then
      errors := Printf.sprintf "Import collision: enum %s already defined" name :: !errors;
    Hashtbl.replace into.Semantic.enums name variants
  ) from.Semantic.enums;

  (* Copy traits *)
  Hashtbl.iter (fun name methods ->
    if Hashtbl.mem into.Semantic.traits name then
      errors := Printf.sprintf "Import collision: trait %s already defined" name :: !errors;
    Hashtbl.replace into.Semantic.traits name methods
  ) from.Semantic.traits;

  (* Copy errors *)
  Hashtbl.iter (fun name fields ->
    if Hashtbl.mem into.Semantic.errors name then
      errors := Printf.sprintf "Import collision: error %s already defined" name :: !errors;
    Hashtbl.replace into.Semantic.errors name fields
  ) from.Semantic.errors;

  (* Copy functions *)
  Hashtbl.iter (fun name sym ->
    match sym with
    | Semantic.SFunc _ ->
      if Hashtbl.mem into.Semantic.symbols name then
        errors := Printf.sprintf "Import collision: function %s already defined" name :: !errors;
      Hashtbl.replace into.Semantic.symbols name sym
    | _ -> ()
  ) from.Semantic.symbols;

  (* Copy methods *)
  Hashtbl.iter (fun key value ->
    if Hashtbl.mem into.Semantic.methods key then
      let (tname, mname) = key in
      errors := Printf.sprintf "Import collision: method %s.%s already defined" tname mname :: !errors;
    Hashtbl.replace into.Semantic.methods key value
  ) from.Semantic.methods

(* Resolve and load all imports for a file *)
let rec resolve_imports cache config current_file program =
  let uses = get_uses program in
  let errors = ref [] in

  List.iter (fun import_path ->
    match Config.resolve_import config import_path with
    | None ->
      let path_str = String.concat "." import_path in
      errors := Printf.sprintf "Cannot resolve import: %s" path_str :: !errors
    | Some resolved_path ->
      if not (Hashtbl.mem cache.parsed resolved_path) then begin
        (* Avoid circular imports *)
        if resolved_path = current_file then
          errors := Printf.sprintf "Circular import: %s" resolved_path :: !errors
        else begin
          match parse_file resolved_path with
          | Error e -> errors := e :: !errors
          | Ok imported_program ->
            Hashtbl.replace cache.parsed resolved_path imported_program;
            (* Recursively resolve imports of the imported file *)
            let sub_errors = resolve_imports cache config resolved_path imported_program in
            errors := sub_errors @ !errors
        end
      end
  ) uses;

  List.rev !errors

(* Analyze a program with its imports *)
let analyze_with_imports config cache program =
  (* First analyze the imported modules *)
  let uses = get_uses program in
  let import_errors = ref [] in

  List.iter (fun import_path ->
    match Config.resolve_import config import_path with
    | None -> ()  (* Error already reported in resolve_imports *)
    | Some resolved_path ->
      if not (Hashtbl.mem cache.envs resolved_path) then begin
        match Hashtbl.find_opt cache.parsed resolved_path with
        | None -> ()  (* Not parsed, error reported elsewhere *)
        | Some imported_program ->
          let (env, warnings) = Semantic.analyze_with_warnings imported_program in
          List.iter (fun w ->
            import_errors := (resolved_path ^ ": " ^ w) :: !import_errors
          ) warnings;
          Hashtbl.replace cache.envs resolved_path env
      end
  ) uses;

  (* Create env and pre-populate with imported symbols BEFORE analysis *)
  let main_env = Semantic.create_env () in

  (* Merge imported symbols into main env first *)
  List.iter (fun import_path ->
    match Config.resolve_import config import_path with
    | None -> ()
    | Some resolved_path ->
      match Hashtbl.find_opt cache.envs resolved_path with
      | None -> ()
      | Some imported_env ->
        let namespace = match Config.namespace_of_import import_path with
          | Some ns -> ns
          | None -> "unknown"
        in
        merge_env ~into:main_env ~from:imported_env ~namespace ~errors:import_errors
  ) uses;

  (* Now register and check the main program items with merged env *)
  List.iter (Semantic.register_item main_env) program;
  List.iter (Semantic.check_item main_env) program;

  let main_warnings = List.rev !(main_env.errors_list) in

  (main_env, !import_errors @ main_warnings)

(* Collect all items from a program and its imports *)
let collect_all_items cache config program =
  let uses = get_uses program in
  let imported_items = ref [] in

  List.iter (fun import_path ->
    match Config.resolve_import config import_path with
    | None -> ()
    | Some resolved_path ->
      match Hashtbl.find_opt cache.parsed resolved_path with
      | None -> ()
      | Some imported_program ->
        (* Filter out IUses from imported programs to avoid duplication *)
        let items = List.filter (function IUses _ -> false | _ -> true) imported_program in
        imported_items := items @ !imported_items
  ) uses;

  (* Return imported items first, then main program items (excluding uses) *)
  let main_items = List.filter (function IUses _ -> false | _ -> true) program in
  !imported_items @ main_items
