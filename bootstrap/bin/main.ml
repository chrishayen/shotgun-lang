(* Shotgun Compiler CLI *)

open Shotgun

let usage = {|
Shotgun Compiler

Usage:
  shotgun <file.bs>                 Compile to binary
  shotgun <file.bs> -o <out>        Compile with custom output name
  shotgun emit-c <file.bs>          Output C code only
  shotgun check <file.bs>           Type check only
  shotgun parse <file.bs>           Parse and print AST (debug)
  shotgun init <name>               Initialize a new project

Options:
  -o <name>      Output file name
  -h, --help     Show this help
  -v, --version  Show version
|}

(* Parse a file and return the AST *)
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

(* Try to load project config, return None if not found *)
let try_load_config filename =
  Config.load_config filename

(* Generate C code *)
let emit_c env ast =
  Codegen.generate env ast

(* Compile C code to binary *)
let compile_c c_file output_name =
  let cmd = Printf.sprintf "gcc -Wall -Wextra -Wno-unused-function -Wno-unused-variable -std=gnu99 -pthread -o %s %s"
              output_name c_file in
  let result = Sys.command cmd in
  if result = 0 then Ok ()
  else Error (Printf.sprintf "gcc failed with exit code %d" result)

(* Get base name without extension *)
let basename_no_ext filename =
  let base = Filename.basename filename in
  try
    let dot = String.rindex base '.' in
    String.sub base 0 dot
  with Not_found -> base

(* Commands *)
let cmd_parse filename =
  match parse_file filename with
  | Error e ->
    prerr_endline e;
    exit 1
  | Ok ast ->
    print_endline (Ast.show_program ast);
    exit 0

let cmd_check filename =
  let abs_filename = if Filename.is_relative filename then
    Filename.concat (Sys.getcwd ()) filename
  else filename in
  let pkg_dir = Filename.dirname abs_filename in
  let cache = Resolver.create_cache () in
  (* Check for project config for multi-file support *)
  let config = try_load_config filename in
  let warnings = match config with
    | None ->
      (* No config - use relative import resolution with single file *)
      (match parse_file filename with
      | Error e ->
        prerr_endline e;
        exit 1
      | Ok ast ->
        Hashtbl.replace cache.parsed abs_filename ast;
        let resolve_errors = Resolver.resolve_imports_relative cache abs_filename ast in
        List.iter (fun e -> prerr_endline e) resolve_errors;
        if resolve_errors <> [] then exit 1;
        let (_env, warnings) = Resolver.analyze_with_imports_relative cache abs_filename ast in
        if List.exists (fun w -> not (String.starts_with ~prefix:"Warning" w)) warnings then begin
          List.iter prerr_endline warnings;
          exit 1
        end;
        warnings)
    | Some cfg ->
      (* Package mode - load all .bs files in TARGET FILE's directory (not project root) *)
      (match Resolver.load_package_from_dir cache pkg_dir with
      | Error e ->
        prerr_endline e;
        exit 1
      | Ok ast ->
        let resolve_errors = Resolver.resolve_package_imports cache cfg pkg_dir ast in
        List.iter (fun e -> prerr_endline e) resolve_errors;
        if resolve_errors <> [] then exit 1;
        let (_env, warnings) = Resolver.analyze_package_with_imports cfg cache pkg_dir ast in
        if List.exists (fun w -> not (String.starts_with ~prefix:"Warning" w)) warnings then begin
          List.iter prerr_endline warnings;
          exit 1
        end;
        warnings)
  in
  List.iter (fun w -> prerr_endline ("Warning: " ^ w)) warnings;
  print_endline "OK";
  exit 0

let cmd_emit_c filename =
  let abs_filename = if Filename.is_relative filename then
    Filename.concat (Sys.getcwd ()) filename
  else filename in
  let pkg_dir = Filename.dirname abs_filename in
  let cache = Resolver.create_cache () in
  (* Check for project config for multi-file support *)
  let config = try_load_config filename in
  let (env, warnings, all_items) = match config with
    | None ->
      (* No config - use relative import resolution with single file *)
      (match parse_file filename with
      | Error e ->
        prerr_endline e;
        exit 1
      | Ok ast ->
        Hashtbl.replace cache.parsed abs_filename ast;
        let resolve_errors = Resolver.resolve_imports_relative cache abs_filename ast in
        List.iter (fun e -> prerr_endline ("Error: " ^ e)) resolve_errors;
        if resolve_errors <> [] then exit 1;
        let (env, warnings) = Resolver.analyze_with_imports_relative cache abs_filename ast in
        let all_items = Resolver.collect_all_items_relative cache abs_filename ast in
        (env, warnings, all_items))
    | Some cfg ->
      (* Package mode - load all .bs files in TARGET FILE's directory (not project root) *)
      (match Resolver.load_package_from_dir cache pkg_dir with
      | Error e ->
        prerr_endline e;
        exit 1
      | Ok ast ->
        let resolve_errors = Resolver.resolve_package_imports cache cfg pkg_dir ast in
        List.iter (fun e -> prerr_endline ("Error: " ^ e)) resolve_errors;
        if resolve_errors <> [] then exit 1;
        let (env, warnings) = Resolver.analyze_package_with_imports cfg cache pkg_dir ast in
        let all_items = Resolver.collect_package_items cache cfg ast in
        (env, warnings, all_items))
  in
  List.iter (fun w -> prerr_endline ("Warning: " ^ w)) warnings;
  let c_code = emit_c env all_items in
  print_string c_code;
  exit 0

let cmd_build filename output_opt =
  let abs_filename = if Filename.is_relative filename then
    Filename.concat (Sys.getcwd ()) filename
  else filename in
  let pkg_dir = Filename.dirname abs_filename in
  let cache = Resolver.create_cache () in
  (* Check for project config for multi-file support *)
  let config = try_load_config filename in
  let (env, warnings, all_items, base) = match config with
    | None ->
      (* No config - use relative import resolution with single file *)
      (match parse_file filename with
      | Error e ->
        prerr_endline e;
        exit 1
      | Ok ast ->
        Hashtbl.replace cache.parsed abs_filename ast;
        let resolve_errors = Resolver.resolve_imports_relative cache abs_filename ast in
        List.iter (fun e -> prerr_endline ("Error: " ^ e)) resolve_errors;
        if resolve_errors <> [] then exit 1;
        let (env, warnings) = Resolver.analyze_with_imports_relative cache abs_filename ast in
        let all_items = Resolver.collect_all_items_relative cache abs_filename ast in
        (env, warnings, all_items, basename_no_ext filename))
    | Some cfg ->
      (* Package mode - load all .bs files in TARGET FILE's directory (not project root) *)
      (match Resolver.load_package_from_dir cache pkg_dir with
      | Error e ->
        prerr_endline e;
        exit 1
      | Ok ast ->
        let resolve_errors = Resolver.resolve_package_imports cache cfg pkg_dir ast in
        List.iter (fun e -> prerr_endline ("Error: " ^ e)) resolve_errors;
        if resolve_errors <> [] then exit 1;
        let (env, warnings) = Resolver.analyze_package_with_imports cfg cache pkg_dir ast in
        let all_items = Resolver.collect_package_items cache cfg ast in
        (* Use project name as output name *)
        (env, warnings, all_items, cfg.name))
  in
  List.iter (fun w -> prerr_endline ("Warning: " ^ w)) warnings;
  let c_code = emit_c env all_items in
  let c_file = base ^ ".c" in
  let output = match output_opt with
    | Some o -> o
    | None -> base
  in
  (* Write C file *)
  let oc = open_out c_file in
  output_string oc c_code;
  close_out oc;
  (* Compile *)
  let result = compile_c c_file output in
  (* Clean up C file *)
  Sys.remove c_file;
  match result with
  | Error e ->
    prerr_endline e;
    exit 1
  | Ok () ->
    Printf.printf "Built %s\n" output;
    exit 0

let cmd_init name =
  (* Create shotgun.toml *)
  let oc = open_out "shotgun.toml" in
  Printf.fprintf oc "name = \"%s\"\n" name;
  close_out oc;
  Printf.printf "Created shotgun.toml for project '%s'\n" name;
  (* Create main.bs if it doesn't exist *)
  if not (Sys.file_exists "main.bs") then begin
    let oc = open_out "main.bs" in
    output_string oc "fn main {\n    print(\"Hello, world!\")\n}\n";
    close_out oc;
    print_endline "Created main.bs"
  end;
  exit 0

(* Parse command line *)
let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] | ["-h"] | ["--help"] ->
    print_string usage;
    exit 0
  | ["-v"] | ["--version"] | ["version"] ->
    print_endline ("shotgun " ^ Version.version);
    exit 0
  | ["parse"; filename] ->
    cmd_parse filename
  | ["check"; filename] ->
    cmd_check filename
  | ["emit-c"; filename] ->
    cmd_emit_c filename
  | [filename] when String.ends_with ~suffix:".bs" filename ->
    cmd_build filename None
  | [filename; "-o"; output] when String.ends_with ~suffix:".bs" filename ->
    cmd_build filename (Some output)
  | ["init"; name] ->
    cmd_init name
  | _ ->
    prerr_string usage;
    exit 1
