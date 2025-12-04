(* Shotgun Compiler CLI *)

open Shotgun

let usage = {|
Shotgun Compiler

Usage:
  shotgun build <file.bs>           Compile to C, then to binary
  shotgun build <file.bs> -o <out>  Compile with custom output name
  shotgun emit-c <file.bs>          Output C code only
  shotgun check <file.bs>           Type check only
  shotgun parse <file.bs>           Parse and print AST (debug)

Options:
  -o <name>   Output file name
  -h, --help  Show this help
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

(* Run semantic analysis *)
let check_program ast =
  match Semantic.analyze ast with
  | Ok env -> Ok env
  | Error errs ->
    Error (String.concat "\n" errs)

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
  match parse_file filename with
  | Error e ->
    prerr_endline e;
    exit 1
  | Ok ast ->
    match check_program ast with
    | Error e ->
      prerr_endline e;
      exit 1
    | Ok _ ->
      print_endline "OK";
      exit 0

let cmd_emit_c filename =
  match parse_file filename with
  | Error e ->
    prerr_endline e;
    exit 1
  | Ok ast ->
    let env = match check_program ast with
      | Error e ->
        prerr_endline ("Warning: " ^ e);
        Semantic.create_env ()
      | Ok env -> env
    in
    let c_code = emit_c env ast in
    print_string c_code;
    exit 0

let cmd_build filename output_opt =
  match parse_file filename with
  | Error e ->
    prerr_endline e;
    exit 1
  | Ok ast ->
    let env = match check_program ast with
      | Error e ->
        prerr_endline ("Warning: " ^ e);
        Semantic.create_env ()
      | Ok env -> env
    in
    let c_code = emit_c env ast in
    let base = basename_no_ext filename in
    let c_file = base ^ ".c" in
    let output = match output_opt with
      | Some o -> o
      | None -> base
    in
    (* Write C file *)
    let oc = open_out c_file in
    output_string oc c_code;
    close_out oc;
    Printf.printf "Generated %s\n" c_file;
    (* Compile *)
    match compile_c c_file output with
    | Error e ->
      prerr_endline e;
      exit 1
    | Ok () ->
      Printf.printf "Built %s\n" output;
      exit 0

(* Parse command line *)
let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] | ["-h"] | ["--help"] ->
    print_string usage;
    exit 0
  | ["parse"; filename] ->
    cmd_parse filename
  | ["check"; filename] ->
    cmd_check filename
  | ["emit-c"; filename] ->
    cmd_emit_c filename
  | ["build"; filename] ->
    cmd_build filename None
  | ["build"; filename; "-o"; output] ->
    cmd_build filename (Some output)
  | _ ->
    prerr_string usage;
    exit 1
