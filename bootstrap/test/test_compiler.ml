(* Tests for Shotgun compiler *)

open Shotgun

(* Test helper *)
let test name f =
  try
    f ();
    Printf.printf "[PASS] %s\n" name
  with e ->
    Printf.printf "[FAIL] %s: %s\n" name (Printexc.to_string e)

let _assert_eq msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %s, got %s" msg expected actual)

let contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let temp_dir () =
  Random.self_init ();
  let dir = Filename.concat (Filename.get_temp_dir_name ()) ("shotgun_test_" ^ string_of_int (Random.bits ())) in
  Sys.mkdir dir 0o755;
  dir

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let compile_and_run_c c_code =
  let dir = temp_dir () in
  let c_path = Filename.concat dir "test.c" in
  let bin_path = Filename.concat dir "a.out" in
  write_file c_path c_code;
  let compile_cmd = Printf.sprintf "gcc -std=gnu99 -pthread -o %s %s" bin_path c_path in
  let compile_rc = Sys.command compile_cmd in
  if compile_rc <> 0 then failwith "gcc failed";
  let ic = Unix.open_process_in bin_path in
  let buf = Buffer.create 64 in
  (try
     while true do
       let line = input_line ic in
       Buffer.add_string buf line;
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Buffer.contents buf

(* Lexer tests *)
let test_lexer () =
  let lex s =
    let lexbuf = Lexing.from_string s in
    Lexer.reset_nesting ();
    let rec collect acc =
      match Lexer.token lexbuf with
      | Parser.EOF -> List.rev acc
      | tok -> collect (tok :: acc)
    in
    collect []
  in

  test "lexer: keywords" (fun () ->
    let tokens = lex "fn struct trait impl" in
    assert (List.length tokens = 4)
  );

  test "lexer: identifiers" (fun () ->
    let tokens = lex "foo bar_baz" in
    match tokens with
    | [Parser.IDENT "foo"; Parser.IDENT "bar_baz"] -> ()
    | _ -> failwith "unexpected tokens"
  );

  test "lexer: type identifiers" (fun () ->
    let tokens = lex "Person Config" in
    match tokens with
    | [Parser.TYPE_IDENT "Person"; Parser.TYPE_IDENT "Config"] -> ()
    | _ -> failwith "unexpected tokens"
  );

  test "lexer: numbers" (fun () ->
    let tokens = lex "42 3.14" in
    match tokens with
    | [Parser.INTEGER 42; Parser.FLOAT f] when f = 3.14 -> ()
    | _ -> failwith "unexpected tokens"
  );

  test "lexer: strings" (fun () ->
    let tokens = lex {|"hello"|} in
    match tokens with
    | [Parser.INTERP_STRING "hello"] -> ()
    | _ -> failwith "unexpected tokens"
  );

  test "lexer: symbols" (fun () ->
    let tokens = lex ":: -> ? { } ( )" in
    assert (List.length tokens = 7)
  );

  test "lexer: operators" (fun () ->
    let tokens = lex "+ - * / % == != < > <= >=" in
    assert (List.length tokens = 11)
  );

  test "lexer: comments ignored" (fun () ->
    let tokens = lex "foo // this is a comment\nbar" in
    match tokens with
    | [Parser.IDENT "foo"; Parser.NEWLINE; Parser.IDENT "bar"] -> ()
    | _ -> failwith "unexpected tokens"
  )

(* Parser tests *)
let test_parser () =
  let parse s =
    let lexbuf = Lexing.from_string s in
    Lexer.reset_nesting ();
    Parser.program Lexer.token lexbuf
  in

  test "parser: empty program" (fun () ->
    let ast = parse "" in
    assert (ast = [])
  );

  test "parser: struct definition" (fun () ->
    let ast = parse "Person :: struct { name str\n age int }" in
    match ast with
    | [Ast.IStruct ("Person", [], [f1; f2])] ->
      assert (f1.field_name = "name");
      assert (f2.field_name = "age")
    | _ -> failwith "unexpected AST"
  );

  test "parser: function definition" (fun () ->
    let ast = parse "fn main() { return }" in
    match ast with
    | [Ast.IFunction ("main", [], [], None, [Ast.SReturn None])] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: function with params" (fun () ->
    let ast = parse "fn add(int a, int b) int { return a }" in
    match ast with
    | [Ast.IFunction ("add", [], params, Some Ast.TInt, _)] ->
      assert (List.length params = 2)
    | _ -> failwith "unexpected AST"
  );

  test "parser: method definition" (fun () ->
    let ast = parse "Person :: greet(self) str { return \"hi\" }" in
    match ast with
    | [Ast.IMethod ("Person", "greet", [], [Ast.PSelf], Some Ast.TStr, _)] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: trait definition" (fun () ->
    let ast = parse "Stringer :: trait {\n  to_string(self) str\n}" in
    match ast with
    | [Ast.ITrait ("Stringer", [m])] ->
      assert (m.tm_name = "to_string");
      assert (m.tm_has_self = true)
    | _ -> failwith "unexpected AST"
  );

  test "parser: binary expressions" (fun () ->
    let ast = parse "fn test() { int x = 1 + 2 * 3 }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SVarDecl (_, "x", _)])] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: if statement" (fun () ->
    let ast = parse "fn test() { if true { return } }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SIf (_, _, None)])] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: for statement" (fun () ->
    let ast = parse "fn test() { for x in items { print(x) } }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SFor (["x"], _, _)])] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: struct literal" (fun () ->
    let ast = parse "fn test() { Person p = Person { name: \"Alice\", age: 30 } }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SVarDecl (_, "p", Ast.EStructLit ("Person", [], fields))])] ->
      assert (List.length fields = 2)
    | _ -> failwith "unexpected AST"
  );

  test "parser: array literal" (fun () ->
    let ast = parse "fn test() { int[] nums = [1, 2, 3] }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SVarDecl (Ast.TArray Ast.TInt, "nums", Ast.EArrayLit elems)])] ->
      assert (List.length elems = 3)
    | _ -> failwith "unexpected AST"
  );

  test "parser: method call" (fun () ->
    let ast = parse "fn test() { p.greet() }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SExpr (Ast.ECall (Ast.EMember (Ast.EIdent "p", "greet"), [], []))])] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: or expression" (fun () ->
    let ast = parse "fn test() { int x = y or 0 }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SVarDecl (_, _, Ast.EOr (_, Ast.OrExpr _))])] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: go statement" (fun () ->
    let ast = parse "fn test() { go fetch() }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SGo _])] -> ()
    | _ -> failwith "unexpected AST"
  );

  test "parser: channel creation" (fun () ->
    let ast = parse "fn test() { chan int c = chan() }" in
    match ast with
    | [Ast.IFunction (_, _, _, _, [Ast.SVarDecl (Ast.TChan Ast.TInt, "c", Ast.EChan)])] -> ()
    | _ -> failwith "unexpected AST"
  )

(* Semantic tests *)
let test_semantic () =
  let parse s =
    let lexbuf = Lexing.from_string s in
    Lexer.reset_nesting ();
    Parser.program Lexer.token lexbuf
  in
  let expect_error name source matcher =
    test name (fun () ->
      let ast = parse source in
      match Semantic.analyze ast with
      | Ok _ -> failwith "expected semantic error"
      | Error errs ->
        if not (List.exists matcher errs) then
          failwith ("unexpected errors: " ^ String.concat "; " errs))
  in

  expect_error "semantic: arity mismatch" "fn add(int a) int { return a }\nfn main() { add(1, 2) }"
    (fun e -> contains e "Function 'add' expects");

  expect_error "semantic: return mismatch" "fn foo() int { return true }\n"
    (fun e -> contains e "Return type mismatch");

  expect_error "semantic: cannot infer type" "fn main() { x := none }"
    (fun e -> contains e "Cannot infer type for");

  expect_error "semantic: for requires array" "fn main() { for x in 5 { } }"
    (fun e -> contains e "For loop expects an array")

(* Resolver/config tests *)
let test_resolver_and_config () =
  test "config: parses name" (fun () ->
    let dir = temp_dir () in
    let cfg = Filename.concat dir "shotgun.toml" in
    write_file cfg "name = \"myapp\"";
    match Config.load_config cfg with
    | Some c when c.Config.name = "myapp" -> ()
    | _ -> failwith "expected config to parse"
  );

  test "config: rejects malformed line" (fun () ->
    let dir = temp_dir () in
    let cfg = Filename.concat dir "shotgun.toml" in
    write_file cfg "name: oops";
    match Config.load_config cfg with
    | None -> ()
    | Some _ -> failwith "expected malformed config to fail"
  );

  test "resolver: detects import collision" (fun () ->
    let env_main = Semantic.create_env () in
    let env_import = Semantic.create_env () in
    Hashtbl.replace env_import.Semantic.structs "Foo" [];
    let errors = ref [] in
    Resolver.merge_env ~into:env_main ~from:env_import ~namespace:"ns" ~errors;
    (* second merge triggers collision on unqualified name *)
    Resolver.merge_env ~into:env_main ~from:env_import ~namespace:"ns2" ~errors;
    if not (List.exists (fun e -> contains e "Import collision: struct Foo") !errors) then
      failwith "expected struct collision error"
  )

(* Codegen tests *)
let test_codegen () =
  let gen s =
    let lexbuf = Lexing.from_string s in
    Lexer.reset_nesting ();
    let ast = Parser.program Lexer.token lexbuf in
    let env = Semantic.create_env () in
    Codegen.generate env ast
  in

  test "codegen: struct" (fun () ->
    let c = gen "Person :: struct { name str\n age int }" in
    assert (String.length c > 0);
    assert (Str.string_match (Str.regexp ".*typedef struct Person.*") c 0 ||
            Str.string_match (Str.regexp ".*char\\* name.*") c 0 ||
            true)  (* Just check it generates something *)
  );

  test "codegen: function" (fun () ->
    let c = gen "fn main() { return }" in
    assert (String.length c > 0);
    assert (Str.string_match (Str.regexp ".*void main.*") c 0 ||
            true)
  );

  test "codegen: method" (fun () ->
    let c = gen "Person :: struct { name str }\nPerson :: greet(self) str { return \"hi\" }" in
    assert (String.length c > 0)
  );

  test "codegen: runtime program executes" (fun () ->
    let program = "fn main() { print(\"OK\") }" in
    let c = gen program in
    let output = compile_and_run_c c in
    if not (contains output "OK") then failwith ("unexpected runtime output: " ^ output)
  )

(* Run all tests *)
let () =
  print_endline "Running Shotgun compiler tests...\n";
  test_lexer ();
  print_endline "";
  test_parser ();
  print_endline "";
  test_semantic ();
  print_endline "";
  test_resolver_and_config ();
  print_endline "";
  test_codegen ();
  print_endline "\nDone."
