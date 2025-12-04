(* Error reporting for Shotgun compiler *)

type error_kind =
  | LexError of string
  | ParseError of string
  | TypeError of string
  | SemanticError of string

type error = {
  kind: error_kind;
  line: int;
  col: int;
  message: string;
}

let make_error kind line col message =
  { kind; line; col; message }

let format_error err =
  let kind_str = match err.kind with
    | LexError _ -> "Lexer error"
    | ParseError _ -> "Parse error"
    | TypeError _ -> "Type error"
    | SemanticError _ -> "Semantic error"
  in
  Printf.sprintf "%s at line %d, column %d: %s" kind_str err.line err.col err.message

let print_error err =
  prerr_endline (format_error err)
