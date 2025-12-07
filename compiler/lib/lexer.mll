{
open Parser

exception Lexer_error of string

let keywords = [
  "fn", FN;
  "struct", STRUCT;
  "variant", VARIANT;
  "trait", TRAIT;
  "impl", IMPL;
  "error", ERROR;
  "return", RETURN;
  "if", IF;
  "else", ELSE;
  "for", FOR;
  "while", WHILE;
  "in", IN;
  "match", MATCH;
  "go", GO;
  "chan", CHAN;
  "wait", WAIT;
  "and", AND;
  "or", OR;
  "not", NOT;
  "self", SELF;
  "none", NONE;
  "true", TRUE;
  "false", FALSE;
  "str", STR;
  "int", INT;
  "bool", BOOL;
  "f32", F32;
  "f64", F64;
  "u32", U32;
  "u64", U64;
  "const", CONST;
  "uses", USES;
  "using", USING;
]

let keyword_table = Hashtbl.create 50
let () = List.iter (fun (k, v) -> Hashtbl.add keyword_table k v) keywords

let lookup_ident s =
  try Hashtbl.find keyword_table s
  with Not_found -> IDENT s

(* Track nesting for continuation inside braces/parens/brackets *)
let nesting_depth = ref 0

let incr_nesting () = incr nesting_depth
let decr_nesting () = decr nesting_depth
let in_nested () = !nesting_depth > 0

let reset_nesting () = nesting_depth := 0

(* Parse string interpolations: "Hello, {name}!" -> [("Hello, ", Some "name"), ("!", None)] *)
type string_segment = { text: string; interp: string option }

let parse_interpolated_string s =
  let len = String.length s in
  let segments = ref [] in
  let text_buf = Buffer.create 64 in
  let i = ref 0 in
  while !i < len do
    if s.[!i] = '\\' && !i + 1 < len then begin
      (match s.[!i + 1] with
       | 'n' -> Buffer.add_char text_buf '\n'
       | 'r' -> Buffer.add_char text_buf '\r'
       | 't' -> Buffer.add_char text_buf '\t'
       | '\\' -> Buffer.add_char text_buf '\\'
       | '"' -> Buffer.add_char text_buf '"'
       | '{' -> Buffer.add_char text_buf '{'
       | c -> Buffer.add_char text_buf '\\'; Buffer.add_char text_buf c);
      i := !i + 2
    end else if s.[!i] = '{' then begin
      (* Start interpolation *)
      let interp_start = !i + 1 in
      let depth = ref 1 in
      incr i;
      while !i < len && !depth > 0 do
        if s.[!i] = '{' then incr depth
        else if s.[!i] = '}' then decr depth;
        if !depth > 0 then incr i
      done;
      let interp_end = !i in
      let interp = String.sub s interp_start (interp_end - interp_start) in
      segments := { text = Buffer.contents text_buf; interp = Some interp } :: !segments;
      Buffer.clear text_buf;
      incr i  (* skip closing } *)
    end else begin
      Buffer.add_char text_buf s.[!i];
      incr i
    end
  done;
  if Buffer.length text_buf > 0 then
    segments := { text = Buffer.contents text_buf; interp = None } :: !segments;
  List.rev !segments
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident_start = ['a'-'z' '_']
let ident_char = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let type_start = ['A'-'Z']
let whitespace = [' ' '\t']
let newline = '\n' | '\r' '\n'

rule token = parse
  | whitespace+ { token lexbuf }
  | newline {
      Lexing.new_line lexbuf;
      if in_nested () then token lexbuf
      else NEWLINE
    }
  | "//" [^ '\n']* { token lexbuf }  (* line comment *)

  (* Wildcard pattern *)
  | "_" { UNDERSCORE }

  (* Keywords and identifiers *)
  | ident_start ident_char* as s { lookup_ident s }
  | type_start ident_char* as s { TYPE_IDENT s }

  (* Numbers *)
  | digit+ '.' digit+ as s { FLOAT (float_of_string s) }
  | digit+ as s { INTEGER (int_of_string s) }

  (* String - read entire string including interpolations *)
  | '"' { read_string (Buffer.create 64) lexbuf }

  (* Symbols *)
  | "::" { COLONCOLON }
  | ":=" { COLONEQ }
  | "->" { ARROW }
  | "?" { QUESTION }
  | "{" { incr_nesting (); LBRACE }
  | "}" { decr_nesting (); RBRACE }
  | "(" { incr_nesting (); LPAREN }
  | ")" { decr_nesting (); RPAREN }
  | "[" { incr_nesting (); LBRACKET }
  | "]" { decr_nesting (); RBRACKET }
  | "," { COMMA }
  | ":" { COLON }
  | "." { DOT }
  | "+=" { PLUSEQ }
  | "-=" { MINUSEQ }
  | "*=" { STAREQ }
  | "/=" { SLASHEQ }
  | "==" { EQEQ }
  | "!=" { NEQ }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<" { LT }
  | ">" { GT }
  | "=" { EQ }
  | "+" { PLUS }
  | "-" { if in_nested () then MINUS else DASH }
  | "*" { STAR }
  | "/" { SLASH }
  | "%" { PERCENT }
  | "||" { OROR }

  | eof { EOF }
  | _ as c { raise (Lexer_error (Printf.sprintf "Unexpected character: %c" c)) }

(* Read a complete string, tracking interpolation braces *)
and read_string buf = parse
  | '"' { INTERP_STRING (Buffer.contents buf) }
  | '\\' 'n' { Buffer.add_string buf "\\n"; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_string buf "\\r"; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_string buf "\\t"; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_string buf "\\\\"; read_string buf lexbuf }
  | '\\' '"' { Buffer.add_string buf "\\\""; read_string buf lexbuf }
  | '\\' '{' { Buffer.add_string buf "\\{"; read_string buf lexbuf }
  | '{' { Buffer.add_char buf '{'; read_interp_expr buf 1 lexbuf }
  | newline { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | eof { raise (Lexer_error "Unterminated string") }
  | _ as c { Buffer.add_char buf c; read_string buf lexbuf }

(* Read interpolation expression, tracking nested braces *)
and read_interp_expr buf depth = parse
  | '{' { Buffer.add_char buf '{'; read_interp_expr buf (depth + 1) lexbuf }
  | '}' {
      Buffer.add_char buf '}';
      if depth = 1 then read_string buf lexbuf
      else read_interp_expr buf (depth - 1) lexbuf
    }
  | '"' { Buffer.add_char buf '"'; read_interp_string_in_expr buf depth lexbuf }
  | newline { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_interp_expr buf depth lexbuf }
  | eof { raise (Lexer_error "Unterminated string interpolation") }
  | _ as c { Buffer.add_char buf c; read_interp_expr buf depth lexbuf }

(* Read a string literal inside an interpolation expression *)
and read_interp_string_in_expr buf depth = parse
  | '"' { Buffer.add_char buf '"'; read_interp_expr buf depth lexbuf }
  | '\\' '"' { Buffer.add_string buf "\\\""; read_interp_string_in_expr buf depth lexbuf }
  | '\\' _ as s { Buffer.add_string buf s; read_interp_string_in_expr buf depth lexbuf }
  | newline { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_interp_string_in_expr buf depth lexbuf }
  | eof { raise (Lexer_error "Unterminated string in interpolation") }
  | _ as c { Buffer.add_char buf c; read_interp_string_in_expr buf depth lexbuf }
