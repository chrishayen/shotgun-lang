%{
open Ast

(* Parse a simple expression for interpolation: "x" or "x.y" or "x.y.z" *)
let parse_interp_expr s =
  let parts = String.split_on_char '.' s in
  match parts with
  | [] -> EIdent s
  | [single] -> EIdent single
  | first :: rest ->
    List.fold_left (fun acc field -> EMember (acc, field)) (EIdent first) rest

(* Parse interpolated string into parts *)
let parse_interp_string s =
  let len = String.length s in
  let parts = ref [] in
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
      (* Save text before interpolation *)
      if Buffer.length text_buf > 0 then begin
        parts := SLiteral (Buffer.contents text_buf) :: !parts;
        Buffer.clear text_buf
      end;
      (* Find matching closing brace *)
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
      (* Parse the interpolation expression *)
      parts := SInterp (parse_interp_expr interp) :: !parts;
      incr i  (* skip closing } *)
    end else begin
      Buffer.add_char text_buf s.[!i];
      incr i
    end
  done;
  if Buffer.length text_buf > 0 then
    parts := SLiteral (Buffer.contents text_buf) :: !parts;
  List.rev !parts
%}

(* Tokens *)
%token <int> INTEGER
%token <float> FLOAT
%token <string> INTERP_STRING  (* full string including interpolation markers *)
%token <string> IDENT
%token <string> TYPE_IDENT

(* Keywords *)
%token FN STRUCT VARIANT TRAIT IMPL ERROR
%token RETURN IF ELSE FOR WHILE IN MATCH USING
%token GO CHAN WAIT
%token OR
%token ANDAND BANG  (* && and ! *)
%token SELF NONE TRUE FALSE CONST
%token USES
%token UNDERSCORE

(* Type keywords *)
%token STR INT BOOL F32 F64 U32 U64

(* Symbols *)
%token COLONCOLON COLONEQ ARROW QUESTION DASH
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token COMMA COLON DOT
%token EQ PLUSEQ MINUSEQ STAREQ SLASHEQ
%token PLUS MINUS STAR SLASH PERCENT
%token EQEQ NEQ LT GT LTE GTE  (* LT/GT also used for generic type params *)
%token OROR  (* || for boolean or *)

%token NEWLINE EOF

(* Precedence - lowest to highest *)
%left OROR
%left OR
%left ANDAND
%left EQEQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left STAR SLASH PERCENT
%right BANG
%left DOT LPAREN LBRACKET

%start <Ast.program> program

%%

program:
  | items = list(item) EOF { items }
  ;

item:
  | newlines struct_def = struct_definition newlines { struct_def }
  | newlines variant_def = variant_definition newlines { variant_def }
  | newlines trait_def = trait_definition newlines { trait_def }
  | newlines impl_block = impl_block_def newlines { impl_block }
  | newlines method_def = method_definition newlines { method_def }
  | newlines func_def = function_definition newlines { func_def }
  | newlines err_def = error_definition newlines { err_def }
  | newlines uses_block = uses_block_def newlines { uses_block }
  ;

(* uses: std.io, myapp.utils  OR  uses:\n - std.io\n - std.json *)
uses_block_def:
  | USES COLON paths = inline_import_list { IUses paths }
  | USES COLON NEWLINE paths = dash_import_list { IUses paths }
  ;

(* Single line: uses: std.io, std.json *)
inline_import_list:
  | p = import_path { [p] }
  | p = import_path COMMA rest = inline_import_list { p :: rest }
  ;

(* Multi-line with dashes: - std.io\n - std.json *)
dash_import_list:
  | (* empty *) { [] }
  | DASH p = import_path newlines rest = dash_import_list { p :: rest }
  ;

import_path:
  | first = IDENT rest = import_path_rest { first :: rest }
  ;

import_path_rest:
  | (* empty *) { [] }
  | DOT name = IDENT rest = import_path_rest { name :: rest }
  ;

newlines:
  | (* empty *) { () }
  | NEWLINE newlines { () }
  ;

(* Type parameters: <T>, <T, U>, etc. - must be uppercase like type names *)
type_params:
  | LT params = separated_nonempty_list(COMMA, TYPE_IDENT) GT { params }
  ;

(* Person :: struct { ... } or List<T> :: struct { ... } *)
struct_definition:
  | name = TYPE_IDENT COLONCOLON STRUCT LBRACE fields = list(field_def) RBRACE
    { IStruct (name, [], fields) }
  | name = TYPE_IDENT tparams = type_params COLONCOLON STRUCT LBRACE fields = list(field_def) RBRACE
    { IStruct (name, tparams, fields) }
  ;

field_def:
  | newlines name = IDENT typ = typ newlines { { field_name = name; field_type = typ } }
  ;

(* Option :: variant { ... } or Option<T> :: variant { ... } *)
variant_definition:
  | name = TYPE_IDENT COLONCOLON VARIANT LBRACE variants = list(variant_case_def) RBRACE
    { IEnum (name, [], variants) }
  | name = TYPE_IDENT tparams = type_params COLONCOLON VARIANT LBRACE variants = list(variant_case_def) RBRACE
    { IEnum (name, tparams, variants) }
  ;

variant_case_def:
  | newlines name = TYPE_IDENT newlines
    { { variant_name = name; variant_fields = [] } }
  | newlines name = TYPE_IDENT LBRACE fields = list(field_def) RBRACE newlines
    { { variant_name = name; variant_fields = fields } }
  ;

(* Stringer :: trait { ... } *)
trait_definition:
  | name = TYPE_IDENT COLONCOLON TRAIT LBRACE methods = list(trait_method_def) RBRACE
    { ITrait (name, methods) }
  ;

trait_method_def:
  | newlines name = IDENT LPAREN params = param_list RPAREN ret = option(typ) newlines
    {
      let has_self = List.exists (fun p -> p = PSelf) params in
      let named_params = List.filter_map (function
        | PSelf -> None
        | PNamed (t, n) -> Some (t, n)
      ) params in
      { tm_name = name; tm_params = named_params; tm_has_self = has_self; tm_return = ret }
    }
  ;

(* Person :: impl Stringer { ... } *)
impl_block_def:
  | type_name = TYPE_IDENT COLONCOLON IMPL trait_name = TYPE_IDENT
    LBRACE methods = list(impl_method_def) RBRACE
    { IImpl (type_name, trait_name, methods) }
  ;

impl_method_def:
  | newlines name = IDENT LPAREN params = param_list RPAREN ret = option(typ) body = block newlines
    { { im_name = name; im_params = params; im_return = ret; im_body = body } }
  ;

(* Person :: greet(self) str { ... } or Box<T> :: get(self) T { ... } *)
method_definition:
  | type_name = TYPE_IDENT COLONCOLON method_name = IDENT
    LPAREN params = param_list RPAREN ret = option(return_type) body = block
    { IMethod (type_name, method_name, [], params, ret, body) }
  | type_name = TYPE_IDENT COLONCOLON method_name = IDENT tparams = type_params
    LPAREN params = param_list RPAREN ret = option(return_type) body = block
    { IMethod (type_name, method_name, tparams, params, ret, body) }
  | type_name = TYPE_IDENT tparams = type_params COLONCOLON method_name = IDENT
    LPAREN params = param_list RPAREN ret = option(return_type) body = block
    { IMethod (type_name, method_name, tparams, params, ret, body) }
  ;

(* fn main { ... } or fn name<T>(params) ret { ... } *)
function_definition:
  | FN name = IDENT body = block
    { IFunction (name, [], [], None, body) }
  | FN name = IDENT LPAREN params = param_list RPAREN ret = option(return_type) body = block
    { IFunction (name, [], params, ret, body) }
  | FN name = IDENT tparams = type_params LPAREN params = param_list RPAREN ret = option(return_type) body = block
    { IFunction (name, tparams, params, ret, body) }
  ;

(* FileNotFound :: error { ... } *)
error_definition:
  | name = TYPE_IDENT COLONCOLON ERROR LBRACE fields = list(field_def) RBRACE
    { IError (name, fields) }
  ;

return_type:
  | t = typ { t }
  | t = typ OR ERROR { TResult (t, "Error") }
  ;

param_list:
  | (* empty *) { [] }
  | p = param { [p] }
  | p = param COMMA rest = param_list { p :: rest }
  ;

param:
  | SELF { PSelf }
  | t = typ name = IDENT { PNamed (t, name) }
  ;

typ:
  | STR { TStr }
  | INT { TInt }
  | BOOL { TBool }
  | F32 { TF32 }
  | F64 { TF64 }
  | U32 { TU32 }
  | U64 { TU64 }
  | CHAN t = typ { TChan t }
  | t = primitive_or_user LBRACKET RBRACKET { TArray t }
  | name = TYPE_IDENT { TUser name }
  | name = TYPE_IDENT LT args = separated_nonempty_list(COMMA, typ) GT { TApply (name, args) }
  | name = TYPE_IDENT LT args = separated_nonempty_list(COMMA, typ) GT LBRACKET RBRACKET { TArray (TApply (name, args)) }
  | name = TYPE_IDENT LT args = separated_nonempty_list(COMMA, typ) GT QUESTION { TOptional (TApply (name, args)) }
  | t = primitive_or_user QUESTION { TOptional t }
  (* Function type: fn(int, str) bool or fn() for void *)
  | FN LPAREN RPAREN { TFunc ([], None) }
  | FN LPAREN RPAREN ret = typ { TFunc ([], Some ret) }
  | FN LPAREN tlist = separated_nonempty_list(COMMA, typ) RPAREN { TFunc (tlist, None) }
  | FN LPAREN tlist = separated_nonempty_list(COMMA, typ) RPAREN ret = typ { TFunc (tlist, Some ret) }
  ;

primitive_or_user:
  | STR { TStr }
  | INT { TInt }
  | BOOL { TBool }
  | F32 { TF32 }
  | F64 { TF64 }
  | U32 { TU32 }
  | U64 { TU64 }
  | name = TYPE_IDENT { TUser name }
  ;

block:
  | LBRACE stmts = list(stmt) RBRACE { stmts }
  ;

stmt:
  | newlines s = stmt_inner newlines { s }
  ;

stmt_inner:
  | t = typ name = IDENT EQ e = expr { SVarDecl (t, name, e) }
  | name = IDENT COLONEQ e = expr { SVarDeclInfer (name, e) }
  | CONST name = IDENT EQ e = expr { SConstDecl (name, e) }
  | RETURN { SReturn None }
  | RETURN e = expr { SReturn (Some e) }
  | IF cond = expr then_block = block { SIf (cond, then_block, None) }
  | IF cond = expr then_block = block ELSE else_block = block { SIf (cond, then_block, Some else_block) }
  | IF cond = expr then_block = block ELSE else_if = if_stmt { SIf (cond, then_block, Some [else_if]) }
  | FOR var = IDENT IN iter = expr body = block { SFor (var, iter, body) }
  | WHILE cond = expr body = block { SWhile (cond, body) }
  | GO e = expr { SGo e }
  | e = expr { SExpr e }
  ;

if_stmt:
  | IF cond = expr then_block = block { SIf (cond, then_block, None) }
  | IF cond = expr then_block = block ELSE else_block = block { SIf (cond, then_block, Some else_block) }
  | IF cond = expr then_block = block ELSE else_if = if_stmt { SIf (cond, then_block, Some [else_if]) }
  ;


pattern:
  | UNDERSCORE { PWildcard }
  | name = IDENT { PIdent name }
  | i = INTEGER { PLiteral (EInt i) }
  | TRUE { PLiteral (EBool true) }
  | FALSE { PLiteral (EBool false) }
  | s = INTERP_STRING { PLiteral (EString (parse_interp_string s)) }
  (* Qualified variant: Expr.Int or Option<int>.Some { value: v } *)
  | enum_name = TYPE_IDENT DOT variant_name = TYPE_IDENT
    { PVariant (Some (TUser enum_name), variant_name, []) }
  | enum_name = TYPE_IDENT DOT variant_name = TYPE_IDENT LBRACE bindings = pattern_field_list RBRACE
    { PVariant (Some (TUser enum_name), variant_name, bindings) }
  | enum_name = TYPE_IDENT LT targs = separated_nonempty_list(COMMA, typ) GT DOT variant_name = TYPE_IDENT
    { PVariant (Some (TApply (enum_name, targs)), variant_name, []) }
  | enum_name = TYPE_IDENT LT targs = separated_nonempty_list(COMMA, typ) GT DOT variant_name = TYPE_IDENT LBRACE bindings = pattern_field_list RBRACE
    { PVariant (Some (TApply (enum_name, targs)), variant_name, bindings) }
  (* Unqualified variant (with using): Int or Int { value: v } *)
  | variant_name = TYPE_IDENT
    { PVariant (None, variant_name, []) }
  | variant_name = TYPE_IDENT LBRACE bindings = pattern_field_list RBRACE
    { PVariant (None, variant_name, bindings) }
  (* Tuple pattern: (p1, p2) *)
  | LPAREN p1 = pattern COMMA p2 = pattern RPAREN { PTuple [p1; p2] }
  | LPAREN p1 = pattern COMMA p2 = pattern COMMA rest = pattern_list RPAREN { PTuple (p1 :: p2 :: rest) }
  ;

pattern_list:
  | p = pattern { [p] }
  | p = pattern COMMA rest = pattern_list { p :: rest }
  ;

pattern_field_list:
  | (* empty *) { [] }
  | f = pattern_field { [f] }
  | f = pattern_field COMMA rest = pattern_field_list { f :: rest }
  ;

pattern_field:
  | field_name = IDENT COLON binding = IDENT { (field_name, binding) }
  ;

expr:
  | e = bool_or_expr { e }
  ;

bool_or_expr:
  | e = or_expr { e }
  | l = bool_or_expr OROR r = or_expr { EBinary (Or, l, r) }
  ;

or_expr:
  | e = and_expr { e }
  | e = and_expr OR clause = or_clause { EOr (e, clause) }
  ;

or_clause:
  | e = and_expr { OrExpr e }
  | RETURN { OrReturn None }
  | RETURN e = and_expr { OrReturn (Some e) }
  | ERROR name = TYPE_IDENT { OrError (name, []) }
  | ERROR name = TYPE_IDENT LBRACE fields = field_init_list RBRACE { OrError (name, fields) }
  | WAIT LPAREN e = expr RPAREN { OrWait e }
  ;

and_expr:
  | e = cmp_expr { e }
  | l = and_expr ANDAND r = cmp_expr { EBinary (And, l, r) }
  ;

cmp_expr:
  | e = add_expr { e }
  | l = cmp_expr EQEQ r = add_expr { EBinary (Eq, l, r) }
  | l = cmp_expr NEQ r = add_expr { EBinary (Neq, l, r) }
  | l = cmp_expr LT r = add_expr { EBinary (Lt, l, r) }
  | l = cmp_expr GT r = add_expr { EBinary (Gt, l, r) }
  | l = cmp_expr LTE r = add_expr { EBinary (Lte, l, r) }
  | l = cmp_expr GTE r = add_expr { EBinary (Gte, l, r) }
  | l = cmp_expr IN r = add_expr { EBinary (In, l, r) }
  ;

add_expr:
  | e = mul_expr { e }
  | l = add_expr PLUS r = mul_expr { EBinary (Add, l, r) }
  | l = add_expr MINUS r = mul_expr { EBinary (Sub, l, r) }
  ;

mul_expr:
  | e = unary_expr { e }
  | l = mul_expr STAR r = unary_expr { EBinary (Mul, l, r) }
  | l = mul_expr SLASH r = unary_expr { EBinary (Div, l, r) }
  | l = mul_expr PERCENT r = unary_expr { EBinary (Mod, l, r) }
  ;

unary_expr:
  | e = postfix_expr { e }
  | BANG e = unary_expr { EUnary (Not, e) }
  ;

postfix_expr:
  | e = primary_expr { e }
  | e = postfix_expr DOT member = IDENT { EMember (e, member) }
  | e = postfix_expr LPAREN args = arg_list RPAREN { ECall (e, [], args) }
  (* Generic function calls require explicit syntax: first::<int>(arr) *)
  | e = postfix_expr COLONCOLON LT targs = separated_nonempty_list(COMMA, typ) GT LPAREN args = arg_list RPAREN { ECall (e, targs, args) }
  | e = postfix_expr LBRACKET idx = expr RBRACKET { EIndex (e, idx) }
  | e = postfix_expr EQ rhs = expr { EAssign (Assign, e, rhs) }
  | e = postfix_expr PLUSEQ rhs = expr { EAssign (AddAssign, e, rhs) }
  | e = postfix_expr MINUSEQ rhs = expr { EAssign (SubAssign, e, rhs) }
  | e = postfix_expr STAREQ rhs = expr { EAssign (MulAssign, e, rhs) }
  | e = postfix_expr SLASHEQ rhs = expr { EAssign (DivAssign, e, rhs) }
  ;

(* Type arguments in expressions: <int>, <str, int>, etc. *)
type_args:
  | (* empty *) { [] }
  | LT args = separated_nonempty_list(COMMA, typ) GT { args }
  ;

primary_expr:
  | i = INTEGER { EInt i }
  | f = FLOAT { EFloat f }
  | s = string_expr { s }
  | TRUE { EBool true }
  | FALSE { EBool false }
  | NONE { ENone }
  | SELF { EIdent "self" }
  | name = IDENT { EIdent name }
  | CHAN LPAREN RPAREN { EChan }
  (* Struct literal: Person { ... } or List<int> { ... } *)
  | name = TYPE_IDENT LBRACE fields = field_init_list RBRACE { EStructLit (name, [], fields) }
  | name = TYPE_IDENT LT targs = separated_nonempty_list(COMMA, typ) GT LBRACE fields = field_init_list RBRACE { EStructLit (name, targs, fields) }
  (* Enum variant: Option.None or Option<int>.Some { value: 42 } *)
  | enum_name = TYPE_IDENT DOT variant_name = TYPE_IDENT { EEnumVariant (enum_name, [], variant_name, []) }
  | enum_name = TYPE_IDENT DOT variant_name = TYPE_IDENT LBRACE fields = field_init_list RBRACE { EEnumVariant (enum_name, [], variant_name, fields) }
  | enum_name = TYPE_IDENT LT targs = separated_nonempty_list(COMMA, typ) GT DOT variant_name = TYPE_IDENT { EEnumVariant (enum_name, targs, variant_name, []) }
  | enum_name = TYPE_IDENT LT targs = separated_nonempty_list(COMMA, typ) GT DOT variant_name = TYPE_IDENT LBRACE fields = field_init_list RBRACE { EEnumVariant (enum_name, targs, variant_name, fields) }
  | LBRACKET elems = array_elems RBRACKET { EArrayLit elems }
  | LPAREN e = expr RPAREN { EParen e }
  (* Match expression - single value *)
  | MATCH e = expr LBRACE arms = match_arm_list RBRACE { EMatch ([e], None, arms) }
  | MATCH e = expr USING t = typ LBRACE arms = match_arm_list RBRACE { EMatch ([e], Some t, arms) }
  (* Match expression - tuple *)
  | MATCH LPAREN e1 = expr COMMA e2 = expr RPAREN LBRACE arms = match_arm_list RBRACE { EMatch ([e1; e2], None, arms) }
  | MATCH LPAREN e1 = expr COMMA e2 = expr RPAREN USING t = typ LBRACE arms = match_arm_list RBRACE { EMatch ([e1; e2], Some t, arms) }
  (* Anonymous function: fn(int n) { return n * 2 } - return type inferred, captures filled in by semantic analysis *)
  | FN LPAREN params = param_list RPAREN body = block
    { let named_params = List.filter_map (function PSelf -> None | PNamed (t, n) -> Some (t, n)) params in
      EAnonFn (named_params, None, body, []) }
  | FN LPAREN params = param_list RPAREN ret = typ body = block
    { let named_params = List.filter_map (function PSelf -> None | PNamed (t, n) -> Some (t, n)) params in
      EAnonFn (named_params, Some ret, body, []) }
  ;

(* Match arm list - comma separated *)
match_arm_list:
  | newlines { [] }
  | newlines arm = match_expr_arm newlines { [arm] }
  | newlines arm = match_expr_arm COMMA rest = match_arm_list { arm :: rest }
  ;

(* Match expression arm: pattern -> expr *)
match_expr_arm:
  | p = pattern ARROW e = expr { (p, e) }
  ;

string_expr:
  | s = INTERP_STRING { EString (parse_interp_string s) }
  ;

field_init_list:
  | (* empty *) { [] }
  | f = field_init { [f] }
  | f = field_init COMMA rest = field_init_list { f :: rest }
  ;

field_init:
  | name = IDENT COLON e = expr { (name, e) }
  ;

arg_list:
  | (* empty *) { [] }
  | e = expr { [e] }
  | e = expr COMMA rest = arg_list { e :: rest }
  ;

array_elems:
  | (* empty *) { [] }
  | e = expr { [e] }
  | e = expr COMMA rest = array_elems { e :: rest }
  ;
