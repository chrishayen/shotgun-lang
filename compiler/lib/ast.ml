(* Shotgun AST *)

type loc = {
  line: int;
  col: int;
}
[@@deriving show, eq]

let dummy_loc = { line = 0; col = 0 }

(* Types *)
type typ =
  | TInt
  | TStr
  | TBool
  | TF32
  | TF64
  | TU32
  | TU64
  | TOptional of typ
  | TArray of typ
  | TChan of typ
  | TUser of string
  | TResult of typ * string  (* value type, error type name *)
  | TVoid
[@@deriving show, eq]

(* Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or
[@@deriving show, eq]

(* Unary operators *)
type unop =
  | Not
[@@deriving show, eq]

(* Assignment operators *)
type assignop =
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
[@@deriving show, eq]

(* String parts for interpolation *)
type string_part =
  | SLiteral of string
  | SInterp of expr
[@@deriving show, eq]

(* Or clause variants *)
and or_clause =
  | OrExpr of expr
  | OrReturn of expr option
  | OrError of string * (string * expr) list  (* error type, field inits *)
  | OrWait of expr
[@@deriving show, eq]

(* Expressions *)
and expr =
  | EInt of int
  | EFloat of float
  | EString of string_part list
  | EBool of bool
  | ENone
  | EIdent of string
  | EBinary of binop * expr * expr
  | EUnary of unop * expr
  | ECall of expr * expr list
  | EMember of expr * string
  | EIndex of expr * expr
  | EOr of expr * or_clause
  | EStructLit of string * (string * expr) list
  | EArrayLit of expr list
  | EChan  (* chan() constructor *)
  | EParen of expr
  | EAssign of assignop * expr * expr
[@@deriving show, eq]

(* Patterns for match *)
type pattern =
  | PIdent of string
  | PConstructor of string * string option  (* Constructor name, optional binding *)
[@@deriving show, eq]

(* Statements *)
type stmt =
  | SVarDecl of typ * string * expr
  | SConstDecl of string * expr  (* name, initializer - type inferred *)
  | SReturn of expr option
  | SIf of expr * stmt list * stmt list option
  | SFor of string * expr * stmt list
  | SMatch of expr * (pattern * stmt list) list
  | SGo of expr
  | SExpr of expr
[@@deriving show, eq]

(* Struct field *)
type field = {
  field_name: string;
  field_type: typ;
}
[@@deriving show, eq]

(* Trait method signature *)
type trait_method = {
  tm_name: string;
  tm_params: (typ * string) list;  (* does not include self *)
  tm_has_self: bool;
  tm_return: typ option;
}
[@@deriving show, eq]

(* Function/method parameter *)
type param =
  | PSelf
  | PNamed of typ * string
[@@deriving show, eq]

(* Import path: std.io becomes ["std"; "io"] *)
type import_path = string list
[@@deriving show, eq]

(* Top-level items *)
type item =
  | IStruct of string * field list
  | ITrait of string * trait_method list
  | IImpl of string * string * impl_method list  (* type, trait, methods *)
  | IMethod of string * string * param list * typ option * stmt list  (* type, method name, params, return type, body *)
  | IFunction of string * param list * typ option * stmt list  (* name, params, return type, body *)
  | IError of string * field list
  | IUses of import_path list  (* uses { std.io, myapp.utils } *)
[@@deriving show, eq]

and impl_method = {
  im_name: string;
  im_params: param list;
  im_return: typ option;
  im_body: stmt list;
}
[@@deriving show, eq]

(* Program is a list of items *)
type program = item list
[@@deriving show, eq]
