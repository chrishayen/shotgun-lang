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
  | TChar
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
  | TParam of string              (* T - type parameter reference *)
  | TApply of string * typ list   (* List<int>, Option<str> - generic instantiation *)
  | TFunc of typ list * typ option  (* fn(int, str) bool - function type *)
[@@deriving show, eq]

(* Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or
  | In  (* array membership: x in [a, b, c] *)
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

(* Patterns for match *)
and pattern =
  | PIdent of string  (* simple identifier binding *)
  | PLiteral of expr  (* literal value: 200, "hello", true *)
  | PVariant of typ option * string * (string * string) list  (* enum type (None if using), variant name, [(field, binding)] *)
  | PWildcard  (* _ catch-all *)
  | PTuple of pattern list  (* (p1, p2, ...) *)
[@@deriving show, eq]

(* Expressions *)
and expr =
  | EInt of int
  | EFloat of float
  | EString of string_part list
  | EBool of bool
  | EChar of char
  | ENone
  | EIdent of string
  | EBinary of binop * expr * expr
  | EUnary of unop * expr
  | ECall of expr * typ list * expr list  (* callee, type args, args *)
  | EMember of expr * string
  | EIndex of expr * expr
  | EOr of expr * or_clause
  | EStructLit of string * typ list * (string * expr) list  (* name, type args, field inits *)
  | EEnumVariant of string * typ list * string * (string * expr) list  (* enum name, type args, variant name, field inits *)
  | EArrayLit of expr list
  | EChan  (* chan() constructor *)
  | EParen of expr
  | EAssign of assignop * expr * expr
  | EMatch of expr list * typ option * (pattern * stmt list) list  (* match exprs, optional using type, [(pattern, body)] *)
  | EAnonFn of (typ * string) list * typ option * stmt list * string list  (* params, return type, body, captured vars *)
  | EIs of expr * typ option * string  (* expr is Variant - expr, optional type qualifier, variant name *)

(* Statements - mutually recursive with expr for closures *)
and stmt =
  | SVarDecl of typ * string * expr
  | SVarDeclInfer of string * expr  (* name, initializer - type inferred with := *)
  | SConstDecl of string * expr  (* name, initializer - type inferred *)
  | SReturn of expr option
  | SBreak
  | SContinue
  | SIf of expr * stmt list * stmt list option
  | SFor of string * expr * stmt list
  | SWhile of expr * stmt list  (* condition, body *)
  | SGo of expr
  | SExpr of expr
[@@deriving show, eq]

(* Struct field *)
type field = {
  field_name: string;
  field_type: typ;
}
[@@deriving show, eq]

(* Enum variant *)
type enum_variant = {
  variant_name: string;
  variant_fields: field list;  (* empty for unit variants like None *)
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
  | IStruct of string * string list * field list  (* name, type_params, fields *)
  | IEnum of string * string list * enum_variant list  (* name, type_params, variants *)
  | ITrait of string * trait_method list
  | IImpl of string * string * impl_method list  (* type, trait, methods *)
  | IMethod of string * string * string list * param list * typ option * stmt list  (* type, method name, type_params, params, return type, body *)
  | IFunction of string * string list * param list * typ option * stmt list  (* name, type_params, params, return type, body *)
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
