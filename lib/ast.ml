(* AST rico para frontend + middleware (typecheck). *)
type typ =
  | TInt
  | TFloat64
  | TString
  | TBool
  | TNil
  | TVoid
  | TAny
  | TSlice of typ
  | TMap of typ * typ
  | TFunc of typ list * typ list
  | TStruct of (string * typ) list  (* NUEVO: representa los campos de un struct *)
  | TInterface of (string * typ) list (* NUEVO: representa los métodos de una interfaz *)
  | TName of string                 (* NUEVO: representa un tipo personalizado como "Resultado" *)

type literal =
  | IntLit of Int64.t
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | RuneLit of int
  | NilLit

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | And
  | Or
  | BAnd
  | BOr
  | BXor
  | Shl
  | Shr
  | AndNot

type unop =
  | Not
  | Neg
  | Inc
  | Dec
  | AddrOf
  | Deref

type func_decl = {
  name : string;
  params : (string * typ) list;
  ret : typ list;
  body : stmt list;
}

and expr =
  | Lit of literal
  | Var of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Call of expr * expr list
  | MethodCall of expr * string * expr list
  | Index of expr * expr
  | Slice of expr * expr option * expr option * expr option
  | Selector of expr * string
  | StructLit of string * (string option * expr) list  (* NUEVO: Key: Value *)
  | SliceLit of typ * (string option * expr) list      (* NUEVO: Index: Value *)
  | Spread of expr
  | Cast of typ * expr
  | KeyedExpr of string * expr  (* Para soportar Key: Value en otros contextos *)
  | FuncLit of func_decl

and stmt =
  | Assign of expr list * expr list
  | MultiAssign of expr list * expr list
  | ShortDecl of string list * expr list
  | VarDeclStmt of string * typ option * expr option
  | TypeSwitch of string * expr * (string list * stmt list) list * stmt list option
  | If of expr * stmt list * stmt list option
  | IfInit of stmt * expr * stmt list * stmt list option
  | ForCond of expr * stmt list
  | ForClassic of stmt option * expr option * stmt option * stmt list
  | ForRange of string * string * expr * stmt list
  | Return of expr list
  | ExprStmt of expr
  | Defer of expr
  | Go of expr

type decl =
  | FuncDecl of func_decl
  | VarDecl of string * typ option * expr option
  | TypeDecl of string * typ         (* NUEVO: declarar 'type Resultado struct {...}' *)

type program = {
  package : string;
  imports : string list;
  decls : decl list;
}
