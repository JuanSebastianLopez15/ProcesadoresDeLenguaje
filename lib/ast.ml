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

type literal =
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
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

type unop =
  | Not
  | Neg
  | Inc
  | Dec

type expr =
  | Lit of literal
  | Var of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Call of string * expr list
  | MethodCall of expr * string * expr list
  | Index of expr * expr
  | Selector of expr * string

type stmt =
  | Assign of expr list * expr list
  | ShortDecl of string * expr
  | If of expr * stmt list * stmt list option
  | ForCond of expr * stmt list
  | ForRange of string * string * expr * stmt list
  | Return of expr list
  | ExprStmt of expr
  | Defer of expr
  | Go of expr

type func_decl = {
  name : string;
  params : (string * typ) list;
  ret : typ list;
  body : stmt list;
}

type decl =
  | FuncDecl of func_decl
  | VarDecl of string * typ option * expr option

type program = {
  package : string;
  imports : string list;
  decls : decl list;
}