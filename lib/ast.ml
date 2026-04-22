(* lib/ast.ml *)
type typ =
  | TInt                         (* int, int64 *)
  | TFloat                       (* float64 *)
  | TString                      (* string *)
  | TBool                        (* bool *)
  | TVoid                        (* tipo de retorno de funciones sin return *)
  | TSlice of typ                (* []T *)
  | TName of string              (* structs definidos por el usuario *)

(* Literales *)
type literal =
  | IntLit of int64
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool

(* Operadores binarios *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Leq | Geq
  | And | Or

(* Operadores unarios *)
type unop =
  | Not                          (* ! *)
  | Neg                          (* - (unario) *)

(* Expresiones *)
type expr =
  | Lit of literal
  | Var of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Call of string * expr list                     (* f(args) — solo funciones por nombre *)
  | Index of expr * expr                           (* e[i] *)
  | Selector of expr * string                      (* e.campo *)
  | StructLit of string * (string option * expr) list  (* Struct{...} posicional o nominal *)
  | SliceLit of typ * expr list
  | Cast of typ * expr                    (* NUEVO *)
                    (* []T{e1, e2, ...} *)

(* Statements *)
type stmt =
  | ShortDecl of string * expr                     (* x := e *)
  | Assign of string * expr                        (* x = e (reasignación) *)
  | FieldAssign of expr * expr                     (* lhs (Selector o Index) = rhs *)
  | If of expr * stmt list * stmt list option      (* if cond { ... } [else { ... }] *)
  | Return of expr option                          (* return [e] *)
  | ExprStmt of expr                               (* llamada como statement *)

(* Declaración de función top-level *)
type func_decl = {
  name : string;
  params : (string * typ) list;
  ret : typ;                     (* TVoid si no hay return *)
  body : stmt list;
}

(* Declaración de struct top-level *)
type struct_decl = {
  name : string;
  fields : (string * typ) list;
}

(* Declaraciones top-level *)
type decl =
  | FuncDecl of func_decl
  | StructDecl of struct_decl

(* Programa completo *)
type program = {
  package : string;
  imports : string list;
  decls : decl list;
}
