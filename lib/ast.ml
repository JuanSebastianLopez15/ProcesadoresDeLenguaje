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
  | TName of string                 (* NUEVO: representa un tipo personalizado como "Resultado" *)

type literal =
  | IntLit of Int64.t
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
  | Call of expr * expr list
  | MethodCall of expr * string * expr list
  | Index of expr * expr
  | Slice of expr * expr option * expr option * expr option
  | Selector of expr * string
  | StructLit of string * expr list  (* NUEVO: instanciar struct Resultado{num, clas} *)
  | SliceLit of typ * expr list      (* NUEVO: instanciar arreglo []int64{1, 2, 3} *)
  | Cast of typ * expr               (* NUEVO: casteos explícitos como int64(-1) o []rune("palabra") *)

type stmt =
  | Assign of expr list * expr list
  | MultiAssign of expr list * expr list
  | ShortDecl of string * expr
  | MultiShortDecl of string list * expr list
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
  | TypeDecl of string * typ         (* NUEVO: declarar 'type Resultado struct {...}' *)

type program = {
  package : string;
  imports : string list;
  decls : decl list;
}