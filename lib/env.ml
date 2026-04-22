(* lib/env.ml *)
open Ast

type t = {
  vars : (string * typ) list;
  funcs : (string * (typ list * typ)) list;       (* params * ret *)
  structs : (string * (string * typ) list) list;  (* name → fields *)
}

let empty = { vars = []; funcs = []; structs = [] }

let add_var name t env = { env with vars = (name, t) :: env.vars }
let add_func name sig_ env = { env with funcs = (name, sig_) :: env.funcs }
let add_struct name fields env = { env with structs = (name, fields) :: env.structs }

let lookup_var name env = List.assoc_opt name env.vars
let lookup_func name env = List.assoc_opt name env.funcs
let lookup_struct name env = List.assoc_opt name env.structs
