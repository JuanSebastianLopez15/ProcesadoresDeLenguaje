open Lib.Ast

let indent n = String.make (2 * n) ' '

type ctx = {
  indent: int;
  mutable_vars: string list;
  var_types: (string * typ) list;
  func_ret_types: (string * typ) list;
}

let empty_ctx = { indent = 0; mutable_vars = []; var_types = []; func_ret_types = [] }
let with_indent ctx = { ctx with indent = ctx.indent + 1 }

let lookup_var_type ctx x =
  match List.assoc_opt x ctx.var_types with
  | Some t -> t
  | None -> TAny

let lookup_func_ret_type ctx name =
  match List.assoc_opt name ctx.func_ret_types with
  | Some t -> t
  | None -> TAny

(* expr_to_ocaml and infer_expr_type are defined later, after operator helpers. *)

(* ---------------------------------------------------------------------- *)
(*  Tipos OCaml                                                           *)
(* ---------------------------------------------------------------------- *)

let rec collect_mutable_in_expr = function
  | Lit _ | Var _ -> []
  | BinOp (_, l, r) -> collect_mutable_in_expr l @ collect_mutable_in_expr r
  | UnOp (_, e) -> collect_mutable_in_expr e
  | Call (_, args) -> List.concat_map collect_mutable_in_expr args
  | MethodCall (obj, _, args) ->
      collect_mutable_in_expr obj @ List.concat_map collect_mutable_in_expr args
  | Index (arr, idx) -> collect_mutable_in_expr arr @ collect_mutable_in_expr idx
  | Selector (e, _) -> collect_mutable_in_expr e

let rec collect_mutable_in_stmt = function
  | Assign (lhs, rhs) ->
      let lhs_vars = List.concat_map collect_mutable_in_expr lhs in
      let rhs_vars = List.concat_map collect_mutable_in_expr rhs in
      let mutable_names = List.filter_map (function Var x -> Some x | _ -> None) lhs in
      mutable_names @ lhs_vars @ rhs_vars
  | ShortDecl (_, e) -> collect_mutable_in_expr e
  | If (cond, then_b, else_opt) ->
      let cond_vars = collect_mutable_in_expr cond in
      let then_vars = List.concat_map collect_mutable_in_stmt then_b in
      let else_vars = match else_opt with Some b -> List.concat_map collect_mutable_in_stmt b | None -> [] in
      cond_vars @ then_vars @ else_vars
  | ForCond (cond, body) ->
      collect_mutable_in_expr cond @ List.concat_map collect_mutable_in_stmt body
  | ForRange (_, _, coll, body) ->
      collect_mutable_in_expr coll @ List.concat_map collect_mutable_in_stmt body
  | Return exprs -> List.concat_map collect_mutable_in_expr exprs
  | ExprStmt e -> collect_mutable_in_expr e
  | Defer e | Go e -> collect_mutable_in_expr e

let collect_mutable_in_func (fd: func_decl) : string list =
  let stmts = fd.body in
  let assign_targets = List.concat_map (function
      | Assign (lhs, _) ->
          List.filter_map (function Var x -> Some x | _ -> None) lhs
      | ShortDecl (x, _) -> [x]
      | _ -> []) stmts
  in
  let inc_dec_targets = List.concat_map (function
      | ExprStmt (UnOp ((Inc|Dec), e)) -> (match e with Var x -> [x] | _ -> [])
      | _ -> []) stmts
  in
  assign_targets @ inc_dec_targets

(* ---------------------------------------------------------------------- *)
(*  Tipos OCaml                                                           *)
(* ---------------------------------------------------------------------- *)

let rec ocaml_type_of_typ = function
  | TInt -> "int"
  | TFloat64 -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TNil | TVoid -> "unit"
  | TAny -> "Obj.t"
  | TSlice t -> Printf.sprintf "%s array" (ocaml_type_of_typ t)
  | TMap (k, v) -> Printf.sprintf "(%s, %s) Hashtbl.t" (ocaml_type_of_typ k) (ocaml_type_of_typ v)
  | TFunc (params, rets) ->
      let ps = String.concat " -> " (List.map ocaml_type_of_typ params) in
      let rs = match rets with [] -> "unit" | [r] -> ocaml_type_of_typ r | _ -> "(" ^ String.concat " * " (List.map ocaml_type_of_typ rets) ^ ")" in
      if params = [] then rs else ps ^ " -> " ^ rs

(* ---------------------------------------------------------------------- *)
(*  Operadores                                                            *)
(* ---------------------------------------------------------------------- *)

let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "mod"
  | Eq -> "=" | Neq -> "<>" | Lt -> "<" | Gt -> ">"
  | Leq -> "<=" | Geq -> ">=" | And -> "&&" | Or -> "||"

(* ---------------------------------------------------------------------- *)
(*  Escape de strings (mantiene tildes y UTF-8)                           *)
(* ---------------------------------------------------------------------- *)

let escape_ocaml_string s =
  let buf = Buffer.create (String.length s + 10) in
  Buffer.add_char buf '"';
  String.iter (function
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c   (* UTF-8 intacto, incluye tildes *)
  ) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* ---------------------------------------------------------------------- *)
(*  Convertir expresión a string para print_endline                       *)
(* ---------------------------------------------------------------------- *)

let rec expr_to_ocaml ctx = function
  | Lit (IntLit n) -> string_of_int n
  | Lit (FloatLit f) -> string_of_float f
  | Lit (StringLit s) -> escape_ocaml_string s
  | Lit (BoolLit b) -> string_of_bool b
  | Lit NilLit -> "()"
  | Var x ->
      if List.mem x ctx.mutable_vars then Printf.sprintf "!%s" x else x
  | BinOp (op, l, r) ->
      Printf.sprintf "(%s %s %s)" (expr_to_ocaml ctx l) (string_of_binop op) (expr_to_ocaml ctx r)
  | UnOp (Not, e) -> Printf.sprintf "(not %s)" (expr_to_ocaml ctx e)
  | UnOp (Neg, e) -> Printf.sprintf "(-. %s)" (expr_to_ocaml ctx e)
  | UnOp (Inc, e) -> (match e with Var x -> Printf.sprintf "%s := !%s + 1" x x | _ -> failwith "inc")
  | UnOp (Dec, e) -> (match e with Var x -> Printf.sprintf "%s := !%s - 1" x x | _ -> failwith "dec")
  | Call (name, args) ->
      let args_str = String.concat " " (List.map (expr_to_ocaml ctx) args) in
      Printf.sprintf "%s %s" name args_str
  | MethodCall (obj, method_name, args) ->
      let obj_str = expr_to_ocaml ctx obj in
      if obj_str = "fmt" && method_name = "Println" then
        "()"  (* se maneja aparte en ExprStmt *)
      else
        let args_str = String.concat " " (List.map (expr_to_ocaml ctx) args) in
        Printf.sprintf "%s.%s %s" obj_str method_name args_str
  | Index (arr, idx) -> Printf.sprintf "%s.(%s)" (expr_to_ocaml ctx arr) (expr_to_ocaml ctx idx)
  | Selector (e, field) -> Printf.sprintf "%s.%s" (expr_to_ocaml ctx e) field

let rec infer_expr_type ctx = function
  | Lit (IntLit _) -> TInt
  | Lit (FloatLit _) -> TFloat64
  | Lit (StringLit _) -> TString
  | Lit (BoolLit _) -> TBool
  | Lit NilLit -> TNil
  | Var x -> lookup_var_type ctx x
  | BinOp (op, l, r) ->
      begin match op with
      | Eq | Neq | Lt | Gt | Leq | Geq | And | Or -> TBool
      | Add | Sub | Mul | Div | Mod ->
          let lt = infer_expr_type ctx l in
          let rt = infer_expr_type ctx r in
          if lt = TFloat64 || rt = TFloat64 then TFloat64
          else if lt = TString && rt = TString then TString
          else if lt = TInt && rt = TInt then TInt
          else TAny
      end
  | UnOp (Not, _) -> TBool
  | UnOp (Neg, e) ->
      begin match infer_expr_type ctx e with
      | TInt -> TInt
      | TFloat64 -> TFloat64
      | _ -> TAny
      end
  | UnOp ((Inc|Dec), _) -> TInt
  | Call (name, _) -> lookup_func_ret_type ctx name
  | MethodCall (obj, method_name, _) ->
      if expr_to_ocaml ctx obj = "fmt" && List.mem method_name ["Println"; "Printf"; "Print"] then TVoid
      else TAny
  | Index (arr, _) ->
      begin match infer_expr_type ctx arr with
      | TSlice t -> t
      | _ -> TAny
      end
  | Selector (_, _) -> TAny

let string_of_expr_string ctx expr =
  let expr_str = expr_to_ocaml ctx expr in
  match infer_expr_type ctx expr with
  | TString -> expr_str
  | TBool -> Printf.sprintf "string_of_bool (%s)" expr_str
  | TFloat64 -> Printf.sprintf "string_of_float (%s)" expr_str
  | TInt -> Printf.sprintf "string_of_int (%s)" expr_str
  | _ -> expr_str

let expr_to_print_string ctx = function
  | Lit (IntLit n) -> string_of_int n
  | Lit (FloatLit f) -> string_of_float f
  | Lit (StringLit s) -> escape_ocaml_string s
  | Lit (BoolLit b) -> string_of_bool b
  | Lit NilLit -> "\"nil\""
  | Var x ->
      let base = if List.mem x ctx.mutable_vars then Printf.sprintf "!%s" x else x in
      begin match lookup_var_type ctx x with
      | TString -> base
      | TBool -> Printf.sprintf "string_of_bool %s" base
      | TFloat64 -> Printf.sprintf "string_of_float %s" base
      | TInt -> Printf.sprintf "string_of_int %s" base
      | _ -> base
      end
  | expr ->
      string_of_expr_string ctx expr

(* ---------------------------------------------------------------------- *)
(*  Expresiones → OCaml                                                   *)
(* ---------------------------------------------------------------------- *)

let rec stmt_to_ocaml ctx = function
  | Assign (lhs, rhs) ->
      let lhs_str = List.map (expr_to_ocaml ctx) lhs in
      let rhs_str = List.map (expr_to_ocaml ctx) rhs in
      (match lhs_str, rhs_str with [l], [r] -> Printf.sprintf "%s = %s" l r | _ -> failwith "multi-assign")
  | ShortDecl (name, expr) ->
      let init = expr_to_ocaml ctx expr in
      if List.mem name ctx.mutable_vars then
        Printf.sprintf "let %s = ref %s in" name init
      else
        Printf.sprintf "let %s = %s in" name init
  | If (cond, then_b, else_opt) ->
      let cond_str = expr_to_ocaml ctx cond in
      let then_block = block_to_ocaml (with_indent ctx) then_b in
      let then_body = if then_block = "\n  ()" then "    ()" else String.sub then_block 1 (String.length then_block - 1) in
      let then_part = Printf.sprintf "\n%s" then_body in
      let else_part = match else_opt with
        | None -> ""
        | Some b ->
            let else_block = block_to_ocaml (with_indent ctx) b in
            let else_body = if else_block = "\n  ()" then "    ()" else String.sub else_block 1 (String.length else_block - 1) in
            Printf.sprintf " else\n%s" else_body
      in
      Printf.sprintf "if %s then%s%s" cond_str then_part else_part
  | ForCond (cond, body) ->
      let cond_str = expr_to_ocaml ctx cond in
      let body_block = block_to_ocaml (with_indent ctx) body in
      let body_str = if body_block = "\n  ()" then "\n    ()\n" ^ indent ctx.indent
                     else "\n" ^ String.sub body_block 1 (String.length body_block - 1) ^ "\n" ^ indent ctx.indent
      in
      Printf.sprintf "while %s do%s done" cond_str body_str
  | ForRange (k, v, coll, body) ->
      let coll_str = expr_to_ocaml ctx coll in
      let body_block = block_to_ocaml (with_indent ctx) body in
      let body_str = String.sub body_block 1 (String.length body_block - 1) in
      Printf.sprintf "for i = 0 to Array.length %s - 1 do\n%s  let %s = i in\n%s  let %s = %s.(i) in%s\ndone"
        coll_str (indent (ctx.indent + 1)) k (indent (ctx.indent + 1)) v coll_str body_str
  | Return exprs ->
      let exprs_str = List.map (expr_to_ocaml ctx) exprs in
      (match exprs_str with [] -> "()" | [e] -> e | _ -> "(" ^ String.concat ", " exprs_str ^ ")")
  | ExprStmt e ->
      begin match e with
      | MethodCall (obj, method_name, args) when expr_to_ocaml ctx obj = "fmt" && method_name = "Println" ->
          let strings = List.map (expr_to_print_string ctx) args in
          let concat = String.concat " ^ \" \" ^ " strings in
          Printf.sprintf "print_endline (%s)" concat
      | _ ->
          expr_to_ocaml ctx e
      end
  | Defer e -> Printf.sprintf "(* defer %s not implemented *)" (expr_to_ocaml ctx e)
  | Go e -> Printf.sprintf "(* go %s not implemented *)" (expr_to_ocaml ctx e)

(* ---------------------------------------------------------------------- *)
(*  Bloques (con propagación de mutabilidad, sin ; después de in)        *)
(* ---------------------------------------------------------------------- *)

and block_to_ocaml ctx = function
  | [] -> "\n  ()"
  | [stmt] ->
      (* Para una sola sentencia, no añadir ; *)
      Printf.sprintf "\n%s" (stmt_to_ocaml ctx stmt)
  | stmt :: rest ->
      match stmt with
      | ShortDecl (name, expr) ->
          let init = expr_to_ocaml ctx expr in
          let is_mutable = List.mem name ctx.mutable_vars in
          let decl_type = infer_expr_type ctx expr in
          let new_ctx =
            let typed_ctx = { ctx with var_types = (name, decl_type) :: ctx.var_types } in
            if is_mutable then { typed_ctx with mutable_vars = name :: typed_ctx.mutable_vars }
            else typed_ctx
          in
          let decl = if is_mutable then Printf.sprintf "let %s = ref %s in" name init
                     else Printf.sprintf "let %s = %s in" name init in
          (* Envolver el resto en paréntesis para que el let...in tenga alcance *)
          Printf.sprintf "\n%s (%s)" decl (block_to_ocaml new_ctx rest)
      | _ ->
          let first = stmt_to_ocaml ctx stmt in
          let inner = block_to_ocaml ctx rest in
          (* Separar con ;, pero sin ; después de in porque ya está manejado arriba *)
          Printf.sprintf "\n%s;\n%s" first (String.sub inner 1 (String.length inner - 1))

(* ---------------------------------------------------------------------- *)
(*  Declaraciones top-level                                               *)
(* ---------------------------------------------------------------------- *)

let decl_to_ocaml ctx = function
  | FuncDecl fd ->
      let param_str = List.map (fun (name, typ) -> Printf.sprintf "(%s : %s)" name (ocaml_type_of_typ typ)) fd.params |> String.concat " " in
      let ret_str = match fd.ret with [] -> "unit" | [t] -> ocaml_type_of_typ t | _ -> failwith "multi-return"
      in
      let mutable_vars = collect_mutable_in_func fd in
      let params_types = List.map (fun (name, typ) -> (name, typ)) fd.params in
      let ctx' = { ctx with mutable_vars = mutable_vars; var_types = params_types @ ctx.var_types } in
      let body_str = block_to_ocaml (with_indent ctx') fd.body in
      if fd.params = [] then
        Printf.sprintf "let %s () : %s =%s" fd.name ret_str body_str
      else
        Printf.sprintf "let %s %s : %s =%s" fd.name param_str ret_str body_str
  | VarDecl (name, typ_opt, expr_opt) ->
      let typ_str = match typ_opt with Some t -> " : " ^ ocaml_type_of_typ t | None -> "" in
      let init = match expr_opt with Some e -> expr_to_ocaml ctx e | None -> "()" in
      Printf.sprintf "let %s%s = %s" name typ_str init

(* ---------------------------------------------------------------------- *)
(*  Programa completo (sin open Printf)                                   *)
(* ---------------------------------------------------------------------- *)

let program_to_ocaml (prog: program) : string =
  let func_ret_types =
    List.fold_left (fun acc -> function
      | FuncDecl { name; ret = [t]; _ } -> (name, t) :: acc
      | _ -> acc
    ) [] prog.decls
  in
  let ctx = { empty_ctx with func_ret_types } in
  let decls_str = List.map (decl_to_ocaml ctx) prog.decls |> String.concat "\n\n" in
  let final_main =
    if List.exists (function FuncDecl { name = "main"; params = _; ret = _; body = _ } -> true | _ -> false) prog.decls then
      "\n\nlet () = main ()"
    else ""
  in
  decls_str ^ final_main

let generate (prog: program) : string = program_to_ocaml prog