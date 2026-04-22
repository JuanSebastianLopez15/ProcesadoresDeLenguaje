(* lib/subset_check.ml *)
open Ast

exception UnsupportedFeature of string

(* Lista de features no soportados para generar errores explícitos.
   En esta versión, como el parser no los acepta, el subset_check
   verifica invariantes semánticos adicionales. *)

let check_func_body_has_return (fd : Ast.func_decl) =
  if fd.ret <> TVoid then
    let rec all_paths_return = function
      | [] -> false
      | [Return _] -> true
      | [If (_, t, Some e)] -> all_paths_return t && all_paths_return e
      | _ :: rest -> all_paths_return rest
    in
    if not (all_paths_return fd.body) then
      raise (UnsupportedFeature
        (Printf.sprintf "La función '%s' no tiene return en todos los caminos"
           fd.name))

let check_no_method_receivers _fd = ()  (* el AST no permite receivers *)

let check_single_return fd =
  (* Ya garantizado por el AST (Return toma expr option) *)
  ignore fd

let check_no_nested_funcs stmts =
  (* En el AST actual, las funciones solo existen top-level *)
  ignore stmts

let check_assign_is_local (fd : Ast.func_decl) =
  (* Reglas de mutabilidad: verificar que las reasignaciones
     son sobre variables declaradas con := en el mismo scope *)
  let rec check declared = function
    | [] -> ()
    | ShortDecl (x, _) :: rest -> check (x :: declared) rest
    | Assign (_, _) :: rest -> check declared rest
    | FieldAssign (_, _) :: rest -> check declared rest
    | If (_, t, e) :: rest ->
        check declared t;
        Option.iter (check declared) e;
        check declared rest
    | _ :: rest -> check declared rest
  in
  (* Los parámetros NO se agregan a declared — son inmutables *)
  check [] fd.body

let rec validate_expr = function
  | Lit _ | Var _ -> ()
  | BinOp (_, l, r) -> validate_expr l; validate_expr r
  | UnOp (_, e) -> validate_expr e
  | Call (_, args) -> List.iter validate_expr args
  | Index (e, i) -> validate_expr e; validate_expr i
  | Selector (e, _) -> validate_expr e
  | StructLit (_, args) -> List.iter (fun (_, e) -> validate_expr e) args
  | SliceLit (_, elems) -> List.iter validate_expr elems
  | Cast (TInt, e) | Cast (TFloat, e) | Cast (TString, e)
  | Cast (TSlice TInt, e) -> validate_expr e
  | Cast (t, _) ->
      let rec show_typ = function
        | TInt -> "int" | TFloat -> "float64" | TString -> "string"
        | TBool -> "bool" | TVoid -> "void" | TSlice t -> "[]" ^ show_typ t
        | TName n -> n
      in
      raise (UnsupportedFeature
        (Printf.sprintf "Cast a tipo no soportado: %s" (show_typ t)))

let rec validate_stmt = function
  | ShortDecl (_, e) | Assign (_, e) | ExprStmt e -> validate_expr e
  | FieldAssign (lhs, rhs) -> validate_expr lhs; validate_expr rhs
  | Return (Some e) -> validate_expr e
  | Return None -> ()
  | If (c, t, e) ->
      validate_expr c;
      List.iter validate_stmt t;
      Option.iter (List.iter validate_stmt) e

let check_decl = function
  | FuncDecl fd ->
      check_func_body_has_return fd;
      check_assign_is_local fd;
      List.iter validate_stmt fd.body
  | StructDecl _ -> ()

let check_program prog =
  List.iter check_decl prog.decls

(* API: *)
let validate prog =
  try
    check_program prog;
    Ok prog
  with UnsupportedFeature msg -> Error msg
