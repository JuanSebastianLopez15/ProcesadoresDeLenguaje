open Lib.Ast

let indent n = String.make (2 * n) ' '

type ctx = {
  indent: int;
  mutable_vars: string list;
  var_types: (string * typ) list;
  func_ret_types: (string * typ) list;
  type_aliases: (string * typ) list;
  current_ret_types: typ list;
  return_exn_name: string option;
  interface_impls: (string * string list) list; (* Interface name -> Implementation names *)
}

type literal_kind = [`int | `float | `unknown]

let empty_ctx = {
  indent = 0;
  mutable_vars = [];
  var_types = [];
  func_ret_types = [];
  type_aliases = [];
  current_ret_types = [];
  return_exn_name = None;
  interface_impls = [];
}
let with_indent ctx = { ctx with indent = ctx.indent + 1 }

let ocaml_keywords =
  [ "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for";
    "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit";
    "initializer"; "lazy"; "let"; "match"; "method"; "module"; "mutable";
    "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec";
    "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
    "virtual"; "when"; "while"; "with" ]

let sanitize_ident raw =
  let n = String.length raw in
  if n = 0 then "v"
  else
    let buf = Buffer.create (n + 4) in
    let add i c =
      let ok =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ||
        (i > 0 && c >= '0' && c <= '9')
      in
      if ok then Buffer.add_char buf c else Buffer.add_char buf '_'
    in
    String.iteri add raw;
    let s0 = Buffer.contents buf in
    let s1 =
      if s0 = "" then "v"
      else
        let c0 = s0.[0] in
        if (c0 >= 'a' && c0 <= 'z') || c0 = '_' then s0
        else "v_" ^ String.uncapitalize_ascii s0
    in
    if List.mem s1 ocaml_keywords then s1 ^ "_" else s1

let value_ident raw = sanitize_ident (String.lowercase_ascii raw)
let field_ident raw = sanitize_ident (String.lowercase_ascii raw)
let type_ident raw = sanitize_ident (String.lowercase_ascii raw)
let constructor_ident raw = String.capitalize_ascii (sanitize_ident raw)

let lookup_var_type ctx x =
  match List.assoc_opt x ctx.var_types with
  | Some t -> t
  | None -> TAny

let lookup_func_ret_type ctx name =
  match List.assoc_opt name ctx.func_ret_types with
  | Some t -> t
  | None -> TAny

let rec resolve_typ ctx = function
  | TName n ->
      (match List.assoc_opt n ctx.type_aliases with
       | Some t -> resolve_typ ctx t
       | None -> TName n)
  | t -> t

let go_numeric_named_types =
  [ "int8"; "int16"; "int32"; "int64";
    "uint"; "uint8"; "uint16"; "uint32"; "uint64"; "uintptr";
    "byte"; "rune"; "float32"; "float64" ]

let is_float_named_type = function
  | "float32" | "float64" -> true
  | _ -> false

let is_known_type_alias ctx name =
  List.assoc_opt name ctx.type_aliases <> None

let is_interface_type_name ctx name =
  match List.assoc_opt name ctx.type_aliases with
  | Some (TInterface _) -> true
  | _ -> false

let kind_of_typ ctx t =
  match resolve_typ ctx t with
  | TFloat64 -> `float
  | TInt -> `int
  | _ -> `unknown

let rec field_typ_of_expr ctx expr field =
  let lookup_named_field fields target =
    List.find_opt (fun (fname, _) -> field_ident fname = field_ident target || fname = target)
      fields
    |> Option.map snd
  in
  let lookup_field fields =
    List.find_opt (fun (fname, _) -> field_ident fname = field_ident field || fname = field)
      fields
    |> Option.map snd
  in
  match expr with
  | Var x ->
      (match resolve_typ ctx (lookup_var_type ctx x) with
       | TStruct fields ->
           (match lookup_field fields with
            | Some t -> Some t
            | None ->
                (match lookup_named_field fields "_embedded" with
                 | Some (TName emb) ->
                     (match List.assoc_opt emb ctx.type_aliases with
                      | Some (TStruct emb_fields) -> lookup_named_field emb_fields field
                      | _ -> None)
                 | Some (TStruct emb_fields) -> lookup_named_field emb_fields field
                 | _ -> None))
       | _ -> None)
  | Selector (inner, inner_field) ->
      (match field_typ_of_expr ctx inner inner_field with
       | Some t ->
           (match resolve_typ ctx t with
            | TStruct fields -> lookup_field fields
            | _ -> None)
       | None -> None)
  | _ -> None

let inferred_receiver_struct_type ctx expr =
  match expr with
  | Var x ->
      (match resolve_typ ctx (lookup_var_type ctx x) with
       | TStruct fields -> Some fields
       | _ -> None)
  | _ -> None

let rec expr_kind ctx = function
  | Lit (IntLit _) | Lit (RuneLit _) -> `int
  | Lit (FloatLit _) -> `float
  | Var x ->
      kind_of_typ ctx (lookup_var_type ctx x)
  | Selector (base, field) ->
      (match field_typ_of_expr ctx base field with
       | Some t -> kind_of_typ ctx t
       | None -> `unknown)
  | Cast (t, _) ->
      kind_of_typ ctx t
  | Call (Var ("float64" | "float32"), _) -> `float
  | Call (Var ("int" | "int64"), _) -> `int
  | Call (Var name, _) ->
      kind_of_typ ctx (lookup_func_ret_type ctx (value_ident name))
  | Call (Selector (_, field), _) ->
      kind_of_typ ctx (lookup_func_ret_type ctx (value_ident field))
  | BinOp ((Add | Sub | Mul | Div), l, r) ->
      (match expr_kind ctx l, expr_kind ctx r with
       | `float, _ | _, `float -> `float
       | `int, `int -> `int
       | _ -> `unknown)
  | UnOp (_, e) -> expr_kind ctx e
  | FuncLit fd -> if fd.ret = [TFloat64] then `float else if fd.ret = [TInt] then `int else `unknown
  | _ -> `unknown

let default_expr_of_typ ctx t =
  match resolve_typ ctx t with
  | TInt -> "0"
  | TFloat64 -> "0.0"
  | TString -> "\"\""
  | TBool -> "false"
  | TSlice _ -> "[||]"
  | TMap _ -> "Hashtbl.create 16"
  | _ -> "Obj.magic ()"

let rec collect_mutable_in_expr = function
  | Lit _ | Var _ -> []
  | BinOp (_, l, r) -> collect_mutable_in_expr l @ collect_mutable_in_expr r
  | UnOp (_, e) -> collect_mutable_in_expr e
  | Call (e, args) -> collect_mutable_in_expr e @ List.concat_map collect_mutable_in_expr args
  | MethodCall (obj, _, args) ->
      collect_mutable_in_expr obj @ List.concat_map collect_mutable_in_expr args
  | Index (arr, idx) -> collect_mutable_in_expr arr @ collect_mutable_in_expr idx
  | Slice (arr, low, high, max) ->
      collect_mutable_in_expr arr @
      (match low with Some e -> collect_mutable_in_expr e | None -> []) @
      (match high with Some e -> collect_mutable_in_expr e | None -> []) @
      (match max with Some e -> collect_mutable_in_expr e | None -> [])
  | Selector (e, _) -> collect_mutable_in_expr e
  | StructLit (_, args) -> List.concat_map (fun (_, e) -> collect_mutable_in_expr e) args
  | SliceLit (_, args) -> List.concat_map (fun (_, e) -> collect_mutable_in_expr e) args
  | Spread e -> collect_mutable_in_expr e
  | Cast (_, e) -> collect_mutable_in_expr e
  | KeyedExpr (_, e) -> collect_mutable_in_expr e
  | FuncLit fd -> List.concat_map collect_mutable_in_stmt fd.body

and collect_mutable_in_stmt = function
  | Assign (lhs, rhs) ->
      let lhs_vars = List.concat_map collect_mutable_in_expr lhs in
      let rhs_vars = List.concat_map collect_mutable_in_expr rhs in
      let mutable_names = List.filter_map (function Var x -> Some x | _ -> None) lhs in
      mutable_names @ lhs_vars @ rhs_vars
  | MultiAssign (lhs, rhs) ->
      let lhs_vars = List.concat_map collect_mutable_in_expr lhs in
      let rhs_vars = List.concat_map collect_mutable_in_expr rhs in
      let mutable_names = List.filter_map (function Var x -> Some x | _ -> None) lhs in
      mutable_names @ lhs_vars @ rhs_vars
  | ShortDecl (_, exprs) -> List.concat_map collect_mutable_in_expr exprs
  | VarDeclStmt (_, _, expr_opt) -> (match expr_opt with Some e -> collect_mutable_in_expr e | None -> [])
  | If (cond, then_b, else_opt) ->
      let cond_vars = collect_mutable_in_expr cond in
      let then_vars = List.concat_map collect_mutable_in_stmt then_b in
      let else_vars = match else_opt with Some b -> List.concat_map collect_mutable_in_stmt b | None -> [] in
      cond_vars @ then_vars @ else_vars
  | IfInit (init_stmt, cond, then_b, else_opt) ->
      collect_mutable_in_stmt init_stmt
      @ collect_mutable_in_expr cond
      @ List.concat_map collect_mutable_in_stmt then_b
      @ (match else_opt with Some b -> List.concat_map collect_mutable_in_stmt b | None -> [])
  | TypeSwitch (_, target, cases, default_opt) ->
      let target_vars = collect_mutable_in_expr target in
      let case_vars =
        List.concat_map
          (fun (_, body) -> List.concat_map collect_mutable_in_stmt body)
          cases
      in
      let default_vars =
        match default_opt with
        | None -> []
        | Some body -> List.concat_map collect_mutable_in_stmt body
      in
      target_vars @ case_vars @ default_vars
  | ForCond (cond, body) ->
      collect_mutable_in_expr cond @ List.concat_map collect_mutable_in_stmt body
  | ForClassic (init_opt, cond_opt, post_opt, body) ->
      let init_vars = match init_opt with Some s -> collect_mutable_in_stmt s | None -> [] in
      let cond_vars = match cond_opt with Some e -> collect_mutable_in_expr e | None -> [] in
      let post_vars = match post_opt with Some s -> collect_mutable_in_stmt s | None -> [] in
      init_vars @ cond_vars @ post_vars @ List.concat_map collect_mutable_in_stmt body
  | ForRange (_, _, coll, body) ->
      collect_mutable_in_expr coll @ List.concat_map collect_mutable_in_stmt body
  | Return exprs -> List.concat_map collect_mutable_in_expr exprs
  | ExprStmt e -> collect_mutable_in_expr e
  | Defer e | Go e -> collect_mutable_in_expr e

let collect_mutable_in_func (fd: func_decl) : string list =
  let rec targets_in_stmt = function
    | Assign (lhs, _) | MultiAssign (lhs, _) ->
        List.filter_map (function Var x -> Some x | _ -> None) lhs
    | ShortDecl (names, _) -> names
    | VarDeclStmt (name, _, _) -> [name]
    | If (_, then_b, else_opt) ->
        List.concat_map targets_in_stmt then_b
        @ (match else_opt with Some b -> List.concat_map targets_in_stmt b | None -> [])
    | IfInit (init_stmt, _, then_b, else_opt) ->
        targets_in_stmt init_stmt
        @ List.concat_map targets_in_stmt then_b
        @ (match else_opt with Some b -> List.concat_map targets_in_stmt b | None -> [])
    | TypeSwitch (_, _, cases, default_opt) ->
        List.concat_map (fun (_, body) -> List.concat_map targets_in_stmt body) cases
        @ (match default_opt with Some body -> List.concat_map targets_in_stmt body | None -> [])
    | ForCond (_, body) -> List.concat_map targets_in_stmt body
    | ForClassic (init_opt, _, post_opt, body) ->
        (match init_opt with Some s -> targets_in_stmt s | None -> [])
        @ (match post_opt with Some s -> targets_in_stmt s | None -> [])
        @ List.concat_map targets_in_stmt body
    | ForRange (_, _, _, body) -> List.concat_map targets_in_stmt body
    | ExprStmt (UnOp ((Inc|Dec), e)) -> (match e with Var x -> [x] | _ -> [])
    | Return _ | ExprStmt _ | Defer _ | Go _ -> []
  in
  List.concat_map targets_in_stmt fd.body

let rec ocaml_type_of_typ ?(normalize_alias_primitives = false) ctx = function
  | TInt -> "int"
  | TFloat64 -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TNil | TVoid -> "unit"
  | TAny -> "Obj.t"
  | TSlice t ->
      Printf.sprintf "%s array"
        (ocaml_type_of_typ ~normalize_alias_primitives ctx t)
  | TMap (k, v) ->
      Printf.sprintf "(%s, %s) Hashtbl.t"
        (ocaml_type_of_typ ~normalize_alias_primitives ctx k)
        (ocaml_type_of_typ ~normalize_alias_primitives ctx v)
  | TName n when List.mem n go_numeric_named_types ->
      if is_float_named_type n then "float" else "int"
  | TName n when is_known_type_alias ctx n ->
      (match List.assoc_opt n ctx.interface_impls with
       | Some _impls -> type_ident n
       | None ->
          if normalize_alias_primitives then
            (match resolve_typ ctx (TName n) with
             | TInt | TFloat64 | TString | TBool as t ->
                 ocaml_type_of_typ ~normalize_alias_primitives ctx t
             | _ -> type_ident n)
          else
            type_ident n)
  | TName n -> Printf.sprintf "'%s" (type_ident n)
  | TStruct _ -> "Obj.t"
  | TInterface _ -> "Obj.t"
  | TFunc (params, rets) ->
      let ps =
        if params = [] then "unit"
        else String.concat " -> " (List.map (ocaml_type_of_typ ~normalize_alias_primitives ctx) params)
      in
      let rs =
        match rets with
        | [] -> "unit"
        | [r] -> ocaml_type_of_typ ~normalize_alias_primitives ctx r
        | _ ->
            "("
            ^ String.concat " * "
                (List.map
                   (ocaml_type_of_typ ~normalize_alias_primitives ctx)
                   rets)
            ^ ")"
      in
      ps ^ " -> " ^ rs

let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "mod"
  | Eq -> "=" | Neq -> "<>" | Lt -> "<" | Gt -> ">"
  | Leq -> "<=" | Geq -> ">=" | And -> "&&" | Or -> "||"
  | BAnd -> "land" | BOr -> "lor" | BXor -> "lxor"
  | Shl -> "lsl" | Shr -> "lsr" | AndNot -> "(* &^ not supported *) land"

let string_of_binop_float = function
  | Add -> "+." | Sub -> "-." | Mul -> "*." | Div -> "/."
  | op -> string_of_binop op

let escape_ocaml_string s =
  let buf = Buffer.create (String.length s + 10) in
  Buffer.add_char buf '"';
  String.iter (function
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c   
  ) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let likely_bool_expr ctx = function
  | Lit (BoolLit _) -> true
  | BinOp ((Eq | Neq | Lt | Gt | Leq | Geq | And | Or), _, _) -> true
  | UnOp (Not, _) -> true
  | Var x -> lookup_var_type ctx x = TBool
  | _ -> false

let rec infer_expr_type ctx expr =
  match expr with
  | Lit (IntLit _) -> TInt
  | Lit (RuneLit _) -> TInt
  | Lit (FloatLit _) -> TFloat64
  | Lit (StringLit _) -> TString
  | Lit (BoolLit _) -> TBool
  | Lit NilLit -> TNil
  | Var x -> lookup_var_type ctx x
  | BinOp (op, l, r) ->
      begin match op with
      | Eq | Neq | Lt | Gt | Leq | Geq | And | Or -> TBool
      | BAnd | BOr | BXor | Shl | Shr | AndNot -> TInt
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
  | UnOp (AddrOf, e) -> TSlice (infer_expr_type ctx e)
  | UnOp (Deref, e) -> (match infer_expr_type ctx e with TSlice t -> t | _ -> TAny)
  | Call (Selector (obj, method_name), args) ->
      infer_expr_type ctx (MethodCall (obj, method_name, args))
  | Call (Var name, _) ->
      (match lookup_func_ret_type ctx (value_ident name) with
       | TAny ->
           (match resolve_typ ctx (lookup_var_type ctx name) with
            | TFunc (_, []) -> TVoid
            | TFunc (_, [t]) -> t
            | TFunc (_, ts) -> TFunc ([], ts)
            | _ -> TAny)
       | t -> t)
  | Call (_, _) -> TAny
  | MethodCall (obj, method_name, _) ->
      if expr_to_ocaml ctx obj = "fmt" && List.mem method_name ["Println"; "Printf"; "Print"] then TVoid
      else
        (match field_typ_of_expr ctx obj method_name with
         | Some t -> t
         | None -> 
             let mname = value_ident method_name in
             let res = match lookup_func_ret_type ctx mname with
             | TAny -> 
                 (match resolve_typ ctx (lookup_var_type ctx mname) with
                  | TFunc (_, [t]) -> t
                  | _ -> TAny)
             | t -> t in
             res)
  | Index (arr, _) ->
      begin match infer_expr_type ctx arr with
      | TSlice t -> t
      | _ -> TAny
      end
  | Slice (arr, _, _, _) -> infer_expr_type ctx arr
  | Selector (e, field) ->
      (match resolve_typ ctx (infer_expr_type ctx e) with
       | TStruct fields ->
           (match List.find_opt (fun (fname, _) -> field_ident fname = field_ident field || fname = field) fields with
            | Some (_, t) -> t
            | None -> 
                (match List.find_opt (fun (fname, _) -> fname = "_embedded" || field_ident fname = "_embedded") fields with
                 | Some (_, TName emb) ->
                     (match List.assoc_opt emb ctx.type_aliases with
                      | Some (TStruct emb_fields) ->
                          (match List.find_opt (fun (fname, _) -> field_ident fname = field_ident field || fname = field) emb_fields with
                           | Some (_, t) -> t | None -> TAny)
                      | _ -> TAny)
                 | _ -> TAny))
       | _ -> TAny)
  | StructLit (name, _) -> TName name
  | Spread e -> infer_expr_type ctx e
  | SliceLit (t, _) -> TSlice t
  | Cast (t, _) -> t
  | KeyedExpr (_, _) -> TAny
  | FuncLit fd -> TFunc (List.map snd fd.params, fd.ret)

and expr_looks_float ctx = function
  | Lit (FloatLit _) -> true
  | Lit (IntLit _ | RuneLit _ | StringLit _ | BoolLit _ | NilLit) -> false
  | Var x -> (match resolve_typ ctx (lookup_var_type ctx x) with TFloat64 -> true | _ -> false)
  | Selector (base, field) -> (match resolve_typ ctx (infer_expr_type ctx (Selector (base, field))) with TFloat64 -> true | _ -> false)
  | Cast (t, _) -> (match resolve_typ ctx t with TFloat64 -> true | _ -> false)
  | Call (Var ("float64" | "float32"), _) -> true
  | Call (Var name, _) -> kind_of_typ ctx (lookup_func_ret_type ctx (value_ident name)) = `float
  | MethodCall (obj, method_name, _) -> kind_of_typ ctx (infer_expr_type ctx (MethodCall (obj, method_name, []))) = `float
  | BinOp ((Add | Sub | Mul | Div), l, r) -> expr_looks_float ctx l || expr_looks_float ctx r
  | UnOp (Neg, e) -> expr_looks_float ctx e
  | UnOp (_, _) -> false
  | _ -> false

and expr_looks_int ctx = function
  | Lit (IntLit _ | RuneLit _) -> true
  | Lit (FloatLit _ | StringLit _ | BoolLit _ | NilLit) -> false
  | Var x -> (match resolve_typ ctx (lookup_var_type ctx x) with TInt -> true | _ -> false)
  | Selector (base, field) -> (match resolve_typ ctx (infer_expr_type ctx (Selector (base, field))) with TInt -> true | _ -> false)
  | Cast (t, _) -> (match resolve_typ ctx t with TInt -> true | _ -> false)
  | Call (Var ("int" | "int64"), _) -> true
  | Call (Var name, _) -> kind_of_typ ctx (lookup_func_ret_type ctx (value_ident name)) = `int
  | MethodCall (obj, method_name, _) -> kind_of_typ ctx (infer_expr_type ctx (MethodCall (obj, method_name, []))) = `int
  | UnOp ((Neg|Inc|Dec), e) -> expr_looks_int ctx e
  | BinOp ((Add | Sub | Mul | Div | Mod | BAnd | BOr | BXor | Shl | Shr), l, r) ->
      expr_looks_int ctx l && expr_looks_int ctx r && not (expr_looks_float ctx l || expr_looks_float ctx r)
  | _ -> false

and coerce_expr_to_expected_type ctx expected_t expr =
  let rendered = expr_to_ocaml ctx expr in
  let expected_t_resolved = resolve_typ ctx expected_t in
  let got_t = infer_expr_type ctx expr in
  let got_t_resolved = resolve_typ ctx got_t in
  match expected_t_resolved, got_t_resolved with
  | TFloat64, TInt -> Printf.sprintf "float_of_int (%s)" rendered
  | TInt, TFloat64 -> Printf.sprintf "int_of_float (%s)" rendered
  | TName iface, TName struct_name when is_interface_type_name ctx iface ->
      (match List.assoc_opt iface ctx.interface_impls with
       | Some impls when List.mem struct_name impls ->
           Printf.sprintf "(%s %s)" (constructor_ident struct_name) rendered
       | _ -> rendered)
  | _ -> rendered

and bind_typeswitch_ctx ctx bind labels =
  match labels with
  | [label] -> { ctx with var_types = (bind, TName label) :: ctx.var_types }
  | _ -> { ctx with var_types = (bind, TAny) :: ctx.var_types }

and expr_to_ocaml ctx = function
  | Lit (IntLit n) -> Int64.to_string n
  | Lit (RuneLit n) -> Printf.sprintf "%d" n
  | Lit (FloatLit f) -> string_of_float f
  | Lit (StringLit s) -> escape_ocaml_string s
  | Lit (BoolLit b) -> string_of_bool b
  | Lit NilLit -> "()"
  | Var x ->
      let sx = value_ident x in
      if List.mem x ctx.mutable_vars then Printf.sprintf "!%s" sx else sx
  | BinOp (op, l, r) ->
      let l_raw = expr_to_ocaml ctx l in
      let r_raw = expr_to_ocaml ctx r in
      let lt = infer_expr_type ctx l in
      let rt = infer_expr_type ctx r in
      let op_is_float =
        match op with
        | Add | Sub | Mul | Div ->
            lt = TFloat64 || rt = TFloat64 || expr_looks_float ctx l || expr_looks_float ctx r
        | _ -> false
      in
      let op_str =
        match op with
        | Add | Sub | Mul | Div when op_is_float -> string_of_binop_float op
        | _ -> string_of_binop op
      in
      let left_str =
        if op_is_float && (lt = TInt || expr_looks_int ctx l)
        then Printf.sprintf "float_of_int (%s)" l_raw
        else l_raw
      in
      let right_str =
        if op_is_float && (rt = TInt || expr_looks_int ctx r)
        then Printf.sprintf "float_of_int (%s)" r_raw
        else r_raw
      in
      Printf.sprintf "(%s %s %s)" left_str op_str right_str
  | UnOp (Not, e) ->
      if likely_bool_expr ctx e then Printf.sprintf "(not %s)" (expr_to_ocaml ctx e)
      else expr_to_ocaml ctx e
  | UnOp (Neg, e) -> 
      if expr_looks_float ctx e then Printf.sprintf "(-. %s)" (expr_to_ocaml ctx e)
      else Printf.sprintf "(- %s)" (expr_to_ocaml ctx e)
  | UnOp (Inc, e) -> 
      let e_str = expr_to_ocaml ctx e in
      if List.mem (match e with Var x -> x | _ -> "") ctx.mutable_vars then
        Printf.sprintf "%s := !%s + 1" e_str e_str
      else "()"
  | UnOp (Dec, e) -> 
      let e_str = expr_to_ocaml ctx e in
      if List.mem (match e with Var x -> x | _ -> "") ctx.mutable_vars then
        Printf.sprintf "%s := !%s - 1" e_str e_str
      else "()"
  | UnOp (AddrOf, e) -> Printf.sprintf "(ref %s)" (expr_to_ocaml ctx e)
  | UnOp (Deref, e) -> Printf.sprintf "!(%s)" (expr_to_ocaml ctx e)
  | FuncLit fd ->
      let param_str = 
        if fd.params = [] then "()" 
        else String.concat " " (List.map (fun (n, _) -> value_ident n) fd.params)
      in
      let body_str = block_to_ocaml (with_indent ctx) fd.body in
      Printf.sprintf "(fun %s -> %s)" param_str body_str
  | Call (Var "len", [arr]) -> Printf.sprintf "Array.length %s" (expr_to_ocaml ctx arr)
  | Call (Var "append", [arr; elem]) ->
      (match elem with
       | Spread e ->
           Printf.sprintf "Array.append %s %s"
             (expr_to_ocaml ctx arr)
             (expr_to_ocaml ctx e)
       | _ ->
           Printf.sprintf "Array.append %s [|%s|]"
             (expr_to_ocaml ctx arr)
             (expr_to_ocaml ctx elem))
  | Call (Var ("float64" | "float32" | "int" | "int64" | "string"), [arg]) -> expr_to_ocaml ctx arg
  | Call (Var "make", args) -> 
      (match args with
       | [Lit (NilLit)] | [] -> "Obj.magic ()"
       | [Var _] | [_] -> 
           (* This is a bit weak because we don't have the type arg here anymore 
              (it was ignored in parser). Let's use infer_expr_type if possible
              or just a heuristic. Actually, parser ignores the type. *)
           "[||]"
       | _ -> "[||]")
  | Call (Var "new", _) -> "ref (Obj.magic ())"
  | Call (Var "delete", _) -> "()"
  | Call (Var "copy", _) -> "0"
  | Call (Var "close", _) -> "()"
  | Call (Var "complex", _) -> "Obj.magic ()"
  | Call (Var "real", [arg]) -> expr_to_ocaml ctx arg
  | Call (Var "imag", [arg]) -> expr_to_ocaml ctx arg
  | Call (Var "recover", _) -> "Obj.magic ()"
  | Call (Var "panic", _) -> "()"
  | Call (Selector (Var "fmt", "Sprintf"), args)
  | Call (Selector (Var "fmt", "Errorf"), args) ->
      (match args with
       | [] -> "\"\""
       | fmt :: rest ->
           let fmt_str = expr_to_ocaml ctx fmt in
           let rest_str = String.concat " " (List.map (expr_to_ocaml ctx) rest) in
           if rest_str = "" then Printf.sprintf "Printf.sprintf %s" fmt_str
           else Printf.sprintf "Printf.sprintf %s %s" fmt_str rest_str)
  | Call (Selector (Var "errors", "New"), [msg]) -> expr_to_ocaml ctx msg
  | Call (Selector (Var "errors", "New"), _) -> "\"\""
  | Call (Selector (Var "strings", "Join"), [xs; sep]) ->
      Printf.sprintf "String.concat %s (Array.to_list %s)" (expr_to_ocaml ctx sep) (expr_to_ocaml ctx xs)
  | Call (Selector (Var "strings", "Fields"), [_]) -> "[||]"
  | Call (Selector (Var "sort", "Strings"), [_]) -> "()"
  | Call (Var name, args) ->
      let fname = value_ident name in
      let is_ctor_like = String.length name > 0 && Char.uppercase_ascii name.[0] = name.[0] in
      if is_ctor_like && List.length args = 1 then expr_to_ocaml ctx (List.hd args)
      else
        let args_str = String.concat " " (List.map (expr_to_ocaml ctx) args) in
        if args_str = "" then fname
        else Printf.sprintf "%s %s" fname args_str
  | Call (e, args) ->
      let e_str = expr_to_ocaml ctx e in
      let args_str = String.concat " " (List.map (expr_to_ocaml ctx) args) in
      if args_str = "" then Printf.sprintf "(%s)" e_str
      else Printf.sprintf "(%s) %s" e_str args_str
  | MethodCall (obj, method_name, args) ->
      let obj_str = expr_to_ocaml ctx obj in
      if obj_str = "fmt" && (method_name = "Println" || method_name = "Printf") then
        "()"
      else
        let mname = value_ident method_name in
        let call_with_receiver =
          let all_args = obj_str :: List.map (expr_to_ocaml ctx) args in
          let args_str = String.concat " " all_args in
          if args_str = "" then mname else Printf.sprintf "%s %s" mname args_str
        in
        (match inferred_receiver_struct_type ctx obj with
         | Some fields when List.exists (fun (fname, _) -> field_ident fname = field_ident method_name || fname = method_name) fields
             && args = [] ->
             Printf.sprintf "%s.%s" obj_str (field_ident method_name)
         | _ -> call_with_receiver)
  | Index (arr, idx) -> 
      (match resolve_typ ctx (infer_expr_type ctx arr) with
       | TMap _ -> Printf.sprintf "(Hashtbl.find %s %s)" (expr_to_ocaml ctx arr) (expr_to_ocaml ctx idx)
       | _ -> Printf.sprintf "(%s).(%s)" (expr_to_ocaml ctx arr) (expr_to_ocaml ctx idx))
  | Slice (arr, _, _, _) -> Printf.sprintf "Array.sub %s 0 (Array.length %s)" (expr_to_ocaml ctx arr) (expr_to_ocaml ctx arr)
  | Selector (Var ("fmt" | "errors" | "strings" | "sort"), field) -> value_ident field
  | Selector (e, field) ->
      let recv = expr_to_ocaml ctx e in
      let fname = value_ident field in
      let fieldname = field_ident field in
      (match field_typ_of_expr ctx e field with
       | Some _ -> Printf.sprintf "%s.%s" recv fieldname
       | None ->
           (match e with
            | Var x ->
                (match resolve_typ ctx (lookup_var_type ctx x) with
                 | TStruct fields ->
                     (match List.find_opt (fun (fn, _) -> fn = "_embedded" || field_ident fn = "_embedded") fields with
                      | Some (emb_name, _) ->
                          let emb_field = field_ident emb_name in
                          Printf.sprintf "%s (%s.%s)" fname recv emb_field
                      | None -> Printf.sprintf "%s %s" fname recv)
                 | _ -> Printf.sprintf "%s %s" fname recv)
            | _ -> Printf.sprintf "%s %s" fname recv))
  | SliceLit (_, args) ->
      Printf.sprintf "[|%s|]" (String.concat "; " (List.map (fun (_, e) -> expr_to_ocaml ctx e) args))
  | Spread e -> expr_to_ocaml ctx e
  | Cast (_, e) -> expr_to_ocaml ctx e
  | StructLit (name, args) ->
      if args = [] then 
        (match List.assoc_opt name ctx.type_aliases with
         | Some (TStruct []) -> constructor_ident name
         | _ -> "Obj.magic ()")
      else
        let all_keyed = List.for_all (fun (k_opt, _) -> k_opt <> None) args in
        if not all_keyed then "Obj.magic ()"
        else
          let fields =
            List.map
              (fun (k_opt, e) ->
                let v = expr_to_ocaml ctx e in
                match k_opt with
                | Some k -> Printf.sprintf "%s = %s" (field_ident k) v
                | None -> "")
              args
          in
          Printf.sprintf "{ %s }" (String.concat "; " fields)
  | KeyedExpr (k, v) -> Printf.sprintf "%s = %s" (field_ident k) (expr_to_ocaml ctx v)

and stmt_to_ocaml ctx = function
  | Assign (lhss, rhss) ->
      let render_assign l r =
        match l with
        | Index (arr, idx) ->
            (match resolve_typ ctx (infer_expr_type ctx arr) with
             | TMap _ -> Printf.sprintf "Hashtbl.replace %s %s %s" (expr_to_ocaml ctx arr) (expr_to_ocaml ctx idx) (expr_to_ocaml ctx r)
             | _ -> Printf.sprintf "%s = %s" (expr_to_ocaml ctx l) (expr_to_ocaml ctx r))
        | _ -> 
            let ls = expr_to_ocaml ctx l in
            let rs = expr_to_ocaml ctx r in
            if List.mem (match l with Var x -> x | _ -> "") ctx.mutable_vars then
              Printf.sprintf "%s := %s" ls rs
            else
              Printf.sprintf "%s = %s" ls rs
      in
      let lhs_str = List.map (expr_to_ocaml ctx) lhss in
      let rhs_str = List.map (expr_to_ocaml ctx) rhss in
      if List.length lhss = List.length rhss then
        List.map2 render_assign lhss rhss |> String.concat "; "
      else if List.length rhs_str = 1 then
        Printf.sprintf "let (%s) = %s in %s" (String.concat ", " lhs_str) (List.hd rhs_str)
          (List.map (fun l -> Printf.sprintf "%s := %s" l l) lhs_str |> String.concat "; ")
      else "()"
  | MultiAssign (lhss, rhss) ->
      stmt_to_ocaml ctx (Assign (lhss, rhss))
  | ShortDecl (names, exprs) ->
      let exprs_str = List.map (expr_to_ocaml ctx) exprs in
      if List.length names = List.length exprs_str then
        List.map2 (fun n e -> 
          let sn = value_ident n in
          if List.mem n ctx.mutable_vars then Printf.sprintf "let %s = ref %s in" sn e
          else Printf.sprintf "let %s = %s in" sn e
        ) names exprs_str |> String.concat " "
      else if List.length exprs_str = 1 then
        Printf.sprintf "let (%s) = %s in" (String.concat ", " (List.map value_ident names)) (List.hd exprs_str)
      else "()"
  | VarDeclStmt (name, typ_opt, expr_opt) ->
      let init = match expr_opt, typ_opt with
        | Some e, _ -> expr_to_ocaml ctx e
        | None, Some t -> default_expr_of_typ ctx t
        | _ -> "Obj.magic ()"
      in
      let sn = value_ident name in
      if List.mem name ctx.mutable_vars then Printf.sprintf "let %s = ref %s in" sn init
      else Printf.sprintf "let %s = %s in" sn init
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
  | IfInit (init_stmt, cond, then_b, else_opt) ->
      let if_code = stmt_to_ocaml ctx (If (cond, then_b, else_opt)) in
      (match init_stmt with
       | ShortDecl (names, exprs) ->
           let exprs_str = List.map (expr_to_ocaml ctx) exprs in
           if List.length names = List.length exprs_str then
             let decl =
               List.map2
                 (fun n e ->
                   let sn = value_ident n in
                   if List.mem n ctx.mutable_vars then Printf.sprintf "let %s = ref %s in" sn e
                   else Printf.sprintf "let %s = %s in" sn e)
                 names exprs_str
               |> String.concat " "
             in
             Printf.sprintf "%s %s" decl if_code
           else if List.length exprs_str = 1 then
             Printf.sprintf "let (%s) = %s in %s" (String.concat ", " (List.map value_ident names)) (List.hd exprs_str) if_code
           else Printf.sprintf "let () = () in %s" if_code
       | _ -> Printf.sprintf "let () = %s in %s" (stmt_to_ocaml ctx init_stmt) if_code)
  | TypeSwitch (bind, target, cases, default_opt) ->
      let bind_name = value_ident bind in
      let target_str = expr_to_ocaml ctx target in
      let render_case (labels, body) =
        let labels_str =
          match labels with
          | [] -> "_"
          | [l] -> constructor_ident l ^ " " ^ bind_name
          | ls -> String.concat " | " (List.map (fun l -> constructor_ident l ^ " " ^ bind_name) ls)
        in
        let case_ctx = bind_typeswitch_ctx ctx bind labels in
        let body_block = block_to_ocaml (with_indent case_ctx) body in
        if body_block = "\n  ()" then Printf.sprintf "%s ->\n    ()" labels_str
        else Printf.sprintf "%s ->%s" labels_str body_block
      in
      let rec build_match = function
        | [] ->
            (match default_opt with
             | Some body ->
                 let default_ctx = { ctx with var_types = (bind, TAny) :: ctx.var_types } in
                 let body_block = block_to_ocaml (with_indent default_ctx) body in
                 if body_block = "\n  ()" then "_ ->\n    ()"
                 else Printf.sprintf "_ ->%s" body_block
             | None -> "_ -> ()")
        | case_clause :: rest ->
            let branch = render_case case_clause in
            if rest = [] then branch else branch ^ "\n| " ^ build_match rest
      in
      let match_body = build_match cases in
      Printf.sprintf "match %s with\n| %s" target_str match_body
  | ForCond (cond, body) ->
      let cond_str = expr_to_ocaml ctx cond in
      let body_block = block_to_ocaml (with_indent ctx) body in
      let body_str = if body_block = "\n  ()" then "\n    ()\n" ^ indent ctx.indent
                     else "\n" ^ String.sub body_block 1 (String.length body_block - 1) ^ "\n" ^ indent ctx.indent
      in
      Printf.sprintf "while %s do%s done" cond_str body_str
  | ForClassic (init_opt, cond_opt, post_opt, body) ->
      let cond_str = match cond_opt with Some c -> expr_to_ocaml ctx c | None -> "true" in
      let loop_body = match post_opt with Some s -> body @ [s] | None -> body in
      let body_block = block_to_ocaml (with_indent ctx) loop_body in
      let body_str =
        if body_block = "\n  ()" then "\n    ()\n" ^ indent ctx.indent
        else "\n" ^ String.sub body_block 1 (String.length body_block - 1) ^ "\n" ^ indent ctx.indent
      in
      let loop_code = Printf.sprintf "while %s do%s done" cond_str body_str in
      (match init_opt with
       | None -> loop_code
       | Some init_stmt ->
           (match init_stmt with
            | ShortDecl (names, exprs) ->
                let exprs_str = List.map (expr_to_ocaml ctx) exprs in
                if List.length names = List.length exprs_str then
                  let decl =
                    List.map2
                      (fun n e ->
                        let sn = value_ident n in
                        if List.mem n ctx.mutable_vars then Printf.sprintf "let %s = ref %s in" sn e
                        else Printf.sprintf "let %s = %s in" sn e)
                      names exprs_str
                    |> String.concat " "
                  in
                  Printf.sprintf "%s %s" decl loop_code
                else if List.length exprs_str = 1 then
                  Printf.sprintf "let (%s) = %s in %s" (String.concat ", " (List.map value_ident names)) (List.hd exprs_str) loop_code
                else Printf.sprintf "let () = () in %s" loop_code
            | _ -> Printf.sprintf "let () = %s in %s" (stmt_to_ocaml ctx init_stmt) loop_code))
  | ForRange (k, v, coll, body) ->
      let coll_str = expr_to_ocaml ctx coll in
      let body_block = block_to_ocaml (with_indent ctx) body in
      let body_str = String.sub body_block 1 (String.length body_block - 1) in
      let sk = value_ident k in
      let sv = value_ident v in
      (match resolve_typ ctx (infer_expr_type ctx coll) with
           | TMap _ ->
               let body_expr = if body_str = "()" then "()" else Printf.sprintf "let () = %s in ()" body_str in
               Printf.sprintf "Hashtbl.iter (fun %s %s -> %s) %s"
                 (if k = "_" then "_" else sk)
                 (if v = "_" then "_" else sv)
                 body_expr
                 coll_str


           let key_bind = if k = "_" then "let _ = i in" else Printf.sprintf "let %s = i in" sk in
           let val_bind = if v = "_" then Printf.sprintf "let _ = (%s).(i) in" coll_str else Printf.sprintf "let %s = (%s).(i) in" sv coll_str in
           Printf.sprintf "for i = 0 to Array.length (%s) - 1 do\n%s  %s\n%s  %s\n%s\ndone"
             coll_str (indent (ctx.indent + 1)) key_bind (indent (ctx.indent + 1)) val_bind body_str)
  | Return exprs ->
      let rendered =
        if List.length exprs = List.length ctx.current_ret_types
        then List.map2 (coerce_expr_to_expected_type ctx) ctx.current_ret_types exprs
        else List.map (expr_to_ocaml ctx) exprs
      in
      let return_value =
        match rendered with
       | [] -> "()"
       | [e] -> e
       | _ -> "(" ^ String.concat ", " rendered ^ ")"
      in
      (match ctx.return_exn_name with
       | Some exn_name -> Printf.sprintf "raise (%s (%s))" exn_name return_value
       | None -> return_value)
  | ExprStmt e ->
      begin match e with
      | MethodCall (obj, method_name, args) when expr_to_ocaml ctx obj = "fmt" && method_name = "Println" ->
          let strings = List.map (expr_to_print_string ctx) args in
          let concat = String.concat " ^ \" \" ^ " strings in
          Printf.sprintf "print_endline (%s)" concat
      | MethodCall (obj, method_name, args) when expr_to_ocaml ctx obj = "fmt" && method_name = "Printf" ->
          let format_str = expr_to_ocaml ctx (List.hd args) in
          let other_args = List.tl args in
          let args_str = String.concat " " (List.map (expr_to_ocaml ctx) other_args) in
          Printf.sprintf "Printf.printf %s %s" format_str args_str
      | _ ->
          expr_to_ocaml ctx e
      end
  | Defer e -> Printf.sprintf "(* defer %s not implemented *)" (expr_to_ocaml ctx e)
  | Go e -> Printf.sprintf "(* go %s not implemented *)" (expr_to_ocaml ctx e)

and block_to_ocaml ctx = function
  | [] -> "\n  ()"
  | [stmt] ->
      Printf.sprintf "\n%s" (stmt_to_ocaml ctx stmt)
  | stmt :: rest ->
      (match stmt with
       | ShortDecl (names, exprs) ->
           let exprs_str = List.map (expr_to_ocaml ctx) exprs in
           let is_tuple = List.length names > 1 && List.length exprs_str = 1 in
           let inferred_types =
             if List.length names = List.length exprs then
               List.map (infer_expr_type ctx) exprs
             else List.init (List.length names) (fun _ -> TAny)
           in
           let new_ctx =
             List.fold_left2
               (fun c n t ->
                 let typed_c = { c with var_types = (n, t) :: c.var_types } in
                 if List.mem n ctx.mutable_vars then { typed_c with mutable_vars = n :: typed_c.mutable_vars }
                 else typed_c)
               ctx names inferred_types
           in
           let decl =
             if is_tuple then
               Printf.sprintf "let (%s) = %s in" (String.concat ", " (List.map value_ident names)) (List.hd exprs_str)
             else
               List.map2
                 (fun n (e, t) ->
                   let sn = value_ident n in
                   let expr_str = if e = Call(Var "make", []) || e = Call(Var "make", [Lit NilLit]) then
                                    default_expr_of_typ ctx t
                                  else expr_to_ocaml ctx e
                   in
                   if List.mem n ctx.mutable_vars then Printf.sprintf "let %s = ref %s in" sn expr_str
                   else Printf.sprintf "let %s = %s in" sn expr_str)
                 names (List.combine exprs inferred_types)
               |> String.concat " "
           in
           Printf.sprintf "\n%s (%s)" decl (block_to_ocaml new_ctx rest)
       | VarDeclStmt (name, typ_opt, expr_opt) ->
           let inferred_t = match expr_opt, typ_opt with
             | Some e, _ -> infer_expr_type ctx e
             | None, Some t -> t
             | _ -> TAny
           in
           let new_ctx = { ctx with var_types = (name, inferred_t) :: ctx.var_types } in
           let init = match expr_opt, typ_opt with
             | Some e, _ -> expr_to_ocaml ctx e
             | None, Some t -> default_expr_of_typ ctx t
             | _ -> "Obj.magic ()"
           in
           let sn = value_ident name in
           let decl = if List.mem name ctx.mutable_vars then Printf.sprintf "let %s = ref %s in" sn init
                      else Printf.sprintf "let %s = %s in" sn init
           in
           Printf.sprintf "\n%s (%s)" decl (block_to_ocaml new_ctx rest)
        | _ ->
            let first = stmt_to_ocaml ctx stmt in
            let inner = block_to_ocaml ctx rest in
            Printf.sprintf "\n(%s);\n%s" first (String.sub inner 1 (String.length inner - 1)))

and string_of_expr_string ctx expr =
  let expr_str = expr_to_ocaml ctx expr in
  match infer_expr_type ctx expr with
  | TString -> expr_str
  | TBool -> Printf.sprintf "string_of_bool (%s)" expr_str
  | TFloat64 -> Printf.sprintf "string_of_float (%s)" expr_str
  | TInt -> Printf.sprintf "string_of_int (%s)" expr_str
  | _ -> expr_str

and expr_to_print_string ctx = function
  | Lit (IntLit n) -> Int64.to_string n
  | Lit (RuneLit n) -> Printf.sprintf "%d" n
  | Lit (FloatLit f) -> string_of_float f
  | Lit (StringLit s) -> escape_ocaml_string s
  | Lit (BoolLit b) -> string_of_bool b
  | Lit NilLit -> "\"nil\""
  | Var x ->
      let sx = value_ident x in
      let base = if List.mem x ctx.mutable_vars then Printf.sprintf "!%s" sx else sx in
      begin match resolve_typ ctx (lookup_var_type ctx x) with
      | TString -> base
      | TBool -> Printf.sprintf "string_of_bool %s" base
      | TFloat64 -> Printf.sprintf "string_of_float %s" base
      | TInt -> Printf.sprintf "string_of_int %s" base
      | _ -> base
      end
  | expr ->
      string_of_expr_string ctx expr

let rec stmt_has_explicit_return = function
  | Return _ -> true
  | TypeSwitch (_, _, cases, default_opt) ->
      let case_has_returns =
        List.for_all
          (fun (_, body) -> List.exists stmt_has_explicit_return body)
          cases
      in
      (match default_opt with
       | None -> false
       | Some body -> case_has_returns && List.exists stmt_has_explicit_return body)
  | If (_, then_b, else_opt) ->
      let then_has = List.exists stmt_has_explicit_return then_b in
      let else_has = match else_opt with Some b -> List.exists stmt_has_explicit_return b | None -> false in
      then_has || else_has
  | IfInit (_, _, then_b, else_opt) ->
      let then_has = List.exists stmt_has_explicit_return then_b in
      let else_has = match else_opt with Some b -> List.exists stmt_has_explicit_return b | None -> false in
      then_has || else_has
  | ForCond (_, body) -> List.exists stmt_has_explicit_return body
  | ForClassic (_, _, _, body) -> List.exists stmt_has_explicit_return body
  | ForRange (_, _, _, body) -> List.exists stmt_has_explicit_return body
  | _ -> false

let block_has_explicit_return stmts =
  List.exists stmt_has_explicit_return stmts

let rec block_ends_with_return = function
  | [] -> false
  | [Return _] -> true
  | [_] -> false
  | _ :: rest -> block_ends_with_return rest

let wrap_function_body_with_return_handler raw_body_str ret_str default_ret =
  let body_core =
    if String.trim raw_body_str = "" || String.trim raw_body_str = "()" then
      "\n" ^ default_ret
    else
      raw_body_str ^ ";\n" ^ default_ret
  in
  Printf.sprintf
    "\nlet module Ret = struct exception E of %s end in\ntry%s\nwith\n| Ret.E v -> v"
    ret_str
    body_core

let decl_to_ocaml ctx = function
  | FuncDecl fd ->
      let param_str =
        List.map
          (fun (name, typ) ->
            let pname = value_ident name in
            match typ with
            | TAny -> Printf.sprintf "(%s)" pname
            | _ ->
                Printf.sprintf "(%s : %s)" pname
                  (ocaml_type_of_typ ~normalize_alias_primitives:true ctx typ))
          fd.params
        |> String.concat " "
      in
      let ret_str =
        match fd.ret with
        | [] -> "unit"
        | [t] -> ocaml_type_of_typ ~normalize_alias_primitives:true ctx t
        | ts ->
            "("
            ^ String.concat " * "
                (List.map
                   (ocaml_type_of_typ ~normalize_alias_primitives:true ctx)
                   ts)
            ^ ")"
      in
      let mutable_vars = collect_mutable_in_func fd in
      let params_types = List.map (fun (name, typ) -> (name, typ)) fd.params in
      let return_exn_name =
        if fd.ret = [] then None else Some "Ret.E"
      in
      let ctx' = {
        ctx with
        mutable_vars = mutable_vars;
        var_types = params_types @ ctx.var_types;
        current_ret_types = fd.ret;
        return_exn_name;
      } in
      let raw_body_str = block_to_ocaml (with_indent ctx') fd.body in
      let fname = value_ident fd.name in
      let body_str =
        if fd.ret = [] then raw_body_str
        else
          let default_ret =
            match fd.ret with
            | [t] -> default_expr_of_typ ctx t
            | ts -> "(" ^ String.concat ", " (List.map (default_expr_of_typ ctx) ts) ^ ")"
          in
          wrap_function_body_with_return_handler raw_body_str ret_str default_ret
      in
      if fd.params = [] then
        Printf.sprintf "let rec %s () : %s =%s" fname ret_str body_str
      else
        Printf.sprintf "let rec %s %s : %s =%s" fname param_str ret_str body_str
  | VarDecl (name, typ_opt, expr_opt) ->
      let typ_str =
        match typ_opt with
        | Some TAny | None -> ""
        | Some t ->
            " : "
            ^ ocaml_type_of_typ ~normalize_alias_primitives:true ctx t
      in
      let init =
        match expr_opt, typ_opt with
        | Some e, _ -> expr_to_ocaml ctx e
        | None, Some t -> default_expr_of_typ ctx t
        | _ -> "Obj.magic ()"
      in
      Printf.sprintf "let %s%s = %s" (value_ident name) typ_str init
  | TypeDecl (name, TStruct fields) ->
      (match List.assoc_opt name ctx.interface_impls with
       | Some _ -> "" (* Skip, it will be part of interface definition *)
       | None ->
          if fields = [] then
            Printf.sprintf "type %s = unit" (type_ident name)
          else
            let fields_str =
              List.map (fun (fname, ftyp) ->
                Printf.sprintf "mutable %s : %s;" (field_ident fname) (ocaml_type_of_typ ctx ftyp)
              ) fields |> String.concat " "
            in
            Printf.sprintf "type %s = { %s }" (type_ident name) fields_str)
  | TypeDecl (name, TInterface _) ->
      (match List.assoc_opt name ctx.interface_impls with
       | Some impls ->
           let variants =
             List.map (fun impl ->
               Printf.sprintf "| %s of %s" (constructor_ident impl) (type_ident impl)
             ) impls
             |> String.concat "\n  "
           in
           Printf.sprintf "type %s =\n  %s" (type_ident name) variants
       | None -> Printf.sprintf "type %s = Obj.t" (type_ident name))
  | TypeDecl (name, t) ->
      Printf.sprintf "type %s = %s" (type_ident name) (ocaml_type_of_typ ctx t)

let rewrite_const_like_decls (decls: decl list) : decl list =
  let mk_iota n = Lit (IntLit (Int64.of_int n)) in
  let rec consume_enum_run anchor_t saw_any acc = function
    | (VarDecl (name, Some TAny, None)) :: rest ->
        consume_enum_run anchor_t true ((name, Some TAny) :: acc) rest
    | (VarDecl (name, Some t, None)) :: rest when t = anchor_t ->
        consume_enum_run anchor_t saw_any ((name, Some t) :: acc) rest
    | rest -> (List.rev acc, saw_any, rest)
  in
  let rewrite_enum_run anchor_t members =
    let rec loop i acc = function
      | [] -> List.rev acc
      | (name, typ_opt) :: rest ->
          let fixed_typ =
            match typ_opt with
            | Some TAny | None -> Some anchor_t
            | Some t -> Some t
          in
          loop (i + 1) (VarDecl (name, fixed_typ, Some (mk_iota i)) :: acc) rest
    in
    loop 0 [] members
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | (VarDecl (name, Some anchor_t, None)) :: rest when anchor_t <> TAny ->
        let members, saw_any, tail =
          consume_enum_run anchor_t false [ (name, Some anchor_t) ] rest
        in
        if List.length members > 1 && saw_any then
          let rewritten = rewrite_enum_run anchor_t members in
          loop (List.rev_append rewritten acc) tail
        else
          let original = List.map (fun (n, t_opt) -> VarDecl (n, t_opt, None)) members in
          loop (List.rev_append original acc) tail
    | d :: rest -> loop (d :: acc) rest
  in
  loop [] decls

let program_to_ocaml (prog: program) : string =
  let prog = { prog with decls = rewrite_const_like_decls prog.decls } in
  Printf.printf "Codegen: Procesando %d declaraciones\n" (List.length prog.decls);
  
  let type_aliases =
    List.fold_left
      (fun acc -> function
        | TypeDecl (name, t) -> (name, t) :: acc
        | _ -> acc)
      [] prog.decls
  in

  (* Sprint 3: Interface implementations detection *)
  let interfaces = List.filter_map (function TypeDecl (n, TInterface ms) -> Some (n, ms) | _ -> None) prog.decls in
  let interface_impls = List.map (fun (iface_name, _) ->
    let impls = List.filter_map (function
      | FuncDecl fd ->
          let marker = "is" ^ String.lowercase_ascii iface_name in
          if String.lowercase_ascii fd.name = marker then
            match fd.params with
            | [(_, TName struct_name)] -> Some struct_name
            | _ -> None
          else None
      | _ -> None
    ) prog.decls in
    (iface_name, impls)
  ) interfaces in

  let func_ret_types =
    List.fold_left (fun acc -> function
      | FuncDecl { name; ret = [t]; _ } -> (value_ident name, t) :: acc
      | _ -> acc
    ) [] prog.decls
  in
  let infer_var_decl_type typ_opt expr_opt =
    match typ_opt, expr_opt with
    | Some t, _ when t <> TAny -> t
    | _, Some (Lit (IntLit _)) -> TInt
    | _, Some (Lit (FloatLit _)) -> TFloat64
    | _, Some (Lit (StringLit _)) -> TString
    | _, Some (Lit (BoolLit _)) -> TBool
    | _ -> TAny
  in
  let global_var_types =
    List.fold_left
      (fun acc -> function
        | VarDecl (name, typ_opt, expr_opt) -> (name, infer_var_decl_type typ_opt expr_opt) :: acc
        | _ -> acc)
      [] prog.decls
  in
  let ctx = { empty_ctx with func_ret_types; type_aliases; var_types = global_var_types; interface_impls } in
  
  let rec group_types acc current_group = function
    | [] -> 
        if current_group = [] then List.rev acc
        else List.rev (List.rev current_group :: acc)
    | (TypeDecl _ as d) :: rest -> group_types acc (d :: current_group) rest
    | d :: rest ->
        if current_group = [] then group_types ([d] :: acc) [] rest
        else group_types ([d] :: (List.rev current_group :: acc)) [] rest
  in
  let groups = group_types [] [] prog.decls in
  
  let render_group group =
    match group with
    | [TypeDecl (n, t)] -> decl_to_ocaml ctx (TypeDecl (n, t))
    | (TypeDecl _ :: _) as ts ->
        let items = List.map (function
          | TypeDecl (name, TStruct fields) ->
              (match List.assoc_opt name ctx.interface_impls with
               | Some _ -> None
               | None ->
                  if fields = [] then Some (Printf.sprintf "%s = unit" (type_ident name))
                  else
                    let fields_str =
                      List.map (fun (fname, ftyp) ->
                        Printf.sprintf "mutable %s : %s;" (field_ident fname) (ocaml_type_of_typ ctx ftyp)
                      ) fields |> String.concat " "
                    in
                    Some (Printf.sprintf "%s = { %s }" (type_ident name) fields_str))
          | TypeDecl (name, TInterface _) ->
              (match List.assoc_opt name ctx.interface_impls with
               | Some impls ->
                   let variants =
                     List.map (fun impl ->
                       Printf.sprintf "| %s of %s" (constructor_ident impl) (type_ident impl)
                     ) impls
                     |> String.concat "\n  "
                   in
                   Some (Printf.sprintf "%s =\n  %s" (type_ident name) variants)
               | None -> Some (Printf.sprintf "%s = Obj.t" (type_ident name)))
          | TypeDecl (name, t) ->
              Some (Printf.sprintf "%s = %s" (type_ident name) (ocaml_type_of_typ ctx t))
          | _ -> None
        ) ts |> List.filter_map (fun x -> x) in
        (match items with
         | [] -> ""
         | head :: tail ->
             "type " ^ head ^ "\n" ^ (String.concat "" (List.map (fun s -> "and " ^ s ^ "\n") tail)))
    | ds -> String.concat "\n\n" (List.map (decl_to_ocaml ctx) ds)
  in

  let decls_str = 
    List.map render_group groups
    |> List.filter (fun s -> s <> "")
    |> String.concat "\n\n" 
  in
  
  let final_main =
    if List.exists (function FuncDecl { name = "main"; params = _; ret = _; body = _ } -> true | _ -> false) prog.decls then
      Printf.sprintf "\n\nlet () = %s ()" (value_ident "main")
    else ""
  in
  decls_str ^ final_main

let generate (prog: program) : string = program_to_ocaml prog
