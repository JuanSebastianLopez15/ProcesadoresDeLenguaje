(* lib/codegen.ml *)
open Ast

(* Contexto que propaga información por el recorrido *)
type ctx = {
  indent : int;
  var_types : (string * typ) list;        (* tipos inferidos de variables locales *)
  func_sigs : (string * (typ list * typ)) list;
  struct_defs : (string * (string * typ) list) list;
}

let empty_ctx = {
  indent = 0;
  var_types = [];
  func_sigs = [];
  struct_defs = [];
}

let with_indent ctx = { ctx with indent = ctx.indent + 1 }
let indent_str ctx = String.make (2 * ctx.indent) ' '

let lookup_var ctx x = List.assoc_opt x ctx.var_types
let lookup_func ctx f = List.assoc_opt f ctx.func_sigs
let lookup_struct ctx s = List.assoc_opt s ctx.struct_defs

(* Palabras reservadas de OCaml *)
let ocaml_keywords = [
  "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
  "downto"; "else"; "end"; "exception"; "external"; "false"; "for";
  "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit";
  "initializer"; "lazy"; "let"; "match"; "method"; "module"; "mutable";
  "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec";
  "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
  "virtual"; "when"; "while"; "with"; "ref"; "unit"
]

(* Convierte CamelCase a snake_case para identificadores de valor *)
let camel_to_snake s =
  let buf = Buffer.create (String.length s + 4) in
  String.iteri (fun i c ->
    if i > 0 && c >= 'A' && c <= 'Z' then Buffer.add_char buf '_';
    Buffer.add_char buf (Char.lowercase_ascii c)
  ) s;
  Buffer.contents buf

let value_ident raw =
  if String.contains raw '.' then
    let parts = String.split_on_char '.' raw in
    String.concat "." (List.map camel_to_snake parts)
  else
    let s = camel_to_snake raw in
    if List.mem s ocaml_keywords then s ^ "_" else s

let type_ident raw = value_ident raw
let field_ident raw = value_ident raw

let rec ocaml_type_of = function
  | TInt -> "int"
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TVoid -> "unit"
  | TSlice t -> ocaml_type_of t ^ " list"
  | TName n -> type_ident n

(* Formato de string literal con escapes *)
let escape_ocaml_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter (function
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* Infiere el tipo de una expresión *)
let rec infer_type ctx = function
  | Lit (IntLit _) -> TInt
  | Lit (FloatLit _) -> TFloat
  | Lit (StringLit _) -> TString
  | Lit (BoolLit _) -> TBool
  | Var x -> (match lookup_var ctx x with Some t -> t | None -> TVoid)
  | BinOp ((Add|Sub|Mul|Div|Mod), l, _) -> infer_type ctx l
  | BinOp ((Eq|Neq|Lt|Gt|Leq|Geq|And|Or), _, _) -> TBool
  | UnOp (Not, _) -> TBool
  | UnOp (Neg, e) -> infer_type ctx e
  | Call (name, _) ->
      (match lookup_func ctx name with
       | Some (_, ret) -> ret
       | None ->
           (* builtins *)
           match name with
           | "len" -> TInt
           | "int" | "int64" -> TInt
           | "float64" -> TFloat
           | _ -> TVoid)
  | Index (arr, _) ->
      (match infer_type ctx arr with TSlice t -> t | _ -> TVoid)
  | Selector (e, field) ->
      (match infer_type ctx e with
       | TName n ->
           (match lookup_struct ctx n with
            | Some fields ->
                (match List.assoc_opt field fields with
                 | Some t -> t | None -> TVoid)
            | None -> TVoid)
       | _ -> TVoid)
  | StructLit (name, _) -> TName name
  | SliceLit (t, _) -> TSlice t
  | Cast (t, _) -> t

let rec uses_rune_cast_expr ctx = function
  | Cast (TSlice TInt, e) ->
      (match infer_type ctx e with TString -> true | _ -> uses_rune_cast_expr ctx e)
  | Call ("string", [arg]) ->
      (match infer_type ctx arg with TSlice TInt -> true | _ -> uses_rune_cast_expr ctx arg)
  | Cast (_, e) | UnOp (_, e) | Selector (e, _) -> uses_rune_cast_expr ctx e
  | BinOp (_, a, b) | Index (a, b) ->
      uses_rune_cast_expr ctx a || uses_rune_cast_expr ctx b
  | Call (_, args) -> List.exists (uses_rune_cast_expr ctx) args
  | StructLit (_, fs) -> List.exists (fun (_, e) -> uses_rune_cast_expr ctx e) fs
  | SliceLit (_, es) -> List.exists (uses_rune_cast_expr ctx) es
  | _ -> false

let rec uses_rune_cast_stmt ctx s =
  match s with
  | ShortDecl (x, e) ->
      let used = uses_rune_cast_expr ctx e in
      let t = infer_type ctx e in
      let new_ctx = { ctx with var_types = (x, t) :: ctx.var_types } in
      (used, new_ctx)
  | Assign (x, e) ->
      let used = uses_rune_cast_expr ctx e in
      let t = infer_type ctx e in
      let new_ctx = { ctx with var_types = (x, t) :: ctx.var_types } in
      (used, new_ctx)
  | FieldAssign (lhs, rhs) ->
      let used = uses_rune_cast_expr ctx lhs || uses_rune_cast_expr ctx rhs in
      (used, ctx)
  | ExprStmt e -> (uses_rune_cast_expr ctx e, ctx)
  | Return (Some e) -> (uses_rune_cast_expr ctx e, ctx)
  | Return None -> (false, ctx)
  | If (c, t, e) ->
      let used_c = uses_rune_cast_expr ctx c in
      let used_t = uses_rune_cast_list ctx t in
      let used_e = match e with Some es -> uses_rune_cast_list ctx es | None -> false in
      (used_c || used_t || used_e, ctx)

and uses_rune_cast_list ctx stmts =
  let rec loop ctx = function
    | [] -> false
    | s :: rest ->
        let (used, new_ctx) = uses_rune_cast_stmt ctx s in
        used || loop new_ctx rest
  in
  loop ctx stmts

let uses_rune_cast ctx prog =
  List.exists (function
    | FuncDecl fd ->
        let local_ctx = {
          ctx with
          var_types = List.map (fun (n, t) -> (n, t)) fd.params;
        } in
        uses_rune_cast_list local_ctx fd.body
    | _ -> false
  ) prog.decls

let preamble =
  "(* Helpers Unicode generados por el compilador *)\n\
   let __string_to_runes (s : string) : int list =\n\
  \  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in\n\
  \  let rec loop acc =\n\
  \    match Uutf.decode dec with\n\
  \    | `Uchar u -> loop (Uchar.to_int u :: acc)\n\
  \    | `End -> List.rev acc\n\
  \    | `Malformed _ -> loop acc\n\
  \    | `Await -> List.rev acc\n\
  \  in\n\
  \  loop []\n\n\
   let __runes_to_string (rs : int list) : string =\n\
  \  let buf = Buffer.create 16 in\n\
  \  List.iter (fun r -> Uutf.Buffer.add_utf_8 buf (Uchar.of_int r)) rs;\n\
  \  Buffer.contents buf\n\n"

(* Determina si una expresión es float para elegir operadores *)
let is_float_expr ctx e =
  match infer_type ctx e with TFloat -> true | _ -> false

let rec fold_constants e =
  let e' = match e with
    | BinOp (op, l, r) -> BinOp (op, fold_constants l, fold_constants r)
    | UnOp (op, e1) -> UnOp (op, fold_constants e1)
    | Call (n, args) -> Call (n, List.map fold_constants args)
    | Index (arr, idx) -> Index (fold_constants arr, fold_constants idx)
    | Selector (e1, f) -> Selector (fold_constants e1, f)
    | StructLit (n, args) -> StructLit (n, List.map (fun (k, v) -> (k, fold_constants v)) args)
    | SliceLit (t, elems) -> SliceLit (t, List.map fold_constants elems)
    | Cast (t, e1) -> Cast (t, fold_constants e1)
    | _ -> e
  in
  match e' with
  | UnOp (Not, UnOp (Not, e'')) -> e''
  | UnOp (Neg, Lit (IntLit n)) -> Lit (IntLit (Int64.neg n))
  | BinOp (Add, Lit (IntLit a), Lit (IntLit b)) -> Lit (IntLit (Int64.add a b))
  | BinOp (Sub, Lit (IntLit a), Lit (IntLit b)) -> Lit (IntLit (Int64.sub a b))
  | BinOp (Mul, Lit (IntLit a), Lit (IntLit b)) -> Lit (IntLit (Int64.mul a b))
  | BinOp (Div, Lit (IntLit a), Lit (IntLit b)) when b <> 0L -> Lit (IntLit (Int64.div a b))
  | BinOp (Add, e1, Lit (IntLit 0L)) | BinOp (Add, Lit (IntLit 0L), e1) -> e1
  | BinOp (Mul, e1, Lit (IntLit 1L)) | BinOp (Mul, Lit (IntLit 1L), e1) -> e1
  | BinOp (Mul, _, Lit (IntLit 0L)) | BinOp (Mul, Lit (IntLit 0L), _) -> Lit (IntLit 0L)
  | BinOp (Add, Lit (FloatLit a), Lit (FloatLit b)) -> Lit (FloatLit (a +. b))
  | BinOp (Sub, Lit (FloatLit a), Lit (FloatLit b)) -> Lit (FloatLit (a -. b))
  | BinOp (Mul, Lit (FloatLit a), Lit (FloatLit b)) -> Lit (FloatLit (a *. b))
  | BinOp (Div, Lit (FloatLit a), Lit (FloatLit b)) when b <> 0.0 -> Lit (FloatLit (a /. b))
  | _ -> e'

let prec_of_expr = function
  | Var _ | Lit _ | StructLit _ | SliceLit _ | Cast _ -> 100
  | Call _ | Selector _ | Index _ -> 100
  | UnOp _ -> 110
  | BinOp ((Mul|Div|Mod), _, _) -> 70
  | BinOp ((Add|Sub), _, _) -> 60
  | BinOp ((Eq|Neq|Lt|Gt|Leq|Geq), _, _) -> 50
  | BinOp (And, _, _) -> 40
  | BinOp (Or, _, _) -> 30

let rec render_expr ctx parent_prec e =
  let e_folded = fold_constants e in
  let my_prec = prec_of_expr e_folded in
  let s = match e_folded with
    | Lit (IntLit n) -> Int64.to_string n
    | Lit (FloatLit f) ->
        let str = string_of_float f in
        if String.contains str '.' then str else str ^ "0"
    | Lit (StringLit str) -> escape_ocaml_string str
    | Lit (BoolLit true) -> "true"
    | Lit (BoolLit false) -> "false"

    | Var x -> value_ident x

    | BinOp (op, l, r) ->
        let is_float = is_float_expr ctx l || is_float_expr ctx r in
        let op_s = binop_to_ocaml op is_float in
        let next_prec = if op = Eq || op = Neq || op = Lt || op = Gt || op = Leq || op = Geq then my_prec + 1 else my_prec in
        Printf.sprintf "%s %s %s" (render_expr ctx my_prec l) op_s (render_expr ctx next_prec r)

    | UnOp (Not, e') ->
        Printf.sprintf "not %s" (render_expr ctx 110 e')
    | UnOp (Neg, e') ->
        if is_float_expr ctx e' then
          Printf.sprintf "-. %s" (render_expr ctx 110 e')
        else
          Printf.sprintf "- %s" (render_expr ctx 110 e')

    | Call ("len", [arg]) ->
        let arg_type = infer_type ctx arg in
        let arg_rendered = render_arg ctx arg in
        (match arg_type with
         | TString -> Printf.sprintf "String.length %s" arg_rendered
         | _ -> Printf.sprintf "List.length %s" arg_rendered)

    | Call ("append", [slice; elem]) ->
        Printf.sprintf "%s @ [%s]" (render_arg ctx slice) (render_arg ctx elem)

    | Call ("int", [arg]) | Call ("int64", [arg]) ->
        if is_float_expr ctx arg then
          Printf.sprintf "int_of_float %s" (render_arg ctx arg)
        else
          render_expr ctx parent_prec arg

    | Call ("float64", [arg]) ->
        if is_float_expr ctx arg then render_expr ctx parent_prec arg
        else Printf.sprintf "float_of_int %s" (render_arg ctx arg)

    | Call ("string", [arg]) ->
        (match infer_type ctx arg with
         | TSlice TInt -> Printf.sprintf "__runes_to_string %s" (render_arg ctx arg)
         | _ -> render_expr ctx parent_prec arg)

    | Call ("rune", [arg]) | Call ("int32", [arg]) ->
        if is_float_expr ctx arg then
          Printf.sprintf "int_of_float %s" (render_arg ctx arg)
        else
          render_expr ctx parent_prec arg

    | Call ("fmt.Println", args) ->
        let parts = List.map (expr_to_print_string ctx) args in
        (match parts with
         | [] -> "print_string \"\\n\""
         | [one] -> Printf.sprintf "print_string (%s ^ \"\\n\")" one
         | many ->
             let joined = String.concat " ^ \" \" ^ " many in
             Printf.sprintf "print_string (%s ^ \"\\n\")" joined)

    | Call ("fmt.Print", args) ->
        let parts = List.map (expr_to_print_string ctx) args in
        (match parts with
         | [] -> "()"
         | [one] -> Printf.sprintf "print_string %s" one
         | many ->
             let joined = String.concat " ^ " many in
             Printf.sprintf "print_string (%s)" joined)

    | Call ("fmt.Printf", Lit (StringLit fmt) :: rest) ->
        let ocaml_fmt = translate_printf_format fmt in
        let args_s = String.concat " " (List.map (render_arg ctx) rest) in
        Printf.sprintf "Printf.printf %s %s" (escape_ocaml_string ocaml_fmt) args_s

    | Call (name, args) ->
        let fname = value_ident name in
        let args_s = List.map (render_arg ctx) args in
        (match args_s with
         | [] -> Printf.sprintf "%s ()" fname
         | _ -> Printf.sprintf "%s %s" fname (String.concat " " args_s))

    | Index (arr, idx) ->
        Printf.sprintf "List.nth %s %s" (render_arg ctx arr) (render_arg ctx idx)

    | Selector (e', field) ->
        let base = match e' with
          | Var _ | Selector _ -> render_expr ctx 100 e'
          | _ -> "(" ^ render_expr ctx 0 e' ^ ")"
        in
        Printf.sprintf "%s.%s" base (field_ident field)

    | StructLit (name, args) ->
        let fields = match lookup_struct ctx name with
          | Some f -> f
          | None -> []
        in
        let field_strs = List.mapi (fun i (key_opt, value) ->
          let fname = match key_opt with
            | Some k -> k
            | None ->
                (match List.nth_opt fields i with
                 | Some (fn, _) -> fn
                 | None -> "unknown")
          in
          Printf.sprintf "%s = %s" (field_ident fname) (render_expr ctx 0 value)
        ) args in
        Printf.sprintf "{ %s }" (String.concat "; " field_strs)

    | SliceLit (t, elems) ->
        let parts = List.map (fun e ->
          match e with
          | StructLit ("", args) ->
              (match t with
               | TName name -> render_expr ctx 0 (StructLit (name, args))
               | _ -> render_expr ctx 0 e)
          | _ -> render_expr ctx 0 e
        ) elems in
        Printf.sprintf "[%s]" (String.concat "; " parts)

    | Cast (TSlice TInt, e) ->
        (match infer_type ctx e with
         | TString -> Printf.sprintf "__string_to_runes %s" (render_arg ctx e)
         | _ -> render_arg ctx e)

    | Cast (TString, e) ->
        (match infer_type ctx e with
         | TSlice TInt -> Printf.sprintf "__runes_to_string %s" (render_arg ctx e)
         | _ -> render_arg ctx e)

    | Cast (TInt, e) ->
        if is_float_expr ctx e then
          Printf.sprintf "int_of_float %s" (render_arg ctx e)
        else render_arg ctx e

    | Cast (TFloat, e) ->
        if is_float_expr ctx e then render_arg ctx e
        else Printf.sprintf "float_of_int %s" (render_arg ctx e)

    | Cast (_, e) ->
        render_arg ctx e
  in
  if my_prec < parent_prec then "(" ^ s ^ ")" else s

and render_arg ctx e =
  match e with
  | Var _ -> render_expr ctx 100 e
  | Lit (IntLit n) when n >= 0L -> render_expr ctx 100 e
  | Lit (FloatLit f) when f >= 0.0 -> render_expr ctx 100 e
  | Lit (StringLit _) | Lit (BoolLit _) -> render_expr ctx 100 e
  | SliceLit (_, []) -> "[]"
  | SliceLit (_, [_]) -> render_expr ctx 100 e
  | Selector (Var _, _) -> render_expr ctx 100 e
  | _ -> "(" ^ render_expr ctx 0 e ^ ")"

and binop_to_ocaml op is_float =
  match op with
  | Add -> if is_float then "+." else "+"
  | Sub -> if is_float then "-." else "-"
  | Mul -> if is_float then "*." else "*"
  | Div -> if is_float then "/." else "/"
  | Mod -> "mod"
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

and expr_to_print_string ctx expr =
  match infer_type ctx expr with
  | TInt -> Printf.sprintf "(string_of_int %s)" (render_arg ctx expr)
  | TFloat -> Printf.sprintf "(string_of_float %s)" (render_arg ctx expr)
  | TBool -> Printf.sprintf "(string_of_bool %s)" (render_arg ctx expr)
  | TString -> render_expr ctx 0 expr
  | _ -> render_expr ctx 0 expr

and translate_printf_format fmt =
  let buf = Buffer.create (String.length fmt) in
  let n = String.length fmt in
  let i = ref 0 in
  while !i < n do
    let c = fmt.[!i] in
    if c = '%' && !i + 1 < n then begin
      let next = fmt.[!i + 1] in
      (match next with
       | 't' ->
           Buffer.add_string buf "%b";
           i := !i + 2
       | _ ->
           Buffer.add_char buf c;
           incr i;
           while !i < n && not (is_format_letter fmt.[!i]) do
             Buffer.add_char buf fmt.[!i];
             incr i
           done;
           if !i < n then begin
             Buffer.add_char buf fmt.[!i];
             incr i
           end)
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

and is_format_letter c =
  match c with
  | 'd' | 'i' | 's' | 'f' | 'e' | 'g' | 'b' | 't' | 'c' | 'x' | 'o' -> true
  | _ -> false

let expr_to_ocaml ctx e = render_expr ctx 0 e

let rec all_paths_return = function
  | [] -> false
  | [Return _] -> true
  | [If (_, t, Some e)] -> all_paths_return t && all_paths_return e
  | _ :: rest -> all_paths_return rest

let rec simplify_stmts stmts =
  match stmts with
  | [] -> []
  | If (cond, [Return (Some (Lit (BoolLit true)))], Some [Return (Some (Lit (BoolLit false)))]) :: rest ->
      Return (Some cond) :: simplify_stmts rest
  | If (cond, [Return (Some (Lit (BoolLit false)))], Some [Return (Some (Lit (BoolLit true)))]) :: rest ->
      Return (Some (UnOp (Not, cond))) :: simplify_stmts rest
  | If (cond, [Return (Some (Lit (BoolLit true)))], None) :: Return (Some (Lit (BoolLit false))) :: rest ->
      Return (Some cond) :: simplify_stmts rest
  | If (cond, [Return (Some (Lit (BoolLit false)))], None) :: Return (Some (Lit (BoolLit true))) :: rest ->
      Return (Some (UnOp (Not, cond))) :: simplify_stmts rest
  | If (cond, t, e) :: rest ->
      If (cond, simplify_stmts t, Option.map simplify_stmts e) :: simplify_stmts rest
  | s :: rest -> s :: simplify_stmts rest

(* Extrae la variable raíz de cualquier cadena de Selectors.
   a.b     → Some "a"
   a.b.c   → Some "a"
   f().b   → None (no tiene raíz variable) *)
let rec lhs_root_var = function
  | Var x -> Some x
  | Selector (Var x, _) -> Some x
  | Selector (inner, _) -> lhs_root_var inner
  | _ -> None

(* Construye record update anidado desde adentro hacia afuera.
   a.b = v     → "{ a with b = v }"
   a.b.c = v   → "{ a with b = { a.b with c = v } }"
   La recursión sube por el Selector, produciendo updates anidados. *)
let rec build_record_update ctx lhs rhs_s =
  match lhs with
  | Selector (Var x, field) ->
      Printf.sprintf "{ %s with %s = %s }"
        (value_ident x) (field_ident field) rhs_s
  | Selector (inner, field) ->
      (* Paso 1: construye el update del nivel actual *)
      let inner_base_s = render_expr ctx 100 inner in
      let current_level =
        Printf.sprintf "{ %s with %s = %s }" inner_base_s (field_ident field) rhs_s
      in
      (* Paso 2: sube un nivel, envolviendo en el update del padre *)
      build_record_update ctx inner current_level
  | _ -> failwith "build_record_update: LHS inválido"

let rec transform_block ctx = function
  | [] -> "()"
  | [Return None] -> "()"
  | [Return (Some e)] -> expr_to_ocaml ctx e
  | [If (c, t, Some e)] when all_paths_return t && all_paths_return e ->
      format_if ctx c t e
  | If (c, t, None) :: rest when all_paths_return t ->
      format_if ctx c t rest
  | If (c, t, Some e) :: _ when all_paths_return t && all_paths_return e ->
      format_if ctx c t e

  (* Caso especial: ShortDecl(x, default); If(cond, [Assign(x, new_val)], None); rest *)
  | ShortDecl (x, default_val) :: If (c, [Assign (x', new_val)], None) :: rest when x = x' ->
      let t = infer_type ctx new_val in
      let new_ctx = { ctx with var_types = (x, t) :: ctx.var_types } in
      Printf.sprintf "let %s = if %s then %s else %s in\n%s%s"
        (value_ident x)
        (expr_to_ocaml ctx c)
        (expr_to_ocaml ctx new_val)
        (expr_to_ocaml ctx default_val)
        (indent_str ctx)
        (transform_block new_ctx rest)

  (* Caso especial: dos ShortDecl + If con dos Assign en el cuerpo.
     ShortDecl(x1,d1); ShortDecl(x2,d2); If(cond, [Assign(x1',v1); Assign(x2',v2)], None); rest
     -> let x1 = if cond then v1 else d1 in
        let x2 = if cond then v2 else d2 in rest *)
  | ShortDecl (x1, d1) :: ShortDecl (x2, d2) :: If (c, assigns, None) :: rest
    when (let names = List.filter_map (function Assign(n,_) -> Some n | _ -> None) assigns in
          List.mem x1 names && List.mem x2 names && List.length assigns = 2) ->
      let pairs = List.filter_map (function Assign(n,e) -> Some(n,e) | _ -> None) assigns in
      let v1 = List.assoc x1 pairs in
      let v2 = List.assoc x2 pairs in
      let t1 = infer_type ctx v1 in
      let t2 = infer_type ctx v2 in
      let ctx1 = { ctx with var_types = (x1, t1) :: ctx.var_types } in
      let ctx2 = { ctx1 with var_types = (x2, t2) :: ctx1.var_types } in
      let cond_s = expr_to_ocaml ctx c in
      Printf.sprintf "let %s = if %s then %s else %s in\n%slet %s = if %s then %s else %s in\n%s%s"
        (value_ident x1) cond_s (expr_to_ocaml ctx v1) (expr_to_ocaml ctx d1)
        (indent_str ctx)
        (value_ident x2) cond_s (expr_to_ocaml ctx1 v2) (expr_to_ocaml ctx d2)
        (indent_str ctx)
        (transform_block ctx2 rest)

  (* Caso especial: If con múltiples Assign en el cuerpo, todas variables ya declaradas.
     if cond { x1 = v1; x2 = v2; ... } rest
     -> let x1 = if cond then v1 else x1 in
        let x2 = if cond then v2 else x2 in rest *)
  | If (c, assigns, None) :: rest
    when assigns <> [] &&
         List.for_all (function Assign _ -> true | _ -> false) assigns &&
         List.length assigns > 1 ->
      let cond_s = expr_to_ocaml ctx c in
      let pairs = List.filter_map (function Assign(n,e) -> Some(n,e) | _ -> None) assigns in
      let (result, final_ctx) = List.fold_left (fun (acc, cur_ctx) (x, e_new) ->
        let t = infer_type cur_ctx e_new in
        let new_ctx = { cur_ctx with var_types = (x, t) :: cur_ctx.var_types } in
        let line = Printf.sprintf "let %s = if %s then %s else %s in\n%s"
          (value_ident x) cond_s (expr_to_ocaml cur_ctx e_new) (value_ident x)
          (indent_str ctx)
        in
        (acc ^ line, new_ctx)
      ) ("", ctx) pairs in
      result ^ (transform_block final_ctx rest)

  (* Caso especial: if cond { x = e } rest -> let x = if cond then e else x in rest *)
  | If (c, [Assign (x, e_new)], None) :: rest ->
      let t = infer_type ctx e_new in
      let new_ctx = { ctx with var_types = (x, t) :: ctx.var_types } in
      Printf.sprintf "let %s = if %s then %s else %s in\n%s%s"
        (value_ident x)
        (expr_to_ocaml ctx c)
        (expr_to_ocaml ctx e_new)
        (value_ident x)
        (indent_str ctx)
        (transform_block new_ctx rest)

  (* PATRÓN B: If(cond, [FieldAssign(x.*.f, new)], None) — x ya declarada antes *)
  | If (c, [FieldAssign (lhs, new_val)], None) :: rest
    when lhs_root_var lhs <> None ->
      let x = Option.get (lhs_root_var lhs) in
      let update_s = build_record_update ctx lhs (expr_to_ocaml ctx new_val) in
      Printf.sprintf "let %s = if %s then %s else %s in\n%s%s"
        (value_ident x)
        (expr_to_ocaml ctx c)
        update_s
        (value_ident x)
        (indent_str ctx)
        (transform_block ctx rest)

  (* PATRÓN C: FieldAssign standalone — cualquier profundidad, catch-all exhaustivo *)
  | FieldAssign (lhs, rhs) :: rest ->
      (match lhs_root_var lhs with
       | Some x ->
           let update_s = build_record_update ctx lhs (expr_to_ocaml ctx rhs) in
           Printf.sprintf "let %s = %s in\n%s%s"
             (value_ident x) update_s
             (indent_str ctx)
             (transform_block ctx rest)
       | None ->
           failwith (Printf.sprintf "FieldAssign: LHS sin variable raíz: %s"
             (expr_to_ocaml ctx lhs)))

  | If (c, t, None) :: rest ->
      Printf.sprintf "(if %s then\n%s%s\n%selse ());\n%s%s"
        (expr_to_ocaml ctx c)
        (indent_str (with_indent ctx))
        (transform_block (with_indent ctx) t)
        (indent_str ctx)
        (indent_str ctx)
        (transform_block ctx rest)
  | If (c, t, Some e) :: rest ->
      Printf.sprintf "(if %s then\n%s%s\n%selse\n%s%s);\n%s%s"
        (expr_to_ocaml ctx c)
        (indent_str (with_indent ctx))
        (transform_block (with_indent ctx) t)
        (indent_str ctx)
        (indent_str (with_indent ctx))
        (transform_block (with_indent ctx) e)
        (indent_str ctx)
        (transform_block ctx rest)
  | ShortDecl (x, e) :: rest ->
      let t = infer_type ctx e in
      let new_ctx = { ctx with var_types = (x, t) :: ctx.var_types } in
      Printf.sprintf "let %s = %s in\n%s%s"
        (value_ident x)
        (expr_to_ocaml ctx e)
        (indent_str ctx)
        (transform_block new_ctx rest)
  | Assign (x, e) :: rest ->
      Printf.sprintf "let %s = %s in\n%s%s"
        (value_ident x)
        (expr_to_ocaml ctx e)
        (indent_str ctx)
        (transform_block ctx rest)
  | [ExprStmt e; Return None] when infer_type ctx e = TVoid ->
      expr_to_ocaml ctx e
  | ExprStmt e :: rest ->
      if rest = [] then expr_to_ocaml ctx e
      else
        Printf.sprintf "%s;\n%s%s"
          (expr_to_ocaml ctx e)
          (indent_str ctx)
          (transform_block ctx rest)
  | Return None :: _ -> "()"
  | Return (Some e) :: _ -> expr_to_ocaml ctx e

and format_if ctx cond then_b else_b =
  let cond_s = expr_to_ocaml ctx cond in
  let then_s = transform_block (with_indent ctx) then_b in
  let needs_wrapping stmts =
    match stmts with
    | [] | [_] -> false
    | _ -> true
  in
  let then_wrapped =
    if needs_wrapping then_b then
      Printf.sprintf "(\n%s%s\n%s)" (indent_str (with_indent ctx)) then_s (indent_str ctx)
    else then_s
  in
  match else_b with
  | [If (c, t, e_opt)] ->
      let e = Option.value ~default:[] e_opt in
      Printf.sprintf "if %s then\n%s%s\n%selse %s"
        cond_s
        (indent_str (with_indent ctx)) then_wrapped
        (indent_str ctx)
        (format_if ctx c t e)
  | [] ->
      Printf.sprintf "if %s then\n%s%s\n%selse ()"
        cond_s
        (indent_str (with_indent ctx)) then_wrapped
        (indent_str ctx)
  | _ ->
      let else_s = transform_block (with_indent ctx) else_b in
      let else_wrapped =
        if needs_wrapping else_b then
          Printf.sprintf "(\n%s%s\n%s)" (indent_str (with_indent ctx)) else_s (indent_str ctx)
        else else_s
      in
      Printf.sprintf "if %s then\n%s%s\n%selse\n%s%s"
        cond_s
        (indent_str (with_indent ctx)) then_wrapped
        (indent_str ctx)
        (indent_str (with_indent ctx)) else_wrapped

let struct_decl_to_ocaml sd =
  let fields_s = List.map (fun (name, t) ->
    Printf.sprintf "%s : %s" (field_ident name) (ocaml_type_of t)
  ) sd.fields in
  Printf.sprintf "type %s = { %s }"
    (type_ident sd.name)
    (String.concat "; " fields_s)

(* Grafo de dependencias y SCC *)
let rec collect_calls ctx e acc =
  match e with
  | Call (n, args) ->
      let acc' = if List.mem_assoc n ctx.func_sigs then n :: acc else acc in
      List.fold_left (fun a arg -> collect_calls ctx arg a) acc' args
  | BinOp (_, l, r) -> collect_calls ctx r (collect_calls ctx l acc)
  | UnOp (_, e1) -> collect_calls ctx e1 acc
  | Index (arr, idx) -> collect_calls ctx idx (collect_calls ctx arr acc)
  | Selector (e1, _) -> collect_calls ctx e1 acc
  | StructLit (_, args) -> List.fold_left (fun a (_, v) -> collect_calls ctx v a) acc args
  | SliceLit (_, elems) -> List.fold_left (fun a v -> collect_calls ctx v a) acc elems
  | _ -> acc

let rec collect_calls_stmt ctx s acc =
  match s with
  | ShortDecl (_, e) | Assign (_, e) | ExprStmt e | Return (Some e) ->
      collect_calls ctx e acc
  | FieldAssign (lhs, rhs) ->
      collect_calls ctx rhs (collect_calls ctx lhs acc)
  | Return None -> acc
  | If (c, t, e_opt) ->
      let acc' = collect_calls ctx c acc in
      let acc'' = List.fold_left (fun a s' -> collect_calls_stmt ctx s' a) acc' t in
      Option.fold ~none:acc'' ~some:(fun e_stmts ->
        List.fold_left (fun a s' -> collect_calls_stmt ctx s' a) acc'' e_stmts
      ) e_opt

let scc_groups funcs =
  let index = ref 0 in
  let stack = ref [] in
  let in_stack = Hashtbl.create 16 in
  let v_index = Hashtbl.create 16 in
  let v_lowlink = Hashtbl.create 16 in
  let sccs = ref [] in

  let rec strongconnect v edges =
    Hashtbl.add v_index v !index;
    Hashtbl.add v_lowlink v !index;
    incr index;
    stack := v :: !stack;
    Hashtbl.add in_stack v true;

    List.iter (fun w ->
      if not (Hashtbl.mem v_index w) then begin
        let w_edges = try List.assoc w funcs with Not_found -> [] in
        strongconnect w w_edges;
        let v_low = Hashtbl.find v_lowlink v in
        let w_low = Hashtbl.find v_lowlink w in
        Hashtbl.replace v_lowlink v (min v_low w_low)
      end else if Hashtbl.mem in_stack w && Hashtbl.find in_stack w then begin
        let v_low = Hashtbl.find v_lowlink v in
        let w_idx = Hashtbl.find v_index w in
        Hashtbl.replace v_lowlink v (min v_low w_idx)
      end
    ) edges;

    if Hashtbl.find v_lowlink v = Hashtbl.find v_index v then begin
      let rec pop_scc acc =
        match !stack with
        | w :: rest ->
            stack := rest;
            Hashtbl.replace in_stack w false;
            let acc' = w :: acc in
            if w = v then acc' else pop_scc acc'
        | [] -> acc
      in
      sccs := pop_scc [] :: !sccs
    end
  in

  List.iter (fun (v, edges) ->
    if not (Hashtbl.mem v_index v) then strongconnect v edges
  ) funcs;
  List.rev !sccs

let func_decl_to_ocaml ctx fd keyword =
  let params_s =
    if fd.params = [] then "()"
    else
      String.concat " " (List.map (fun (n, t) ->
        Printf.sprintf "(%s : %s)" (value_ident n) (ocaml_type_of t)
      ) fd.params)
  in
  let ret_s = ocaml_type_of fd.ret in
  let local_ctx = {
    ctx with
    indent = 1;
    var_types = List.map (fun (n, t) -> (n, t)) fd.params;
  } in
  let body_s = transform_block local_ctx (simplify_stmts fd.body) in
  Printf.sprintf "%s %s %s : %s =\n  %s"
    keyword (value_ident fd.name) params_s ret_s body_s

let program_to_ocaml prog =
  let struct_defs = List.filter_map (function
    | StructDecl sd -> Some (sd.name, sd.fields)
    | _ -> None
  ) prog.decls in
  let func_sigs = List.filter_map (function
    | FuncDecl fd -> Some (fd.name, (List.map snd fd.params, fd.ret))
    | _ -> None
  ) prog.decls in
  let ctx = { empty_ctx with struct_defs; func_sigs } in

  let struct_decls = List.filter_map (function
    | StructDecl sd -> Some (struct_decl_to_ocaml sd)
    | _ -> None
  ) prog.decls in

  let func_decls = List.filter_map (function
    | FuncDecl fd -> Some fd
    | _ -> None
  ) prog.decls in

  let call_graph = List.map (fun fd ->
    let calls = List.fold_left (fun acc s -> collect_calls_stmt ctx s acc) [] fd.body in
    (* Remove duplicates *)
    let calls = List.sort_uniq String.compare calls in
    (fd.name, calls)
  ) func_decls in

  let sccs = scc_groups call_graph in

  let func_strs = List.map (fun scc ->
    match scc with
    | [] -> ""
    | [f_name] ->
        let fd = List.find (fun (fd : func_decl) -> fd.name = f_name) func_decls in
        let edges = try List.assoc f_name call_graph with Not_found -> [] in
        if List.mem f_name edges then
          func_decl_to_ocaml ctx fd "let rec"
        else
          func_decl_to_ocaml ctx fd "let"
    | _ ->
        let fds = List.map (fun name -> List.find (fun (fd : func_decl) -> fd.name = name) func_decls) scc in
        let strs = List.mapi (fun i fd ->
          let kw = if i = 0 then "let rec" else "and" in
          func_decl_to_ocaml ctx fd kw
        ) fds in
        String.concat "\n" strs
  ) sccs in

  let struct_block =
    if struct_decls = [] then ""
    else String.concat "\n" struct_decls ^ "\n\n"
  in
  let func_block = String.concat "\n\n" func_strs in
  let main_call = "\n\nlet () = main ()\n" in
  let preamble_str = if uses_rune_cast ctx prog then preamble else "" in
  preamble_str ^ struct_block ^ func_block ^ main_call

let generate prog = program_to_ocaml prog
