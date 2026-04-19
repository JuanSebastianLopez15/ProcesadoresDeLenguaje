open Lib.Ast

type error =
    | UndeclaredVar of string
    | TypeMismatch of { expected: typ; got: typ; context: string}
    | WrongArgCount of {func_name: string; expected: int; got: int}
    | UndeclaredFunc of string
    | ReturnTypeMismatch of {expected: typ list; got: typ list}
    | NotCallable of string
    | VoidUsedAsValue of string
exception TypeError of error

let go_numeric_named_types =
  [ "int8"; "int16"; "int32"; "int64";
    "uint"; "uint8"; "uint16"; "uint32"; "uint64"; "uintptr";
    "float32"; "float64"; "complex64"; "complex128"; "byte"; "rune" ]

let is_numeric_type = function
  | TInt | TFloat64 -> true
  | TName n -> List.mem n go_numeric_named_types
  | _ -> false

let rec pp_typ = function
  | TInt      -> "int"
  | TFloat64 -> "float64"
  | TString  -> "string"
  | TBool    -> "bool"
  | TNil     -> "nil"
  | TVoid    -> "void"
  | TAny     -> "any"
  | TSlice t -> "[]" ^ pp_typ t
  | TMap (k, v) -> "map[" ^ pp_typ k ^ "]" ^ pp_typ v
  | TName n -> n
  | TStruct fields -> 
      let fs = String.concat "; " (List.map (fun (n, t) -> n ^ " " ^ pp_typ t) fields) in
      "struct { " ^ fs ^ " }"
  | TFunc (params, []) ->
      let ps = String.concat ", " (List.map pp_typ params) in
      Printf.sprintf "func(%s)" ps
  | TFunc (params, rets) ->
      let ps = String.concat ", " (List.map pp_typ params) in
      let rs = String.concat ", " (List.map pp_typ rets) in
      Printf.sprintf "func(%s) (%s)" ps rs

(* ── Helpers de compatibilidad ───────────────────────── *)

let rec types_compatible t1 t2 =
  match t1, t2 with
  | a, b when a = b -> true
  | TAny, _ | _, TAny -> true
  | TNil, (TSlice _ | TMap _ | TName _ | TFunc _) 
  | (TSlice _ | TMap _ | TFunc _ | TName _), TNil -> true
  | TSlice t1', TSlice t2' -> types_compatible t1' t2'
  (* Alias comunes en Go *)
  | TInt, TName "int64" | TName "int64", TInt -> true
  | TFloat64, TName "float32" | TName "float32", TFloat64 -> true
  | TFloat64, TName "float64" | TName "float64", TFloat64 -> true
  | TName n1, TName n2 when List.mem n1 go_numeric_named_types && List.mem n2 go_numeric_named_types -> true
  | _ -> false

let expect_type ~context expected got =
  if not (types_compatible expected got)
  then raise (TypeError (TypeMismatch { expected; got; context }))

let lookup_or_fail env name =
  match Env.lookup name env with
  | Some t -> t
  | None   -> raise (TypeError (UndeclaredVar name))

let reject_void ~context t =
  match t with
  | TVoid -> raise (TypeError (VoidUsedAsValue context))
  | _ -> ()

(* ── Inferencia de expresiones ───────────────────────── *)

let rec check_expr env = function
  | Lit (IntLit _)    -> TInt
  | Lit (FloatLit _)  -> TFloat64
  | Lit (StringLit _) -> TString
  | Lit (BoolLit _)   -> TBool
  | Lit NilLit        -> TNil

  | Var x -> lookup_or_fail env x

  | BinOp ((Add|Sub|Mul|Div|Mod), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type ~context:"operación aritmética" tl tr;
      (match tl with
       | _ when is_numeric_type tl || tl = TAny -> tl
       | _ -> raise (TypeError (TypeMismatch
           { expected = TInt; got = tl; context = "operación aritmética" })))

  | BinOp ((Eq|Neq|Lt|Gt|Leq|Geq), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type ~context:"comparación" tl tr;
      TBool

  | BinOp ((And|Or), l, r) ->
      expect_type ~context:"and/or" TBool (check_expr env l);
      expect_type ~context:"and/or" TBool (check_expr env r);
      TBool

  | UnOp (Not, e) ->
      expect_type ~context:"not" TBool (check_expr env e);
      TBool
  | UnOp (Neg, e) ->
      let t = check_expr env e in
      (match t with _ when is_numeric_type t || t = TAny -> t
       | _ -> raise (TypeError (TypeMismatch
           { expected = TInt; got = t; context = "negación" })))

  | UnOp ((Inc|Dec), e) ->
      let t = check_expr env e in
      if is_numeric_type t || t = TAny then TInt
      else raise (TypeError (TypeMismatch { expected = TInt; got = t; context = "inc/dec" }))

  (* --- MANEJO DE LLAMADAS Y CONVERSIONES (CASTS) --- *)
  | Call (Var name, args) ->
      (match name, args with
       | "len", [arg] ->
         let t = check_expr env arg in
         if types_compatible (TSlice TAny) t || t = TString || types_compatible (TMap (TAny, TAny)) t then TInt
         else raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "len" }))
       | "cap", [arg] ->
         let t = check_expr env arg in
         if types_compatible (TSlice TAny) t || t = TString || t = TAny then TInt
         else raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "cap" }))
       | "make", _ -> TAny
       | "new", [_] -> TAny
       | "append", [slice; elem] ->
         let ts = check_expr env slice in
         let te = check_expr env elem in
         (match ts with TSlice ti -> expect_type ~context:"append" ti te; ts | _ -> ts)
       | "copy", [_; _] -> TInt
       | "delete", [_; _] -> TVoid
       | "close", [_] -> TVoid
       | "complex", [_; _] -> TName "complex128"
       | "real", [_] -> TFloat64
       | "imag", [_] -> TFloat64
       | "recover", [] -> TAny
       | "panic", [_] -> TVoid
       | ("int64" | "int" | "float64" | "string"), [arg] ->
         let _ = check_expr env arg in
         if name = "int64" then TName "int64"
         else if name = "float64" then TFloat64
         else if name = "string" then TString
         else TInt
       | _ ->
         (match Env.lookup name env with
        | Some (TFunc (param_types, ret_types)) ->
          let n_expected = List.length param_types
          and n_got = List.length args in
          if n_expected <> n_got
          then raise (TypeError (WrongArgCount { func_name = name; expected = n_expected; got = n_got }));
          List.iter2 (fun expected_t arg ->
            let got_t = check_expr env arg in
            reject_void ~context:("argumento de " ^ name) got_t;
            expect_type ~context:("argumento de " ^ name) expected_t got_t
          ) param_types args;
          (match ret_types with
           | [] -> TVoid
           | [t] -> t
           | ts -> TFunc ([], ts))
        | Some _ -> raise (TypeError (NotCallable name))
        | None -> raise (TypeError (UndeclaredFunc name))))

  | Call (e, args) ->
      let t = check_expr env e in
      (match t with
       | TFunc (param_types, ret_types) ->
          let n_expected = List.length param_types
          and n_got = List.length args in
          if n_expected <> n_got
          then raise (TypeError (WrongArgCount { func_name = "anon"; expected = n_expected; got = n_got }));
          List.iter2 (fun expected_t arg ->
            let got_t = check_expr env arg in
            expect_type ~context:"argumento de función" expected_t got_t
          ) param_types args;
          (match ret_types with
           | [] -> TVoid
           | [t] -> t
           | ts -> TFunc ([], ts))
       | TAny -> TAny
       | _ -> raise (TypeError (NotCallable "expression")))

  | MethodCall (Var "fmt", ("Println"|"Printf"|"Print"), _) -> TVoid
        | MethodCall (Var "fmt", "Errorf", _) -> TAny
        | MethodCall (obj, _, _) -> let _ = check_expr env obj in TAny 

  | Index (arr, idx) ->
      expect_type ~context:"índice" TInt (check_expr env idx);
      (match check_expr env arr with
       | TAny -> TAny | TSlice t -> t
       | TString -> TInt (* Go strings return bytes, which are int-ish here *)
       | t -> raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "indexación" })))

  | Slice (arr, low, high, max) ->
      (match low with Some e -> expect_type ~context:"low index" TInt (check_expr env e) | None -> ());
      (match high with Some e -> expect_type ~context:"high index" TInt (check_expr env e) | None -> ());
      (match max with Some e -> expect_type ~context:"max index" TInt (check_expr env e) | None -> ());
      let t = check_expr env arr in
      (match t with
       | TAny | TSlice _ | TString -> t
       | _ -> raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "slicing" })))

  | Selector (e, _) -> let _ = check_expr env e in TAny 
  | StructLit (name, args) -> let _ = List.map (check_expr env) args in TName name
  | SliceLit (t, args) ->
      List.iter (fun arg -> let got_t = check_expr env arg in expect_type ~context:"slice" t got_t) args;
      TSlice t
  | Cast (t, e) -> let _ = check_expr env e in t

(* ── Verificación de statements ──────────────────────── *)

and check_stmt env expected_ret = function
  | ShortDecl (names, exprs) ->
      let types = match names, exprs with
        | [n], [e] -> [check_expr env e]
        | ns, [Call (e, args)] ->
            (match check_expr env (Call (e, args)) with
             | TFunc (_, rets) -> rets
             | TAny -> List.map (fun _ -> TAny) ns
             | t -> [t])
        | ns, es -> List.map (check_expr env) es
      in
      if List.length names <> List.length types then
         (* Simplemente extendemos con TAny si no coinciden, para ser permisivos *)
         List.fold_left2 (fun e n t -> Env.extend n t e) env names (List.init (List.length names) (fun _ -> TAny))
      else
         List.fold_left2 (fun e n t -> reject_void ~context:n t; Env.extend n t e) env names types
  | Assign (lhss, rhss) ->
      (* Similar a ShortDecl pero sin extender env *)
      let _ = List.map (check_expr env) lhss in
      let _ = List.map (check_expr env) rhss in
      env
  | Return exprs ->
      let got = List.map (check_expr env) exprs in
      (match expected_ret, got with
       | [], [] -> ()
       | [et], [gt] -> expect_type ~context:"return" et gt
       | _ -> if expected_ret <> got then raise (TypeError (ReturnTypeMismatch { expected = expected_ret; got })));
      env
  | If (cond, then_block, else_opt) ->
      expect_type ~context:"condición if" TBool (check_expr env cond);
      let _ = check_block env expected_ret then_block in
      (match else_opt with Some b -> ignore (check_block env expected_ret b) | None -> ()); env
  | ForCond (cond, body) ->
      expect_type ~context:"condición for" TBool (check_expr env cond);
      ignore (check_block env expected_ret body); env
  | ForRange (k, v, collection, body) ->
      let elem_t = match check_expr env collection with
        | TAny -> TAny | TSlice t -> t | TMap (_, vt) -> vt
        | t -> raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "range" }))
      in
      let inner_env = env |> Env.extend k TInt |> Env.extend v elem_t in
      ignore (check_block inner_env expected_ret body); env
  | ExprStmt e -> ignore (check_expr env e); env
  | Defer e | Go e -> ignore (check_expr env e); env
  | _ -> env

and check_block env expected_ret stmts =
  List.fold_left (fun env stmt -> check_stmt env expected_ret stmt) env stmts

let check_decl env = function
  | FuncDecl { name; params; ret; body } ->
      let func_typ = TFunc (List.map snd params, ret) in
      let env' = Env.extend name func_typ env in
      let local_env = List.fold_left (fun e (pname, ptyp) -> Env.extend pname ptyp e) env' params in
      ignore (check_block local_env ret body); env'
  | VarDecl (name, typ_opt, expr_opt) ->
      let t = match typ_opt, expr_opt with
        | Some t, None   -> t
        | None,   Some e -> check_expr env e
        | Some t, Some e -> expect_type ~context:("var " ^ name) t (check_expr env e); t
        | None, None -> TNil
      in Env.extend name t env
  | TypeDecl (_, _) -> env

let pp_error = function
  | UndeclaredVar x -> Printf.sprintf "variable no declarada: '%s'" x
  | TypeMismatch { expected; got; context } ->
      Printf.sprintf "tipo incorrecto en %s: se esperaba %s, se obtuvo %s" context (pp_typ expected) (pp_typ got)
  | WrongArgCount { func_name; expected; got } -> Printf.sprintf "'%s' espera %d argumentos, se pasaron %d" func_name expected got
  | UndeclaredFunc f -> Printf.sprintf "función no declarada: '%s'" f
  | ReturnTypeMismatch _ -> "tipo de retorno no coincide con la firma de la función"
  | NotCallable x -> Printf.sprintf "'%s' no es una función" x
  | VoidUsedAsValue context -> Printf.sprintf "expresión de tipo void usada como valor en '%s'" context

let check_program (prog : program) : (program, string) result =
  try
    let env0 = Env.base in
    let _ = List.fold_left check_decl env0 prog.decls in Ok prog
  with TypeError e -> Error (pp_error e)