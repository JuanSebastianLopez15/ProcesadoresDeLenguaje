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

let type_def_key name = "__type__:" ^ name

let rec resolve_named_type env = function
  | TName n as t ->
      (match Env.lookup (type_def_key n) env with
       | Some t' when t' <> t -> resolve_named_type env t'
       | _ -> t)
  | t -> t

let is_numeric_type_in_env env t =
  let resolved = resolve_named_type env t in
  is_numeric_type t || is_numeric_type resolved

let is_index_type_in_env env t =
  match resolve_named_type env t with
  | TInt | TAny -> true
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
  | TInterface _ -> "interface{...}"
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

let rec types_compatible env t1 t2 =
  match t1, t2 with
  | a, b when a = b -> true
  | TAny, _ | _, TAny -> true
  | a, b when is_numeric_type a && is_numeric_type b -> true
  | TNil, (TSlice _ | TMap _ | TName _ | TFunc _ | TInterface _) 
  | (TSlice _ | TMap _ | TFunc _ | TName _ | TInterface _), TNil -> true
  | TSlice t1', TSlice t2' -> types_compatible env t1' t2'
  | TName n, (TName _ as struct_t) when is_interface_name env n ->
      is_impl_of_interface env n struct_t
  | _ -> false

and is_interface_name env name =
  match Env.lookup (type_def_key name) env with
  | Some (TInterface _) -> true
  | _ -> false

and is_impl_of_interface env iface_name struct_t =
  (* Try both lowercase and exact iface name capitalization *)
  let lookup_marker name =
    match Env.lookup ("Is" ^ name) env with
    | Some t -> Some t
    | None -> Env.lookup ("is" ^ String.lowercase_ascii name) env
  in
  match lookup_marker iface_name with
  | Some (TFunc ([param_t], _)) ->
      (* Permissive: if struct matches param or is TAny, it's an impl *)
      types_compatible env param_t struct_t || param_t = TAny || struct_t = TAny
  | Some _ -> true
  | _ -> false

let is_generic_type_param env name =
  not (List.mem name go_numeric_named_types)
  && Env.lookup (type_def_key name) env = None
  && not (is_interface_name env name)

let rec types_compatible_with_generics env expected got =
  match expected, got with
  | TName n, _ when is_generic_type_param env n -> true
  | _, TName n when is_generic_type_param env n -> true
  | TSlice te, TSlice tg -> types_compatible_with_generics env te tg
  | TMap (ke, ve), TMap (kg, vg) ->
      types_compatible_with_generics env ke kg
      && types_compatible_with_generics env ve vg
  | TFunc (eps, ers), TFunc (gps, grs) ->
      List.length eps = List.length gps
      && List.length ers = List.length grs
      && List.for_all2 (types_compatible_with_generics env) eps gps
      && List.for_all2 (types_compatible_with_generics env) ers grs
  | _ ->
      let expected' = resolve_named_type env expected in
      let got' = resolve_named_type env got in
      types_compatible env expected got
      || types_compatible env expected' got'
      || types_compatible_with_generics_resolved env expected' got'

and types_compatible_with_generics_resolved env expected got =
  match expected, got with
  | TName n, _ when is_generic_type_param env n -> true
  | _, TName n when is_generic_type_param env n -> true
  | TSlice te, TSlice tg -> types_compatible_with_generics_resolved env te tg
  | TMap (ke, ve), TMap (kg, vg) ->
      types_compatible_with_generics_resolved env ke kg
      && types_compatible_with_generics_resolved env ve vg
  | TFunc (eps, ers), TFunc (gps, grs) ->
      List.length eps = List.length gps
      && List.length ers = List.length grs
      && List.for_all2 (types_compatible_with_generics_resolved env) eps gps
      && List.for_all2 (types_compatible_with_generics_resolved env) ers grs
  | _ -> types_compatible env expected got

let types_compatible_in_env env expected got =
  types_compatible_with_generics env expected got

let expect_type env ~context expected got =
  if not (types_compatible_in_env env expected got)
  then 
    match expected, got with
    | TName n, _ when is_interface_name env n -> () (* Bypass interface mismatch for now *)
    | _ -> raise (TypeError (TypeMismatch { expected; got; context }))

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
  | Lit (RuneLit _)   -> TInt
  | Lit (FloatLit _)  -> TFloat64
  | Lit (StringLit _) -> TString
  | Lit (BoolLit _)   -> TBool
  | Lit NilLit        -> TNil

  | Var x -> lookup_or_fail env x

  | BinOp ((Add|Sub|Mul|Div|Mod|BAnd|BOr|BXor|Shl|Shr|AndNot), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type env ~context:"operación aritmética" tl tr;
      (match tl with
       | _ when is_numeric_type_in_env env tl || tl = TAny -> tl
       | _ -> raise (TypeError (TypeMismatch
            { expected = TInt; got = tl; context = "operación aritmética" })))

  | BinOp ((Eq|Neq), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      (match tl, tr with
       | TNil, _ | _, TNil -> ()
       | _ -> expect_type env ~context:"comparación" tl tr);
      TBool

  | BinOp ((Lt|Gt|Leq|Geq), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type env ~context:"comparación" tl tr;
      TBool

  | BinOp ((And|Or), l, r) ->
      expect_type env ~context:"and/or" TBool (check_expr env l);
      expect_type env ~context:"and/or" TBool (check_expr env r);
      TBool

  | UnOp (Not, e) ->
      let t = check_expr env e in
      if t = TBool then TBool else TAny
  | UnOp (Neg, e) ->
      let t = check_expr env e in
      (match t with _ when is_numeric_type_in_env env t || t = TAny -> t
       | _ -> raise (TypeError (TypeMismatch
            { expected = TInt; got = t; context = "negación" })))

  | UnOp ((Inc|Dec), e) ->
      let t = check_expr env e in
      if is_numeric_type_in_env env t || t = TAny then TInt
      else raise (TypeError (TypeMismatch { expected = TInt; got = t; context = "inc/dec" }))

  | UnOp (AddrOf, e) -> TSlice (check_expr env e)
  | UnOp (Deref, e) -> 
      (match check_expr env e with
       | TSlice t -> t
       | TAny -> TAny
       | t -> raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "dereferencia" })))

  (* --- MANEJO DE LLAMADAS Y CONVERSIONES (CASTS) --- *)
  | Call (Var name, args) ->
      (match Env.lookup name env, args with
       | Some (TName tname), [arg] when tname = name ->
         let _ = check_expr env arg in
         TName name
       | _ ->
      (match name, args with
        | "len", [arg] ->
          let t = check_expr env arg in
          if types_compatible env (TSlice TAny) t || t = TString || types_compatible env (TMap (TAny, TAny)) t then TInt
         else raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "len" }))
       | "cap", [arg] ->
         let t = check_expr env arg in
         if types_compatible env (TSlice TAny) t || t = TString || t = TAny then TInt
         else raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "cap" }))
       | "make", _ -> TAny
       | "new", [_] -> TAny
        | "append", [slice; elem] ->
          let ts = check_expr env slice in
          let te = check_expr env elem in
           (match ts, te with
            | TSlice ti, TSlice tj ->
                expect_type env ~context:"append" ti tj;
                ts
            | TSlice ti, _ ->
                expect_type env ~context:"append" ti te;
                ts
            | _ -> ts)
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
            expect_type env ~context:("argumento de " ^ name) expected_t got_t
          ) param_types args;
          (match ret_types with
           | [] -> TVoid
           | [t] -> t
           | ts -> TFunc ([], ts))
        | Some TAny -> TAny
        | Some _ -> raise (TypeError (NotCallable name))
        | None -> raise (TypeError (UndeclaredFunc name)))))

  | Call (Selector (recv, member), args) ->
      let is_pkg_call =
        match recv with
        | Var ("fmt" | "strings" | "sort" | "errors") -> true
        | _ -> false
      in
      if not is_pkg_call then ignore (check_expr env recv);
      List.iter (fun arg -> ignore (check_expr env arg)) args;
      (match recv, member with
       | Var "fmt", ("Println" | "Printf" | "Print") -> TVoid
       | Var "fmt", "Sprintf" -> TString
       | Var "fmt", "Errorf" -> TAny
       | Var "fmt", "Sscanf" -> TInt
       | Var "strings", "Fields" -> TSlice TString
       | Var "strings", "Join" -> TString
       | Var "sort", "Strings" -> TVoid
       | Var "errors", "New" -> TAny
       | _ -> TAny)

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
            expect_type env ~context:"argumento de función" expected_t got_t
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
      let arr_t = check_expr env arr in
      let idx_t = check_expr env idx in
      (match arr_t with
       | TAny -> TAny
       | TSlice t ->
           if not (is_index_type_in_env env idx_t)
           then raise (TypeError (TypeMismatch { expected = TInt; got = idx_t; context = "índice" }));
           t
       | TString ->
           if not (is_index_type_in_env env idx_t)
           then raise (TypeError (TypeMismatch { expected = TInt; got = idx_t; context = "índice" }));
           TInt (* Go strings return bytes, which are int-ish here *)
       | TMap (k, v) ->
           expect_type env ~context:"índice de map" k idx_t;
           v
       | t -> raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "indexación" })))

  | Slice (arr, low, high, max) ->
      (match low with Some e -> expect_type env ~context:"low index" TInt (check_expr env e) | None -> ());
      (match high with Some e -> expect_type env ~context:"high index" TInt (check_expr env e) | None -> ());
      (match max with Some e -> expect_type env ~context:"max index" TInt (check_expr env e) | None -> ());
      let t = check_expr env arr in
      (match t with
       | TAny | TSlice _ | TString -> t
       | _ -> raise (TypeError (TypeMismatch { expected = TSlice TAny; got = t; context = "slicing" })))

  | Selector (e, _) -> let _ = check_expr env e in TAny 
  | StructLit (name, args) -> let _ = List.map (fun (_, e) -> check_expr env e) args in TName name
  | Spread e -> check_expr env e
  | SliceLit (t, args) ->
      (match t with
       | TMap (k, v) ->
           List.iter
             (fun (key_opt, e) ->
               let got_v = check_expr env e in
               expect_type env ~context:"map literal value" v got_v;
               match key_opt with
               | None -> ()
               | Some key ->
                   let key_t =
                     if String.length key > 0 && key.[0] >= '0' && key.[0] <= '9'
                     then TInt
                     else TString
                   in
                   expect_type env ~context:"map literal key" k key_t)
             args;
           t
       | _ ->
           List.iter (fun (_, e) -> let got_t = check_expr env e in expect_type env ~context:"slice" t got_t) args;
           TSlice t)
  | Cast (t, e) -> let _ = check_expr env e in t
  | KeyedExpr (_, e) -> check_expr env e
  | FuncLit fd -> 
      let local_env = List.fold_left (fun e (pname, ptyp) -> Env.extend pname ptyp e) env fd.params in
      let _ = check_block local_env fd.ret fd.body in
      TFunc (List.map snd fd.params, fd.ret)

(* ── Verificación de statements ──────────────────────── *)

and check_stmt env expected_ret = function
  | ShortDecl (names, exprs) ->
      let types = match names, exprs with
        | [_], [e] -> [check_expr env e]
        | ns, [Call (e, args)] ->
            (match check_expr env (Call (e, args)) with
             | TFunc (_, rets) -> rets
             | TAny -> List.map (fun _ -> TAny) ns
             | t -> [t])
        | _, es -> List.map (check_expr env) es
      in
      if List.length names <> List.length types then
         List.fold_left2 (fun e n t -> Env.extend n t e) env names (List.init (List.length names) (fun _ -> TAny))
      else
         List.fold_left2 (fun e n t -> reject_void ~context:n t; Env.extend n t e) env names types
  | VarDeclStmt (name, typ_opt, expr_opt) ->
      let t = match typ_opt, expr_opt with
        | Some t, None -> t
        | None, Some e -> check_expr env e
        | Some t, Some e -> expect_type env ~context:("var " ^ name) t (check_expr env e); t
        | None, None -> TAny
      in
      Env.extend name t env
  | Assign (lhss, rhss) ->
      (* Similar a ShortDecl pero sin extender env *)
      let _ = List.map (check_expr env) lhss in
      let _ = List.map (check_expr env) rhss in
      env
  | Return exprs ->
      let got = List.map (check_expr env) exprs in
      (match expected_ret, got with
       | [], [] -> ()
       | [et], [gt] -> expect_type env ~context:"return" et gt
       | _ ->
           if List.length expected_ret <> List.length got then
             raise (TypeError (ReturnTypeMismatch { expected = expected_ret; got }))
           else
             List.iter2 (fun et gt -> expect_type env ~context:"return" et gt) expected_ret got);
      env
  | If (cond, then_block, else_opt) ->
      expect_type env ~context:"condición if" TBool (check_expr env cond);
      let _ = check_block env expected_ret then_block in
      (match else_opt with Some b -> ignore (check_block env expected_ret b) | None -> ()); env
  | IfInit (init_stmt, cond, then_block, else_opt) ->
      let if_env = check_stmt env expected_ret init_stmt in
      expect_type if_env ~context:"condición if" TBool (check_expr if_env cond);
      let _ = check_block if_env expected_ret then_block in
      (match else_opt with Some b -> ignore (check_block if_env expected_ret b) | None -> ());
      env
  | TypeSwitch (bind, target, cases, default_opt) ->
      ignore (check_expr env target);
      List.iter
        (fun (labels, body) ->
          List.iter
            (fun label ->
              let case_env = Env.extend bind (TName label) env in
              ignore (check_block case_env expected_ret body))
            labels)
        cases;
      (match default_opt with
       | None -> ()
       | Some body ->
           let def_env = Env.extend bind TAny env in
           ignore (check_block def_env expected_ret body));
      env
  | ForCond (cond, body) ->
      expect_type env ~context:"condición for" TBool (check_expr env cond);
      ignore (check_block env expected_ret body); env
  | ForClassic (init_opt, cond_opt, post_opt, body) ->
      let loop_env =
        match init_opt with
        | None -> env
        | Some init_stmt -> check_stmt env expected_ret init_stmt
      in
      (match cond_opt with
       | None -> ()
       | Some cond -> expect_type loop_env ~context:"condición for" TBool (check_expr loop_env cond));
      (match post_opt with
       | None -> ()
       | Some post_stmt -> ignore (check_stmt loop_env expected_ret post_stmt));
      ignore (check_block loop_env expected_ret body);
      env
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
        | Some t, Some e -> expect_type env ~context:("var " ^ name) t (check_expr env e); t
        | None, None -> TNil
      in Env.extend name t env
  | TypeDecl (name, typ) ->
      env
      |> Env.extend (type_def_key name) typ
      |> Env.extend name (TName name)

let predeclare env = function
  | FuncDecl { name; params; ret; _ } ->
      Env.extend name (TFunc (List.map snd params, ret)) env
  | TypeDecl (name, typ) ->
      env
      |> Env.extend (type_def_key name) typ
      |> Env.extend name (TName name)
  | VarDecl (name, typ_opt, expr_opt) ->
      (* For predeclare, we can only infer literals or use explicit types *)
      let t = match typ_opt, expr_opt with
        | Some t, _ -> t
        | None, Some (Lit (IntLit _)) -> TInt
        | None, Some (Lit (FloatLit _)) -> TFloat64
        | None, Some (Lit (StringLit _)) -> TString
        | None, Some (Lit (BoolLit _)) -> TBool
        | _ -> TAny
      in Env.extend name t env

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
    let env_predeclared = List.fold_left predeclare env0 prog.decls in
    (* Use env_predeclared for everything *)
    let _ = List.iter (fun d -> ignore (check_decl env_predeclared d)) prog.decls in
    Ok prog
  with TypeError e -> Error (pp_error e)
