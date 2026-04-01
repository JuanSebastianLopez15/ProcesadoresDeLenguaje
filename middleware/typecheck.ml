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
let rec pp_typ = function
  | TInt     -> "int"
  | TFloat64 -> "float64"
  | TString  -> "string"
  | TBool    -> "bool"
  | TNil     -> "nil"
  | TVoid    -> "void"
  | TAny     -> "any"
  | TSlice t -> "[]" ^ pp_typ t
  | TMap (k, v) -> "map[" ^ pp_typ k ^ "]" ^ pp_typ v
  | TFunc (params, []) ->
      let ps = String.concat ", " (List.map pp_typ params) in
      Printf.sprintf "func(%s)" ps
  | TFunc (params, rets) ->
      let ps = String.concat ", " (List.map pp_typ params) in
      let rs = String.concat ", " (List.map pp_typ rets) in
      Printf.sprintf "func(%s) (%s)" ps rs

let pp_error = function
  | UndeclaredVar x ->
      Printf.sprintf "variable no declarada: '%s'" x
  | TypeMismatch { expected; got; context } ->
      Printf.sprintf "tipo incorrecto en %s: se esperaba %s, se obtuvo %s"
        context (pp_typ expected) (pp_typ got)
  | WrongArgCount { func_name; expected; got } ->
      Printf.sprintf "'%s' espera %d argumentos, se pasaron %d"
        func_name expected got
  | UndeclaredFunc f ->
      Printf.sprintf "función no declarada: '%s'" f
  | ReturnTypeMismatch _ ->
      "tipo de retorno no coincide con la firma de la función"
  | NotCallable x ->
      Printf.sprintf "'%s' no es una función" x
  | VoidUsedAsValue context ->
      Printf.sprintf "expresión de tipo void usada como valor en '%s'" context


(* ── Helpers ─────────────────────────────────────────── *)

let expect_type ~context expected got =
  if expected <> got
  then raise (TypeError (TypeMismatch { expected; got; context }))

let lookup_or_fail env name =
  match Env.lookup name env with
  | Some t -> t
  | None   -> raise (TypeError (UndeclaredVar name))

(* Rechaza TVoid donde se requiere un valor concreto *)
let reject_void ~context t =
  match t with
  | TVoid -> raise (TypeError (VoidUsedAsValue context))
  | _ -> ()


(* ── Inferencia de expresiones ───────────────────────── *)
(* check_expr : Env.t -> expr -> typ                      *)

let rec check_expr env = function

  (* Literales — tipos triviales *)
  | Lit (IntLit _)    -> TInt
  | Lit (FloatLit _)  -> TFloat64
  | Lit (StringLit _) -> TString
  | Lit (BoolLit _)   -> TBool
  | Lit NilLit        -> TNil

  (* Variable: buscar en el entorno *)
  | Var x -> lookup_or_fail env x

  (* Operaciones binarias aritméticas: int op int → int *)
  | BinOp ((Add|Sub|Mul|Div|Mod), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type ~context:"operación aritmética" tl tr;
      (match tl with
       | TInt | TFloat64 -> tl
       | _ -> raise (TypeError (TypeMismatch
           { expected = TInt; got = tl; context = "operación aritmética" })))

  (* Comparaciones: a op b → bool (requieren mismo tipo) *)
  | BinOp ((Eq|Neq|Lt|Gt|Leq|Geq), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type ~context:"comparación" tl tr;
      TBool

  (* Lógicos: bool op bool → bool *)
  | BinOp ((And|Or), l, r) ->
      expect_type ~context:"and/or" TBool (check_expr env l);
      expect_type ~context:"and/or" TBool (check_expr env r);
      TBool

  (* Negación *)
  | UnOp (Not, e) ->
      expect_type ~context:"not" TBool (check_expr env e);
      TBool
  | UnOp (Neg, e) ->
      let t = check_expr env e in
      (match t with TInt | TFloat64 -> t
       | _ -> raise (TypeError (TypeMismatch
           { expected = TInt; got = t; context = "negación" })))

  (* ++ / --  solo sobre int *)
  | UnOp ((Inc|Dec), e) ->
      expect_type ~context:"inc/dec" TInt (check_expr env e); TInt

  (* Llamada a función *)
  | Call (name, args) ->
      (match Env.lookup name env with
       | None -> raise (TypeError (UndeclaredFunc name))
       | Some (TFunc (param_types, ret_types)) ->
           (* Verificar aridad *)
           let n_expected = List.length param_types
           and n_got      = List.length args in
           if n_expected <> n_got
           then raise (TypeError (WrongArgCount
               { func_name = name; expected = n_expected; got = n_got }));
           (* Verificar tipo de cada argumento *)
           List.iter2 (fun expected_t arg ->
             let got_t = check_expr env arg in
             reject_void ~context:("argumento de " ^ name) got_t;
             expect_type ~context:("argumento de " ^ name) expected_t got_t
           ) param_types args;
           (* Retornar primer tipo (múltiples retornos → tupla, simplificado) *)
           (match ret_types with
            | []  -> TVoid
            | [t] -> t
            | ts  -> TFunc ([], ts))   (* múltiples retornos *)
      | Some _ -> raise (TypeError (NotCallable name)))

  (* fmt.Println y similares — aceptan cualquier tipo *)
  | MethodCall (Var "fmt", ("Println"|"Printf"|"Print"), _) -> TVoid

  | MethodCall (obj, _, _) ->
      let _ = check_expr env obj in TVoid  (* simplificado *)

  | Index (arr, idx) ->
      expect_type ~context:"índice" TInt (check_expr env idx);
      (match check_expr env arr with
       | TSlice t -> t
       | t -> raise (TypeError (TypeMismatch
           { expected = TSlice TAny; got = t; context = "indexación" })))

  | Selector (e, _) ->
      let _ = check_expr env e in TAny  (* campos de struct: simplificado *)


(* ── Verificación de statements ──────────────────────── *)
(* check_stmt : Env.t -> stmt -> Env.t                   *)
(* Devuelve el entorno extendido con nuevas variables     *)

and check_stmt env expected_ret = function

  (* contador := 1  →  extiende el env con contador:int *)
  | ShortDecl (name, expr) ->
      let t = check_expr env expr in
      reject_void ~context:("declaración ':=' de " ^ name) t;
      Env.extend name t env

  (* a = b  →  verifica que los tipos coincidan *)
  | Assign ([lhs], [rhs]) ->
      let tl = check_expr env lhs in
      let tr = check_expr env rhs in
      reject_void ~context:"lado derecho de asignación" tr;
      expect_type ~context:"asignación" tl tr;
      env   (* asignación no extiende el entorno *)

  (* return expr *)
  | Return exprs ->
      let got = List.map (check_expr env) exprs in
      (match expected_ret, got with
       | [], [] -> ()
       | [et], [gt] -> expect_type ~context:"return" et gt
       | _ -> if expected_ret <> got
              then raise (TypeError (ReturnTypeMismatch
                  { expected = expected_ret; got })));
      env

  (* if cond { } else { } *)
  | If (cond, then_block, else_opt) ->
      expect_type ~context:"condición if" TBool (check_expr env cond);
      let _ = check_block env expected_ret then_block in
      (match else_opt with
       | Some b -> let _ = check_block env expected_ret b in ()
       | None   -> ());
      env

  (* for cond { }  ← el while de Go *)
  | ForCond (cond, body) ->
      expect_type ~context:"condición for" TBool (check_expr env cond);
      let _ = check_block env expected_ret body in
      env

  (* for k, v := range slice { } *)
  | ForRange (k, v, collection, body) ->
      let elem_t = match check_expr env collection with
        | TSlice t -> t
        | TMap (_, vt) -> vt   (* simplificado *)
        | t -> raise (TypeError (TypeMismatch
            { expected = TSlice TAny; got = t; context = "range" }))
      in
      let inner_env = env
        |> Env.extend k TInt      (* índice siempre int *)
        |> Env.extend v elem_t in
      let _ = check_block inner_env expected_ret body in
      env

  | ExprStmt e ->
      let _ = check_expr env e in env

  | Defer e | Go e ->
      let _ = check_expr env e in env

  | _ -> env   (* casos no cubiertos: pasar *)


(* ── Verificación de bloques ─────────────────────────── *)

and check_block env expected_ret stmts =
  (* fold: cada stmt puede extender el env para el siguiente *)
  List.fold_left (fun env stmt ->
    check_stmt env expected_ret stmt
  ) env stmts


(* ── Verificación de declaraciones ───────────────────── *)

let check_decl env = function

  | FuncDecl { name; params; ret; body } ->
      (* 1. Registrar la función en el env global *)
      let func_typ = TFunc (List.map snd params, ret) in
      let env' = Env.extend name func_typ env in
      (* 2. Crear env local con los parámetros *)
      let local_env = List.fold_left
        (fun e (pname, ptyp) -> Env.extend pname ptyp e)
        env' params
      in
      (* 3. Verificar el cuerpo contra el tipo de retorno *)
      let _ = check_block local_env ret body in
      env'   (* devolver env global extendido con la función *)

  | VarDecl (name, typ_opt, expr_opt) ->
      let t = match typ_opt, expr_opt with
        | Some t, None   -> t
        | None,   Some e -> check_expr env e
        | Some t, Some e ->
            expect_type ~context:("var " ^ name) t (check_expr env e); t
        | None, None -> TNil
      in
      Env.extend name t env



(* ── Punto de entrada ────────────────────────────────── *)

let check_program (prog : program) : (program, string) result =
  try
    let env0 = Env.base in
    let _final_env = List.fold_left check_decl env0 prog.decls in
    Ok prog          (* programa bien tipado, devolver AST sin cambios *)
  with
  | TypeError e -> Error (pp_error e)