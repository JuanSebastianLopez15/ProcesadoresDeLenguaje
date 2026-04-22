(* lib/typecheck.ml *)
open Ast

type error =
  | UnknownVar of string
  | UnknownFunc of string
  | UnknownStruct of string
  | UnknownField of string * string           (* struct_name * field *)
  | TypeMismatch of typ * typ * string        (* expected, got, context *)
  | ArgCountMismatch of string * int * int    (* func, expected, got *)
  | NotIndexable of typ
  | NotSelectable of typ

exception TypeError of error

let rec pp_type = function
  | TInt -> "int"
  | TFloat -> "float64"
  | TString -> "string"
  | TBool -> "bool"
  | TVoid -> "void"
  | TSlice t -> "[]" ^ pp_type t
  | TName n -> n

let pp_error = function
  | UnknownVar x -> Printf.sprintf "variable desconocida: '%s'" x
  | UnknownFunc f -> Printf.sprintf "función desconocida: '%s'" f
  | UnknownStruct s -> Printf.sprintf "struct desconocido: '%s'" s
  | UnknownField (s, f) -> Printf.sprintf "el struct '%s' no tiene campo '%s'" s f
  | TypeMismatch (e, g, ctx) ->
      Printf.sprintf "tipo incompatible en %s: se esperaba %s, se obtuvo %s"
        ctx (pp_type e) (pp_type g)
  | ArgCountMismatch (f, e, g) ->
      Printf.sprintf "'%s' espera %d argumentos, se pasaron %d" f e g
  | NotIndexable t ->
      Printf.sprintf "no se puede indexar un valor de tipo %s" (pp_type t)
  | NotSelectable t ->
      Printf.sprintf "no se puede acceder a campos de un valor de tipo %s" (pp_type t)

(* Igualdad de tipos, con TInt ≡ TInt, TFloat ≡ TFloat, etc. *)
let types_equal t1 t2 =
  let rec eq a b = match a, b with
    | TInt, TInt | TFloat, TFloat | TString, TString | TBool, TBool | TVoid, TVoid -> true
    | TSlice a, TSlice b -> eq a b
    | TName n1, TName n2 -> n1 = n2
    | _ -> false
  in eq t1 t2

let expect_type expected got ctx =
  if not (types_equal expected got) then
    raise (TypeError (TypeMismatch (expected, got, ctx)))

(* Builtins reconocidos con su signatura dependiente del argumento *)
let check_builtin_call env name args check_expr =
  match name with
  | "len" ->
      (match args with
       | [e] ->
           let t = check_expr env e in
           (match t with
            | TSlice _ | TString -> TInt
            | _ -> raise (TypeError (TypeMismatch (TSlice TInt, t, "len"))))
       | _ -> raise (TypeError (ArgCountMismatch ("len", 1, List.length args))))

  | "append" ->
      (match args with
       | [slice; elem] ->
           let ts = check_expr env slice in
           let te = check_expr env elem in
           (match ts with
            | TSlice inner ->
                expect_type inner te "append";
                ts
            | _ -> raise (TypeError (TypeMismatch (TSlice TInt, ts, "append"))))
       | _ -> raise (TypeError (ArgCountMismatch ("append", 2, List.length args))))

  | "int" | "int64" ->
      (match args with
       | [e] ->
           let t = check_expr env e in
           (match t with
            | TInt | TFloat -> TInt
            | _ -> raise (TypeError (TypeMismatch (TInt, t, "cast a int"))))
       | _ -> raise (TypeError (ArgCountMismatch (name, 1, List.length args))))

  | "float64" ->
      (match args with
       | [e] ->
           let t = check_expr env e in
           (match t with
            | TInt | TFloat -> TFloat
            | _ -> raise (TypeError (TypeMismatch (TFloat, t, "cast a float64"))))
       | _ -> raise (TypeError (ArgCountMismatch ("float64", 1, List.length args))))

  | "string" ->
      (match args with
       | [e] ->
           let t = check_expr env e in
           (match t with
            | TSlice _ | TInt | TString -> TString
            | _ -> raise (TypeError (TypeMismatch (TString, t, "cast a string"))))
       | _ -> raise (TypeError (ArgCountMismatch ("string", 1, List.length args))))

  | "rune" | "int32" ->
      (match args with
       | [e] ->
           let t = check_expr env e in
           (match t with
            | TInt | TFloat -> TInt
            | _ -> raise (TypeError (TypeMismatch (TInt, t, "cast a rune"))))
       | _ -> raise (TypeError (ArgCountMismatch (name, 1, List.length args))))

  | "fmt.Println" | "fmt.Printf" | "fmt.Print" ->
      List.iter (fun a -> ignore (check_expr env a)) args;
      TVoid

  | _ -> raise (TypeError (UnknownFunc name))

let is_builtin = function
  | "len" | "append" | "int" | "int64" | "float64" | "string" | "rune" | "int32"
  | "fmt.Println" | "fmt.Printf" | "fmt.Print" -> true
  | _ -> false

let rec check_expr env = function
  | Lit (IntLit _) -> TInt
  | Lit (FloatLit _) -> TFloat
  | Lit (StringLit _) -> TString
  | Lit (BoolLit _) -> TBool

  | Var x ->
      (match Env.lookup_var x env with
       | Some t -> t
       | None -> raise (TypeError (UnknownVar x)))

  | BinOp ((Add|Sub|Mul|Div|Mod) as op, l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type tl tr "operación aritmética";
      (match tl with
       | TInt | TFloat -> tl
       | TString when op = Add -> TString  (* concatenación *)
       | _ -> raise (TypeError (TypeMismatch (TInt, tl, "aritmética"))))

  | BinOp ((Eq|Neq|Lt|Gt|Leq|Geq), l, r) ->
      let tl = check_expr env l in
      let tr = check_expr env r in
      expect_type tl tr "comparación";
      TBool

  | BinOp ((And|Or), l, r) ->
      expect_type TBool (check_expr env l) "and/or (izquierdo)";
      expect_type TBool (check_expr env r) "and/or (derecho)";
      TBool

  | UnOp (Not, e) ->
      expect_type TBool (check_expr env e) "negación lógica";
      TBool

  | UnOp (Neg, e) ->
      let t = check_expr env e in
      (match t with
       | TInt | TFloat -> t
       | _ -> raise (TypeError (TypeMismatch (TInt, t, "negación numérica"))))

  | Call (name, args) when is_builtin name ->
      check_builtin_call env name args check_expr

  | Call (name, args) ->
      (match Env.lookup_func name env with
       | Some (param_types, ret_type) ->
           let n_expected = List.length param_types in
           let n_got = List.length args in
           if n_expected <> n_got then
             raise (TypeError (ArgCountMismatch (name, n_expected, n_got)));
           List.iter2 (fun pt arg ->
             let at = check_expr env arg in
             expect_type pt at ("argumento de " ^ name)
           ) param_types args;
           ret_type
       | None -> raise (TypeError (UnknownFunc name)))

  | Index (arr, idx) ->
      let at = check_expr env arr in
      let it = check_expr env idx in
      expect_type TInt it "índice";
      (match at with
       | TSlice inner -> inner
       | _ -> raise (TypeError (NotIndexable at)))

  | Selector (e, field) ->
      let t = check_expr env e in
      (match t with
       | TName struct_name ->
           (match Env.lookup_struct struct_name env with
            | Some fields ->
                (match List.assoc_opt field fields with
                 | Some ft -> ft
                 | None -> raise (TypeError (UnknownField (struct_name, field))))
            | None -> raise (TypeError (UnknownStruct struct_name)))
       | _ -> raise (TypeError (NotSelectable t)))

  | StructLit (name, args) ->
      (match Env.lookup_struct name env with
       | Some fields ->
           let n_fields = List.length fields in
           let n_args = List.length args in
           if n_fields <> n_args then
             raise (TypeError (ArgCountMismatch (name, n_fields, n_args)));
           (* Verificar tipos — posicional o nominal *)
           List.iter2 (fun (fname, ftyp) (key_opt, value) ->
             let actual_name = match key_opt with Some k -> k | None -> fname in
             if actual_name <> fname && key_opt <> None then
               raise (TypeError (UnknownField (name, actual_name)));
             expect_type ftyp (check_expr env value)
               (Printf.sprintf "campo '%s' de %s" fname name)
           ) fields args;
           TName name
       | None -> raise (TypeError (UnknownStruct name)))

  | SliceLit (t, elems) ->
      List.iter (fun e ->
        let et = match e with
          | StructLit ("", args) ->
              (match t with
               | TName name -> check_expr env (StructLit (name, args))
               | _ -> check_expr env e)
          | _ -> check_expr env e
        in
        expect_type t et "elemento de slice"
      ) elems;
      TSlice t

  | Cast (target_t, e) ->
      let _ = check_expr env e in
      target_t

let rec check_stmt env expected_ret = function
  | ShortDecl (x, e) ->
      let t = check_expr env e in
      Env.add_var x t env

  | Assign (x, e) ->
      (match Env.lookup_var x env with
       | Some xt ->
           let et = check_expr env e in
           expect_type xt et ("reasignación de " ^ x);
           env
       | None -> raise (TypeError (UnknownVar x)))

  | If (cond, then_b, else_b) ->
      expect_type TBool (check_expr env cond) "condición de if";
      let _ = check_block env expected_ret then_b in
      Option.iter (fun b -> ignore (check_block env expected_ret b)) else_b;
      env

  | Return None ->
      expect_type TVoid expected_ret "return vacío";
      env

  | Return (Some e) ->
      let t = check_expr env e in
      expect_type expected_ret t "return";
      env

  | ExprStmt e ->
      ignore (check_expr env e);
      env

and check_block env expected_ret stmts =
  List.fold_left (fun e s -> check_stmt e expected_ret s) env stmts

(* Primer pass: registra todos los structs y firmas de funciones *)
let register_decls env decls =
  List.fold_left (fun env -> function
    | StructDecl { name; fields } -> Env.add_struct name fields env
    | FuncDecl { name; params; ret; _ } ->
        Env.add_func name (List.map snd params, ret) env
  ) env decls

(* Segundo pass: verifica cuerpos de funciones *)
let check_decl env = function
  | StructDecl _ -> env  (* ya registrado *)
  | FuncDecl fd ->
      let local_env = List.fold_left (fun e (n, t) -> Env.add_var n t e) env fd.params in
      let _ = check_block local_env fd.ret fd.body in
      env

let check_program prog =
  try
    let env0 = register_decls Env.empty prog.decls in
    let _ = List.fold_left check_decl env0 prog.decls in
    Ok prog
  with TypeError e -> Error (pp_error e)
