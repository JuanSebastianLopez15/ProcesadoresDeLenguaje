open OUnit2
open Lib.Ast

module Middle = Go_to_ocaml_middle
module Env = Middle.Env
module Typecheck = Middle.Typecheck

let string_contains s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  len_sub = 0 || loop 0

let assert_type_error f =
  match f () with
  | exception Typecheck.TypeError e -> e
  | _ -> failwith "Se esperaba TypeError"

let assert_program_error_contains ~needle prog =
  match Typecheck.check_program prog with
  | Ok _ -> assert_failure "Se esperaba Error del typechecker"
  | Error msg ->
      if not (string_contains msg needle) then
        assert_failure
          (Printf.sprintf "Mensaje esperado con '%s', se obtuvo '%s'" needle msg)

let mk_func ?(params = []) ?(ret = []) ?(body = []) name =
  FuncDecl { name; params; ret; body }

let mk_program decls =
  {
    package = "main";
    imports = [];
    decls;
  }

let test_lit_int_type _ =
  assert_equal TInt (Typecheck.check_expr Env.empty (Lit (IntLit 7)))

let test_arith_type _ =
  let expr = BinOp (Add, Lit (IntLit 2), Lit (IntLit 3)) in
  assert_equal TInt (Typecheck.check_expr Env.empty expr)

let test_comparison_type _ =
  let env = Env.extend "x" TInt Env.empty in
  let expr = BinOp (Eq, Var "x", Lit (IntLit 10)) in
  assert_equal TBool (Typecheck.check_expr env expr)

let test_call_return_type _ =
  let env = Env.extend "doble" (TFunc ([ TInt ], [ TInt ])) Env.empty in
  let expr = Call ("doble", [ Lit (IntLit 4) ]) in
  assert_equal TInt (Typecheck.check_expr env expr)

let test_undeclared_var_exception _ =
  match assert_type_error (fun () -> Typecheck.check_expr Env.empty (Var "no_decl")) with
  | Typecheck.UndeclaredVar "no_decl" -> ()
  | _ -> assert_failure "Se esperaba UndeclaredVar"

let test_type_mismatch_exception _ =
  let expr = BinOp (Add, Lit (StringLit "hola"), Lit (IntLit 1)) in
  match assert_type_error (fun () -> Typecheck.check_expr Env.empty expr) with
  | Typecheck.TypeMismatch _ -> ()
  | _ -> assert_failure "Se esperaba TypeMismatch"

let test_wrong_arg_count_exception _ =
  let env = Env.extend "doble" (TFunc ([ TInt ], [ TInt ])) Env.empty in
  match assert_type_error (fun () -> Typecheck.check_expr env (Call ("doble", []))) with
  | Typecheck.WrongArgCount { func_name; expected; got } ->
      assert_equal "doble" func_name;
      assert_equal 1 expected;
      assert_equal 0 got
  | _ -> assert_failure "Se esperaba WrongArgCount"

let test_if_condition_error _ =
  let prog =
    mk_program
      [ mk_func "main" ~body:[ If (Lit (IntLit 1), [], None) ] ]
  in
  assert_program_error_contains ~needle:"condición if" prog

let test_return_type_error _ =
  let prog =
    mk_program
      [ mk_func "f" ~ret:[ TInt ] ~body:[ Return [ Lit (StringLit "no") ] ] ]
  in
  assert_program_error_contains ~needle:"tipo incorrecto en return" prog

let suite =
  "typecheck" >::: [
    "literal int -> TInt" >:: test_lit_int_type;
    "aritmetica -> TInt" >:: test_arith_type;
    "comparacion -> TBool" >:: test_comparison_type;
    "llamada funcion -> retorno" >:: test_call_return_type;
    "excepcion variable no declarada" >:: test_undeclared_var_exception;
    "excepcion type mismatch" >:: test_type_mismatch_exception;
    "excepcion aridad incorrecta" >:: test_wrong_arg_count_exception;
    "error programa condicion if" >:: test_if_condition_error;
    "error programa return" >:: test_return_type_error;
  ]

let () = run_test_tt_main suite
