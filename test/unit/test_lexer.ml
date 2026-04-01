open OUnit2
open Lib.Token
open Frontend.Lexer

let tokenize_string str =
  let lexbuf = Lexing.from_string str in
  let rec loop acc =
    match read lexbuf with
    | EOF -> List.rev (EOF :: acc)
    | token -> loop (token :: acc)
  in
  loop []

let test_basico _ =
  let codigo = "package main" in
  let esperado = [PACKAGE; IDENT "main"; EOF] in
  assert_equal esperado (tokenize_string codigo)

let test_simbolos _ =
  let codigo = "contador := 1" in
  let esperado = [IDENT "contador"; DECL_ASSIGN; INTLIT 1; EOF] in
  assert_equal esperado (tokenize_string codigo)

let test_selector_con_punto _ =
  let codigo = "fmt.Println(\"ok\")" in
  let esperado =
    [IDENT "fmt"; DOT; IDENT "Println"; LPAREN; STRINGLIT "ok"; RPAREN; EOF]
  in
  assert_equal esperado (tokenize_string codigo)

let suite =
  "Suite de pruebas del Lexer" >::: [
    "test_basico" >:: test_basico;
    "test_simbolos" >:: test_simbolos;
    "test_selector_con_punto" >:: test_selector_con_punto;
  ]

let () =
  run_test_tt_main suite

(*simula el codigo que viene de go, es una prueba de lopez*)