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

let test_keywords_y_operadores_extendidos _ =
  let codigo = "var ok bool = true && !false; x--" in
  let esperado =
    [ VAR;
      IDENT "ok";
      IDENT "bool";
      ASSIGN;
      TRUE;
      AND_AND;
      BANG;
      FALSE;
      SEMICOLON;
      IDENT "x";
      DEC;
      EOF ]
  in
  assert_equal esperado (tokenize_string codigo)

let test_inserta_semicolon_por_newline _ =
  let codigo = "return\nx := 1\n" in
  let esperado = [RETURN; SEMICOLON; IDENT "x"; DECL_ASSIGN; INTLIT 1; SEMICOLON; EOF] in
  assert_equal esperado (tokenize_string codigo)

let test_no_inserta_semicolon_tras_operador _ =
  let codigo = "x =\n1\n" in
  let esperado = [IDENT "x"; ASSIGN; INTLIT 1; SEMICOLON; EOF] in
  assert_equal esperado (tokenize_string codigo)

let suite =
  "Suite de pruebas del Lexer" >::: [
    "test_basico" >:: test_basico;
    "test_simbolos" >:: test_simbolos;
    "test_selector_con_punto" >:: test_selector_con_punto;
    "test_keywords_y_operadores_extendidos" >:: test_keywords_y_operadores_extendidos;
    "test_inserta_semicolon_por_newline" >:: test_inserta_semicolon_por_newline;
    "test_no_inserta_semicolon_tras_operador" >:: test_no_inserta_semicolon_tras_operador;
  ]

let () =
  run_test_tt_main suite

(*simula el codigo que viene de go, es una prueba de lopez*)