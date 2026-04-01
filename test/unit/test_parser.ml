open OUnit2

module Parser = Frontend.Parser

let test_dummy_accepts_eof _ =
  let lexer _ = Parser.EOF in
  let lexbuf = Lexing.from_string "" in
  assert_equal () (Parser.dummy lexer lexbuf)

let test_dummy_rejects_ident _ =
  let first = ref true in
  let lexer _ =
    if !first then (
      first := false;
      Parser.IDENT "x"
    ) else Parser.EOF
  in
  let lexbuf = Lexing.from_string "x" in
  assert_raises Parser.Error (fun () -> Parser.dummy lexer lexbuf)

let suite =
  "parser" >::: [
    "acepta EOF" >:: test_dummy_accepts_eof;
    "rechaza IDENT inicial" >:: test_dummy_rejects_ident;
  ]

let () = run_test_tt_main suite