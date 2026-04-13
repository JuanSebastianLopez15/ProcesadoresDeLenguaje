open OUnit2
open Lib.Ast

module Parser = Frontend.Parser
module Lexer = Frontend.Lexer

let parse_program source =
  let lexbuf = Lexing.from_string source in
  Parser.program Lexer.read lexbuf

let test_parse_main_vacio _ =
  let source =
    "package main\n\nfunc main() {\n}\n"
  in
  let got = parse_program source in
  let expected =
    {
      package = "main";
      imports = [];
      decls = [ FuncDecl { name = "main"; params = []; ret = []; body = [] } ];
    }
  in
  assert_equal expected got

let test_parse_if_else_con_llamadas _ =
  let source =
    "package main\n\nimport \"fmt\"\n\nfunc main() {\n  x := 0\n  if x == 0 {\n    fmt.Println(\"ok\")\n  } else {\n    fmt.Println(\"no\")\n  }\n}\n"
  in
  let got = parse_program source in
  let expected =
    {
      package = "main";
      imports = [ "fmt" ];
      decls =
        [
          FuncDecl
            {
              name = "main";
              params = [];
              ret = [];
              body =
                [ ShortDecl ("x", Lit (IntLit 0));
                  If
                    ( BinOp (Eq, Var "x", Lit (IntLit 0)),
                      [ ExprStmt
                          (MethodCall
                             (Var "fmt", "Println", [ Lit (StringLit "ok") ])) ],
                      Some
                        [ ExprStmt
                            (MethodCall
                               (Var "fmt", "Println", [ Lit (StringLit "no") ])) ] );
                ];
            };
        ];
    }
  in
  assert_equal expected got

let test_parse_var_y_for_range _ =
  let source =
    "package main;\n\nimport \"fmt\";\n\nvar datos []int;\nvar bandera bool = true;\n\nfunc main() {\n  for i, v := range datos {\n    fmt.Println(i, v);\n  };\n}\n"
  in
  let got = parse_program source in
  let expected =
    {
      package = "main";
      imports = [ "fmt" ];
      decls =
        [ VarDecl ("datos", Some (TSlice TInt), None);
          VarDecl ("bandera", Some TBool, Some (Lit (BoolLit true)));
          FuncDecl
            {
              name = "main";
              params = [];
              ret = [];
              body =
                [ ForRange
                    ( "i",
                      "v",
                      Var "datos",
                      [ ExprStmt
                          (MethodCall (Var "fmt", "Println", [ Var "i"; Var "v" ])) ] );
                ];
            };
        ];
    }
  in
  assert_equal expected got

let test_parse_precedencia_y_unarios _ =
  let source =
    "package main\n\nfunc main() {\n  if !(1 + 2 * 3 == 7) || false {\n    x := 1\n  }\n}\n"
  in
  let got = parse_program source in
  let expected_cond =
    BinOp
      ( Or,
        UnOp
          ( Not,
            BinOp
              ( Eq,
                BinOp (Add, Lit (IntLit 1), BinOp (Mul, Lit (IntLit 2), Lit (IntLit 3))),
                Lit (IntLit 7) ) ),
        Lit (BoolLit false) )
  in
  let expected =
    {
      package = "main";
      imports = [];
      decls =
        [ FuncDecl
            {
              name = "main";
              params = [];
              ret = [];
              body = [ If (expected_cond, [ ShortDecl ("x", Lit (IntLit 1)) ], None) ];
            };
        ];
    }
  in
  assert_equal expected got

let test_parse_return_vacio _ =
  let source =
    "package main\n\nfunc main() {\n  return\n}\n"
  in
  let got = parse_program source in
  let expected =
    {
      package = "main";
      imports = [];
      decls = [ FuncDecl { name = "main"; params = []; ret = []; body = [ Return [] ] } ];
    }
  in
  assert_equal expected got

let suite =
  "parser" >::: [
    "parsea main vacio" >:: test_parse_main_vacio;
    "parsea if else con llamadas" >:: test_parse_if_else_con_llamadas;
    "parsea var y for range" >:: test_parse_var_y_for_range;
    "parsea precedencia y unarios" >:: test_parse_precedencia_y_unarios;
    "parsea return vacio" >:: test_parse_return_vacio;
  ]

let () = run_test_tt_main suite