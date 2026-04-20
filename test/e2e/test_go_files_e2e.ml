open OUnit2
open Lib.Token
open Lib.Ast

module Lexer = Frontend.Lexer
module Parser = Frontend.Parser
module Typecheck = Go_to_ocaml_middle.Typecheck
module Report = E2e_report

(* E2E real por fixture: fuente -> lexer -> parser -> AST -> typecheck. *)

(* Expectativa de resultado semantico por fixture. *)
type expected_typecheck =
  | Should_pass
  | Should_fail_with of string

type fixture = {
  file_name : string;
  expected : expected_typecheck;
  ast : program;
}

(* Se prueban varias rutas porque Dune ejecuta tests desde _build. *)
let source_dir_candidates =
  [
    "Source Code/tests";
    "../Source Code/tests";
    "../../Source Code/tests";
    "../../../Source Code/tests";
    "../../../../Source Code/tests";
  ]

let resolve_source_dir () =
  match List.find_opt Sys.file_exists source_dir_candidates with
  | Some path -> path
  | None -> failwith "No se encontro carpeta Source Code/tests"

let read_file path =
  let ch = open_in_bin path in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  data

let string_contains s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  len_sub = 0 || loop 0

let tokenize source =
  let lexbuf = Lexing.from_string source in
  let rec loop acc =
    try
      match Lexer.read lexbuf with
      | EOF -> Ok (List.rev (EOF :: acc))
      | t -> loop (t :: acc)
    with
    | Lexer.SyntaxError msg -> Error msg
  in
  loop []

let parse_program source =
  let lexbuf = Lexing.from_string source in
  try Ok (Parser.program Lexer.read lexbuf)
  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      Error
        (Printf.sprintf "error sintactico en linea %d, columna %d" pos.pos_lnum col)
  | Failure msg -> Error msg

(* Ejecuta pipeline completo por fixture: lectura -> lexer -> parser -> typecheck. *)
let run_fixture base_dir fx _ =
  let path = Filename.concat base_dir fx.file_name in
  let source = read_file path in

  let lexer_outcome =
    match tokenize source with
    | Ok tokens -> Report.Lexer_ok tokens
    | Error msg -> Report.Lexer_error msg
  in
  let parser_outcome = parse_program source in
  let parser_report_outcome =
    match parser_outcome with
    | Ok _ -> Report.Parser_ok
    | Error msg -> Report.Parser_error msg
  in
  let typecheck_outcome =
    match parser_outcome with
    | Ok ast ->
        assert_equal ~msg:(Printf.sprintf "AST inesperado en %s" fx.file_name) fx.ast ast;
        (match Typecheck.check_program ast with
        | Ok _ -> Report.Typecheck_ok
        | Error msg -> Report.Typecheck_error msg)
    | Error msg -> Report.Typecheck_error ("parser: " ^ msg)
  in

  Report.print_summary ~file_name:fx.file_name ~lexer_outcome
    ~parser_outcome:parser_report_outcome ~typecheck_outcome ();

  let tokens =
    match lexer_outcome with
    | Report.Lexer_ok ts -> ts
    | Report.Lexer_error msg ->
        assert_failure
          (Printf.sprintf "Lexer fallo en %s: %s" fx.file_name msg)
  in
  assert_bool
    (Printf.sprintf "No se encontro EOF en %s" fx.file_name)
    (List.exists (function EOF -> true | _ -> false) tokens);

  (match parser_outcome with
  | Ok _ -> ()
  | Error msg ->
      assert_failure
        (Printf.sprintf "Parser fallo en %s: %s" fx.file_name msg));

  match (fx.expected, typecheck_outcome) with
  | Should_pass, Report.Typecheck_ok -> ()
  | Should_pass, Report.Typecheck_error msg ->
      assert_failure
        (Printf.sprintf "Se esperaba OK en %s, se obtuvo Error: %s" fx.file_name msg)
  | Should_fail_with needle, Report.Typecheck_error msg ->
      if not (string_contains msg needle) then
        assert_failure
          (Printf.sprintf
             "Error inesperado en %s. Esperaba contener '%s', obtuvo '%s'"
             fx.file_name needle msg)
  | Should_fail_with needle, Report.Typecheck_ok ->
      assert_failure
        (Printf.sprintf
           "Se esperaba error con '%s' en %s, pero paso sin errores"
           needle fx.file_name)

let fixtures =
  [
    (* Caso valido: loop con llamada a funcion y asignacion. *)
    {
      file_name = "ok_loop_go.txt";
      expected = Should_pass;
      ast =
        {
          package = "main";
          imports = [ "fmt" ];
          decls =
            [ FuncDecl
                {
                  name = "sumarUno";
                  params = [ ("x", TInt) ];
                  ret = [ TInt ];
                  body = [ Return [ BinOp (Add, Var "x", Lit (IntLit 1L)) ] ];
                };
              FuncDecl
                {
                  name = "main";
                  params = [];
                  ret = [];
                  body =
                    [ ShortDecl (["contador"], [Lit (IntLit 1L)]);
                      ForCond
                        ( BinOp (Leq, Var "contador", Lit (IntLit 3L)),
                          [ ExprStmt
                              (MethodCall
                                 (Var "fmt", "Println", [ Var "contador" ]));
                            Assign
                              ( [ Var "contador" ],
                                [ Call (Var "sumarUno", [ Var "contador" ]) ] );
                          ] );
                    ];
                };
            ];
        };
    };
    (* Caso invalido: condicion de if no booleana. *)
    {
      file_name = "error_if_no_bool_go.txt";
      expected = Should_fail_with "condición if";
      ast =
        {
          package = "main";
          imports = [];
          decls =
            [ FuncDecl
                {
                  name = "main";
                  params = [];
                  ret = [];
                  body = [ If (Lit (IntLit 1L), [], None) ];
                };
            ];
        };
    };
    (* Caso invalido: aridad incorrecta en llamada. *)
    {
      file_name = "error_call_arity_go.txt";
      expected = Should_fail_with "espera 1 argumentos";
      ast =
        {
          package = "main";
          imports = [];
          decls =
            [ FuncDecl
                {
                  name = "doble";
                  params = [ ("numero", TInt) ];
                  ret = [ TInt ];
                  body = [ Return [ BinOp (Mul, Var "numero", Lit (IntLit 2L)) ] ];
                };
              FuncDecl
                {
                  name = "main";
                  params = [];
                  ret = [];
                  body = [ ShortDecl (["resultado"], [Call (Var "doble", [])]) ];
                };
            ];
        };
    };
    (* Caso valido: if/else con comparacion booleana. *)
    {
      file_name = "ok_if_else_go.txt";
      expected = Should_pass;
      ast =
        {
          package = "main";
          imports = [ "fmt" ];
          decls =
            [ FuncDecl
                {
                  name = "main";
                  params = [];
                  ret = [];
                  body =
                    [ ShortDecl (["x"], [Lit (IntLit 0L)]);
                      If
                        ( BinOp (Eq, Var "x", Lit (IntLit 0L)),
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
        };
    };
    (* Caso valido: llamadas anidadas entre funciones. *)
    {
      file_name = "ok_nested_calls_go.txt";
      expected = Should_pass;
      ast =
        {
          package = "main";
          imports = [];
          decls =
            [ FuncDecl
                {
                  name = "doble";
                  params = [ ("numero", TInt) ];
                  ret = [ TInt ];
                  body = [ Return [ BinOp (Mul, Var "numero", Lit (IntLit 2L)) ] ];
                };
              FuncDecl
                {
                  name = "triple";
                  params = [ ("numero", TInt) ];
                  ret = [ TInt ];
                  body =
                    [ Return
                        [ BinOp
                            ( Add,
                              Call (Var "doble", [ Var "numero" ]),
                              Var "numero" ) ] ];
                };
              FuncDecl
                {
                  name = "main";
                  params = [];
                  ret = [];
                  body = [ ShortDecl (["resultado"], [Call (Var "triple", [ Lit (IntLit 3L) ])]) ];
                };
            ];
        };
    };
    (* Caso invalido: return con tipo incompatible. *)
    {
      file_name = "error_return_type_go.txt";
      expected = Should_fail_with "tipo incorrecto en return";
      ast =
        {
          package = "main";
          imports = [];
          decls =
            [ FuncDecl
                {
                  name = "malo";
                  params = [];
                  ret = [ TInt ];
                  body = [ Return [ Lit (StringLit "oops") ] ];
                };
              FuncDecl
                {
                  name = "main";
                  params = [];
                  ret = [];
                  body = [ ShortDecl (["x"], [Call (Var "malo", [])]) ];
                };
            ];
        };
    };
    (* Caso invalido: variable usada sin declaracion previa. *)
    {
      file_name = "error_undeclared_var_go.txt";
      expected = Should_fail_with "variable no declarada";
      ast =
        {
          package = "main";
          imports = [];
          decls =
            [ FuncDecl
                {
                  name = "main";
                  params = [];
                  ret = [];
                  body = [ ShortDecl (["resultado"], [Var "noDeclarada"]) ];
                };
            ];
        };
    };
  ]

let suite =
  let base_dir = resolve_source_dir () in
  "e2e_full_frontend_middleware"
  >::: List.map (fun fx -> fx.file_name >:: run_fixture base_dir fx) fixtures

let () = run_test_tt_main suite
