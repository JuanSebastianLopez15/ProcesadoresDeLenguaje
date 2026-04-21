open Lib.Token
open Lib.Ast

module Lexer = Frontend.Lexer
module Parser = Frontend.Parser
module Typecheck = Go_to_ocaml_middle.Typecheck

(* Se prueban varias rutas para ejecutar desde raiz o desde test/e2e. *)
let source_candidates =
  [
    "Source Code/sufrimiento_go.txt";
    "../Source Code/sufrimiento_go.txt";
    "../../Source Code/sufrimiento_go.txt";
  ]

let resolve_source_path () =
  match List.find_opt Sys.file_exists source_candidates with
  | Some path -> path
  | None ->
      failwith
        "No se encontro sufrimiento_go.txt. Revisar ruta en source_candidates."

let read_file path =
  let ch = open_in_bin path in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  data

let token_to_string = function
  | PACKAGE -> "PACKAGE"
  | IMPORT -> "IMPORT"
  | FUNC -> "FUNC"
  | FOR -> "FOR"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | VAR -> "VAR"
  | CONST -> "CONST"
  | DEFER -> "DEFER"
  | GO -> "GO"
  | SWITCH -> "SWITCH"
  | CASE -> "CASE"
  | DEFAULT -> "DEFAULT"
  | RANGE -> "RANGE"
  | TYPE -> "TYPE"
  | STRUCT -> "STRUCT"
  | MAP -> "MAP"
  | INTERFACE -> "INTERFACE"
  | CHAN -> "CHAN"
  | MAKE -> "MAKE"
  | NEW -> "NEW"
  | DELETE -> "DELETE"
  | REAL -> "REAL"
  | IMAG -> "IMAG"
  | COMPLEX -> "COMPLEX"
  | COPY -> "COPY"
  | RECOVER -> "RECOVER"
  | PANIC -> "PANIC"
  | APPEND -> "APPEND"
  | CAP -> "CAP"
  | LEN -> "LEN"
  | CLOSE -> "CLOSE"
  | IOTA -> "IOTA"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | NIL -> "NIL"
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | STRUCT_ID s -> Printf.sprintf "STRUCT_ID(%s)" s
  | INTLIT n -> Printf.sprintf "INTLIT(%Ld)" n
  | RUNELIT n -> Printf.sprintf "RUNELIT(%d)" n
  | FLOATLIT f -> Printf.sprintf "FLOATLIT(%g)" f
  | STRINGLIT s -> Printf.sprintf "STRINGLIT(%s)" s
  | ASSIGN -> "ASSIGN"
  | DECL_ASSIGN -> "DECL_ASSIGN"
  | COLON -> "COLON"
  | ELLIPSIS -> "ELLIPSIS"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | MOD -> "MOD"
  | AMP -> "AMP"
  | PIPE -> "PIPE"
  | CARET -> "CARET"
  | SHL -> "SHL"
  | SHR -> "SHR"
  | ARROW -> "ARROW"
  | EQ_EQ -> "EQ_EQ"
  | NOT_EQ -> "NOT_EQ"
  | LT -> "LT"
  | GT -> "GT"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | AND_AND -> "AND_AND"
  | OR_OR -> "OR_OR"
  | BANG -> "BANG"
  | INC -> "INC"
  | DEC -> "DEC"
  | PLUS_ASSIGN -> "PLUS_ASSIGN"
  | MINUS_ASSIGN -> "MINUS_ASSIGN"
  | STAR_ASSIGN -> "STAR_ASSIGN"
  | SLASH_ASSIGN -> "SLASH_ASSIGN"
  | MOD_ASSIGN -> "MOD_ASSIGN"
  | AMP_ASSIGN -> "AMP_ASSIGN"
  | PIPE_ASSIGN -> "PIPE_ASSIGN"
  | CARET_ASSIGN -> "CARET_ASSIGN"
  | SHL_ASSIGN -> "SHL_ASSIGN"
  | SHR_ASSIGN -> "SHR_ASSIGN"
  | AND_NOT -> "AND_NOT"
  | AND_NOT_ASSIGN -> "AND_NOT_ASSIGN"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | SEMICOLON -> "SEMICOLON"
  | EOF -> "EOF"

(* Lexer frontal: convierte fuente en una secuencia de tokens. *)
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

let print_tokens tokens =
  Printf.printf "\n== FRONTEND / LEXER TOKENS ==\n";
  List.iteri
    (fun i t -> Printf.printf "%03d  %s\n" (i + 1) (token_to_string t))
    tokens

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

let indent n = String.make (2 * n) ' '

let string_of_typ = function
  | TInt -> "TInt"
  | TFloat64 -> "TFloat64"
  | TString -> "TString"
  | TBool -> "TBool"
  | TNil -> "TNil"
  | TVoid -> "TVoid"
  | TAny -> "TAny"
  | TSlice _ -> "TSlice(...)"
  | TMap _ -> "TMap(...)"
  | TFunc _ -> "TFunc(...)"
  | TName n -> Printf.sprintf "TName(%s)" n      (* NUEVO *)
  | TStruct _ -> "TStruct(...)"                  (* NUEVO *)

let string_of_literal = function
  | IntLit n -> Printf.sprintf "IntLit(%Ld)" n
  | RuneLit n -> Printf.sprintf "RuneLit(%d)" n
  | FloatLit f -> Printf.sprintf "FloatLit(%f)" f
  | StringLit s -> Printf.sprintf "StringLit(%S)" s
  | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
  | NilLit -> "NilLit"

let string_of_binop = function
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod"
  | Eq -> "Eq" | Neq -> "Neq" | Lt -> "Lt" | Gt -> "Gt"
  | Leq -> "Leq" | Geq -> "Geq" | And -> "And" | Or -> "Or"
  | BAnd -> "BAnd" | BOr -> "BOr" | BXor -> "BXor"
  | Shl -> "Shl" | Shr -> "Shr" | AndNot -> "AndNot"

let string_of_unop = function
  | Not -> "Not"
  | Neg -> "Neg"
  | Inc -> "Inc"
  | Dec -> "Dec"
  | AddrOf -> "AddrOf"
  | Deref -> "Deref"

let rec string_of_expr = function
  | Lit lit -> Printf.sprintf "Lit(%s)" (string_of_literal lit)
  | Var v -> Printf.sprintf "Var(%s)" v
  | BinOp (op, l, r) ->
      Printf.sprintf "BinOp(%s, %s, %s)" (string_of_binop op) (string_of_expr l)
        (string_of_expr r)
  | UnOp (op, e) ->
      Printf.sprintf "UnOp(%s, %s)" (string_of_unop op) (string_of_expr e)
  | Call (e, args) ->
      Printf.sprintf "Call(%s, [%s])" (string_of_expr e)
        (String.concat "; " (List.map string_of_expr args))
  | MethodCall (obj, name, args) ->
      Printf.sprintf "MethodCall(%s, %s, [%s])" (string_of_expr obj) name
        (String.concat "; " (List.map string_of_expr args))
  | Index (arr, idx) ->
      Printf.sprintf "Index(%s, %s)" (string_of_expr arr) (string_of_expr idx)
  | Slice (arr, low, high, max) ->
      let s_opt = function Some e -> string_of_expr e | None -> "" in
      Printf.sprintf "Slice(%s, %s, %s, %s)" (string_of_expr arr) (s_opt low) (s_opt high) (s_opt max)
  | Selector (e, field) ->
      Printf.sprintf "Selector(%s, %s)" (string_of_expr e) field
  | StructLit (t_name, args) ->
      let s_arg (k_opt, e) = match k_opt with Some k -> k ^ ":" ^ string_of_expr e | None -> string_of_expr e in
      Printf.sprintf "StructLit(%s, [%s])" t_name
        (String.concat "; " (List.map s_arg args))
  | SliceLit (t, args) ->
      let s_arg (k_opt, e) = match k_opt with Some k -> k ^ ":" ^ string_of_expr e | None -> string_of_expr e in
      Printf.sprintf "SliceLit(%s, [%s])" (string_of_typ t)
        (String.concat "; " (List.map s_arg args))
  | Spread e ->
      Printf.sprintf "Spread(%s)" (string_of_expr e)
  | Cast (t, e) ->
      Printf.sprintf "Cast(%s, %s)" (string_of_typ t) (string_of_expr e)
  | KeyedExpr (k, e) ->
      Printf.sprintf "KeyedExpr(%s, %s)" k (string_of_expr e)
  | FuncLit fd ->
      Printf.sprintf "FuncLit(%s)" fd.name


let rec string_of_stmt level = function
  | Assign (lhs, rhs) ->
      Printf.sprintf "%sAssign([%s], [%s])" (indent level)
        (String.concat "; " (List.map string_of_expr lhs))
        (String.concat "; " (List.map string_of_expr rhs))
  | MultiAssign (lhs, rhs) ->
      Printf.sprintf "%sMultiAssign([%s], [%s])" (indent level)
        (String.concat "; " (List.map string_of_expr lhs))
        (String.concat "; " (List.map string_of_expr rhs))
  | ShortDecl (names, exprs) ->
      Printf.sprintf "%sShortDecl([%s], [%s])" (indent level)
        (String.concat "; " names)
        (String.concat "; " (List.map string_of_expr exprs))
  | If (cond, then_block, else_opt) ->
      let then_s = String.concat "\n" (List.map (string_of_stmt (level + 1)) then_block) in
      let else_s =
        match else_opt with
        | None -> ""
        | Some b ->
            Printf.sprintf "\n%selse:\n%s" (indent level)
              (String.concat "\n" (List.map (string_of_stmt (level + 1)) b))
      in
      Printf.sprintf "%sIf(%s)\n%s%s" (indent level) (string_of_expr cond) then_s else_s
  | IfInit (init_stmt, cond, then_block, else_opt) ->
      let init_s = string_of_stmt (level + 1) init_stmt in
      let then_s = String.concat "\n" (List.map (string_of_stmt (level + 1)) then_block) in
      let else_s =
        match else_opt with
        | None -> ""
        | Some b ->
            Printf.sprintf "\n%selse:\n%s" (indent level)
              (String.concat "\n" (List.map (string_of_stmt (level + 1)) b))
      in
      Printf.sprintf "%sIfInit(%s)\n%s\n%s%s" (indent level) (string_of_expr cond) init_s then_s else_s
  | ForCond (cond, body) ->
      Printf.sprintf "%sForCond(%s)\n%s" (indent level) (string_of_expr cond)
        (String.concat "\n" (List.map (string_of_stmt (level + 1)) body))
  | ForClassic (init_opt, cond_opt, post_opt, body) ->
      let init_s = match init_opt with None -> "None" | Some s -> string_of_stmt (level + 1) s in
      let cond_s = match cond_opt with None -> "None" | Some e -> string_of_expr e in
      let post_s = match post_opt with None -> "None" | Some s -> string_of_stmt (level + 1) s in
      Printf.sprintf "%sForClassic(init=%s, cond=%s, post=%s)\n%s" (indent level) init_s cond_s post_s
        (String.concat "\n" (List.map (string_of_stmt (level + 1)) body))
  | ForRange (k, v, coll, body) ->
      Printf.sprintf "%sForRange(%s, %s, %s)\n%s" (indent level) k v
        (string_of_expr coll)
        (String.concat "\n" (List.map (string_of_stmt (level + 1)) body))
  | Return exprs ->
      Printf.sprintf "%sReturn([%s])" (indent level)
        (String.concat "; " (List.map string_of_expr exprs))
  | ExprStmt e -> Printf.sprintf "%sExprStmt(%s)" (indent level) (string_of_expr e)
  | Defer e -> Printf.sprintf "%sDefer(%s)" (indent level) (string_of_expr e)
  | Go e -> Printf.sprintf "%sGo(%s)" (indent level) (string_of_expr e)
  | TypeSwitch (bind, target, cases, default_opt) ->
      let cases_s =
        cases
        |> List.map (fun (labels, body) ->
             let labels_s = String.concat ", " labels in
             let body_s = String.concat "\n" (List.map (string_of_stmt (level + 2)) body) in
             Printf.sprintf "%scase [%s]:\n%s" (indent (level + 1)) labels_s body_s)
        |> String.concat "\n"
      in
      let default_s =
        match default_opt with
        | None -> ""
        | Some body ->
            let body_s = String.concat "\n" (List.map (string_of_stmt (level + 2)) body) in
            Printf.sprintf "\n%sdefault:\n%s" (indent (level + 1)) body_s
      in
      Printf.sprintf "%sTypeSwitch(%s := %s.(type))\n%s%s"
        (indent level) bind (string_of_expr target) cases_s default_s

let string_of_decl = function
  | FuncDecl f ->
      let params_s =
        f.params
        |> List.map (fun (name, t) -> Printf.sprintf "%s:%s" name (string_of_typ t))
        |> String.concat ", "
      in
      let ret_s = String.concat ", " (List.map string_of_typ f.ret) in
      let body_s = String.concat "\n" (List.map (string_of_stmt 2) f.body) in
      Printf.sprintf "  FuncDecl %s(%s) -> [%s]\n%s" f.name params_s ret_s body_s
  | VarDecl (name, typ_opt, expr_opt) ->
      let typ_s =
        match typ_opt with
        | None -> "_"
        | Some t -> string_of_typ t
      in
      let expr_s =
        match expr_opt with
        | None -> "_"
        | Some e -> string_of_expr e
      in
      Printf.sprintf "  VarDecl %s : %s = %s" name typ_s expr_s
  | TypeDecl (name, t) ->                                    (* NUEVO *)
      Printf.sprintf "  TypeDecl %s = %s" name (string_of_typ t)

let string_of_program p =
  let imports_s = String.concat ", " p.imports in
  let decls_s = String.concat "\n" (List.map string_of_decl p.decls) in
  Printf.sprintf "Program(package=%s, imports=[%s])\n%s" p.package imports_s decls_s

let run_typecheck ast =
  (* Middleware: valida semanticamente el AST construido. *)
  Printf.printf "\n== MIDDLEWARE / TYPECHECK ==\n";
  match Typecheck.check_program ast with
  | Ok _ -> Printf.printf "Typecheck OK: el programa es valido.\n"
  | Error msg -> Printf.printf "Typecheck ERROR: %s\n" msg

let () =
  let source_path = resolve_source_path () in
  Printf.printf "E2E: procesamiento de archivo Go -> frontend -> middleware\n";
  Printf.printf "Archivo fuente: %s\n" source_path;
  let source = read_file source_path in
  Printf.printf "\n== FUENTE GO ==\n%s\n" source;

  (match tokenize source with
  | Ok tokens -> print_tokens tokens
  | Error msg ->
      Printf.printf "\n== FRONTEND / LEXER ==\n";
      Printf.printf "Lexer ERROR: %s\n" msg;
      Printf.printf "(La prueba continua para mostrar la integracion con middleware.)\n");

  match parse_program source with
  | Ok ast ->
      Printf.printf "\n== AST (PARSER) ==\n%s\n" (string_of_program ast);
      run_typecheck ast
  | Error msg ->
      Printf.printf "\n== FRONTEND / PARSER ==\n";
      Printf.printf "Parser ERROR: %s\n" msg
