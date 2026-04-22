{
  open Parser
  type lex_error = {
    message : string;
    line : int;
    column : int;
    lexeme : string;
  }

  exception LexError of lex_error

  (* Inserción automática de punto y coma al estilo Go:
     cuando el token anterior puede terminar un statement y aparece
     un newline, se emite SEMICOLON. *)
  let last_ends_stmt = ref false
  let ends_stmt = function
    | IDENT _ | INTLIT _ | FLOATLIT _ | STRINGLIT _
    | TRUE | FALSE | RETURN
    | RPAREN | RBRACK | RBRACE -> true
    | _ -> false

  let trace_tokens =
    match Sys.getenv_opt "COMPILER_TRACE_TOKENS" with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false

  let token_to_string = function
    | PACKAGE -> "PACKAGE"
    | IMPORT -> "IMPORT"
    | FUNC -> "FUNC"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | RETURN -> "RETURN"
    | TYPE -> "TYPE"
    | STRUCT -> "STRUCT"
    | VAR -> "VAR"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | IDENT _ -> "IDENT"
    | INTLIT _ -> "INTLIT"
    | FLOATLIT _ -> "FLOATLIT"
    | STRINGLIT _ -> "STRINGLIT"
    | ASSIGN -> "ASSIGN"
    | DECL_ASSIGN -> "DECL_ASSIGN"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | STAR -> "STAR"
    | SLASH -> "SLASH"
    | PERCENT -> "PERCENT"
    | EQ -> "EQ"
    | NEQ -> "NEQ"
    | LT -> "LT"
    | GT -> "GT"
    | LEQ -> "LEQ"
    | GEQ -> "GEQ"
    | AND_AND -> "AND_AND"
    | OR_OR -> "OR_OR"
    | BANG -> "BANG"
    | COMMA -> "COMMA"
    | DOT -> "DOT"
    | COLON -> "COLON"
    | SEMICOLON -> "SEMICOLON"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACK -> "LBRACK"
    | RBRACK -> "RBRACK"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | EOF -> "EOF"

  let current_line_column lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    let column = pos.pos_cnum - pos.pos_bol + 1 in
    (pos.pos_lnum, max 1 column)

  let raise_lex_error lexbuf message =
    let line, column = current_line_column lexbuf in
    let lexeme = Lexing.lexeme lexbuf in
    raise (LexError { message; line; column; lexeme })

  let log_token lexbuf tok =
    if trace_tokens then
      let line, column = current_line_column lexbuf in
      Printf.eprintf "[LEX] %d:%d %-12s lexeme=%S\n"
        line column (token_to_string tok) (Lexing.lexeme lexbuf)

  let emit lexbuf tok =
    last_ends_stmt := ends_stmt tok;
    log_token lexbuf tok;
    tok
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident_char = letter | digit
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | whitespace        { read lexbuf }
  | "//" [^ '\n']*    { read lexbuf }
  | newline {
      Lexing.new_line lexbuf;
      if !last_ends_stmt then begin
        last_ends_stmt := false;
        emit lexbuf SEMICOLON
      end else read lexbuf
    }

  (* Palabras reservadas *)
  | "package"  { emit lexbuf PACKAGE }
  | "import"   { emit lexbuf IMPORT }
  | "func"     { emit lexbuf FUNC }
  | "if"       { emit lexbuf IF }
  | "else"     { emit lexbuf ELSE }
  | "return"   { emit lexbuf RETURN }
  | "type"     { emit lexbuf TYPE }
  | "struct"   { emit lexbuf STRUCT }
  | "true"     { emit lexbuf TRUE }
  | "false"    { emit lexbuf FALSE }
  | "var"      { emit lexbuf VAR }

  (* Operadores (orden importa: más específico primero) *)
  | ":="       { emit lexbuf DECL_ASSIGN }
  | "=="       { emit lexbuf EQ }
  | "!="       { emit lexbuf NEQ }
  | "<="       { emit lexbuf LEQ }
  | ">="       { emit lexbuf GEQ }
  | "&&"       { emit lexbuf AND_AND }
  | "||"       { emit lexbuf OR_OR }
  | "="        { emit lexbuf ASSIGN }
  | "<"        { emit lexbuf LT }
  | ">"        { emit lexbuf GT }
  | "+"        { emit lexbuf PLUS }
  | "-"        { emit lexbuf MINUS }
  | "*"        { emit lexbuf STAR }
  | "/"        { emit lexbuf SLASH }
  | "%"        { emit lexbuf PERCENT }
  | "!"        { emit lexbuf BANG }
  | ","        { emit lexbuf COMMA }
  | "."        { emit lexbuf DOT }
  | ":"        { emit lexbuf COLON }
  | ";"        { emit lexbuf SEMICOLON }
  | "("        { emit lexbuf LPAREN }
  | ")"        { emit lexbuf RPAREN }
  | "["        { emit lexbuf LBRACK }
  | "]"        { emit lexbuf RBRACK }
  | "{"        { emit lexbuf LBRACE }
  | "}"        { emit lexbuf RBRACE }

  (* Literales numéricos *)
  | digit+ "." digit* as s  { emit lexbuf (FLOATLIT (float_of_string s)) }
  | digit+ as s             { emit lexbuf (INTLIT (Int64.of_string s)) }

  (* Strings con escapes básicos *)
  | '"' { emit lexbuf (STRINGLIT (read_string (Buffer.create 32) lexbuf)) }

  (* Identificadores *)
  | letter ident_char* as s { emit lexbuf (IDENT s) }

  | eof {
      if !last_ends_stmt then begin
        last_ends_stmt := false;
        emit lexbuf SEMICOLON
      end else
        emit lexbuf EOF
    }
  | _ as c { raise_lex_error lexbuf (Printf.sprintf "Caracter inesperado: '%c'" c) }

and read_string buf = parse
  | '"'        { Buffer.contents buf }
  | "\\n"      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | "\\t"      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\\""     { Buffer.add_char buf '"'; read_string buf lexbuf }
  | "\\\\"     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | eof { raise_lex_error lexbuf "String sin cerrar" }
