{
  open Parser
  exception LexError of string

  (* Inserción automática de punto y coma al estilo Go:
     cuando el token anterior puede terminar un statement y aparece
     un newline, se emite SEMICOLON. *)
  let last_ends_stmt = ref false
  let ends_stmt = function
    | IDENT _ | INTLIT _ | FLOATLIT _ | STRINGLIT _
    | TRUE | FALSE | RETURN
    | RPAREN | RBRACK | RBRACE -> true
    | _ -> false

  let emit tok = last_ends_stmt := ends_stmt tok; tok
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
        SEMICOLON
      end else read lexbuf
    }

  (* Palabras reservadas *)
  | "package"  { emit PACKAGE }
  | "import"   { emit IMPORT }
  | "func"     { emit FUNC }
  | "if"       { emit IF }
  | "else"     { emit ELSE }
  | "return"   { emit RETURN }
  | "type"     { emit TYPE }
  | "struct"   { emit STRUCT }
  | "true"     { emit TRUE }
  | "false"    { emit FALSE }
  | "var"      { emit VAR }

  (* Operadores (orden importa: más específico primero) *)
  | ":="       { emit DECL_ASSIGN }
  | "=="       { emit EQ }
  | "!="       { emit NEQ }
  | "<="       { emit LEQ }
  | ">="       { emit GEQ }
  | "&&"       { emit AND_AND }
  | "||"       { emit OR_OR }
  | "="        { emit ASSIGN }
  | "<"        { emit LT }
  | ">"        { emit GT }
  | "+"        { emit PLUS }
  | "-"        { emit MINUS }
  | "*"        { emit STAR }
  | "/"        { emit SLASH }
  | "%"        { emit PERCENT }
  | "!"        { emit BANG }
  | ","        { emit COMMA }
  | "."        { emit DOT }
  | ":"        { emit COLON }
  | ";"        { emit SEMICOLON }
  | "("        { emit LPAREN }
  | ")"        { emit RPAREN }
  | "["        { emit LBRACK }
  | "]"        { emit RBRACK }
  | "{"        { emit LBRACE }
  | "}"        { emit RBRACE }

  (* Literales numéricos *)
  | digit+ "." digit* as s  { emit (FLOATLIT (float_of_string s)) }
  | digit+ as s             { emit (INTLIT (Int64.of_string s)) }

  (* Strings con escapes básicos *)
  | '"' { emit (STRINGLIT (read_string (Buffer.create 32) lexbuf)) }

  (* Identificadores *)
  | letter ident_char* as s { emit (IDENT s) }

  | eof { EOF }
  | _ as c { raise (LexError (Printf.sprintf "Carácter inesperado: '%c'" c)) }

and read_string buf = parse
  | '"'        { Buffer.contents buf }
  | "\\n"      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | "\\t"      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\\""     { Buffer.add_char buf '"'; read_string buf lexbuf }
  | "\\\\"     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | eof { raise (LexError "String sin cerrar") }
