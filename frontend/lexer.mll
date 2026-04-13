(*Es como un escaner que toma el codigo de go, lo agrupa y lo pone en tokens*)
(*Segun investigue y chat me dijo jajaj, OCaml usa ocamllex, este traduce el archivo .mll a codigo OCaml original o puro como diria el simon de los simones *)
{
  open Lib.Token
  exception SyntaxError of string

  let last_token_can_end_stmt = ref false

  let token_ends_stmt = function
    | IDENT _
    | INTLIT _
    | STRINGLIT _
    | TRUE
    | FALSE
    | NIL
    | RETURN
    | INC
    | DEC
    | RPAREN
    | RBRACK
    | RBRACE -> true
    | _ -> false

  let emit tok =
    last_token_can_end_stmt := token_ends_stmt tok;
    tok
}

(*Etiquetas para no repetir codigo*)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | whitespace { read lexbuf } (*Se llama a el mismo para leer mas caracteres*)
  | newline    {
      if !last_token_can_end_stmt then emit SEMICOLON
      else read lexbuf
    }
  
  | "package"  { emit PACKAGE }
  | "import"   { emit IMPORT }
  | "func"     { emit FUNC }
  | "for"      { emit FOR }
  | "if"       { emit IF }
  | "else"     { emit ELSE }
  | "return"   { emit RETURN }
  | "var"      { emit VAR }
  | "range"    { emit RANGE }
  | "true"     { emit TRUE }
  | "false"    { emit FALSE }
  | "nil"      { emit NIL }
  
  | ":="       { emit DECL_ASSIGN }
  | "=="       { emit EQ_EQ }
  | "!="       { emit NOT_EQ }
  | "<="       { emit LTE }
  | ">="       { emit GTE }
  | "&&"       { emit AND_AND }
  | "||"       { emit OR_OR }
  | "++"       { emit INC }
  | "--"       { emit DEC }
  | "="        { emit ASSIGN }
  | "!"        { emit BANG }
  | "<"        { emit LT }
  | ">"        { emit GT }
  | "+"        { emit PLUS }
  | "-"        { emit MINUS }
  | "*"        { emit STAR }
  | "/"        { emit SLASH }
  | "%"        { emit MOD }
  | "("        { emit LPAREN }
  | ")"        { emit RPAREN }
  | "["        { emit LBRACK }
  | "]"        { emit RBRACK }
  | "{"        { emit LBRACE }
  | "}"        { emit RBRACE }
  | ","        { emit COMMA }
  | "."        { emit DOT }
  | ";"        { emit SEMICOLON }
  
  | digit+ as n { emit (INTLIT (int_of_string n)) }
  | '"' ([^ '"']* as s) '"' { emit (STRINGLIT s) }
  | ident as id { emit (IDENT id) }
  
  | eof {
      last_token_can_end_stmt := false;
      EOF
    }
  | _ as c { raise (SyntaxError ("Caracter inesperado: " ^ String.make 1 c)) }