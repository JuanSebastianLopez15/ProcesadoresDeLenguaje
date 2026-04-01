(*Es como un escaner que toma el codigo de go, lo agrupa y lo pone en tokens*)
(*Segun investigue y chat me dijo jajaj, OCaml usa ocamllex, este traduce el archivo .mll a codigo OCaml original o puro como diria el simon de los simones *)
{
  open Lib.Token
  exception SyntaxError of string
}

(*Etiquetas para no repetir codigo*)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | whitespace { read lexbuf } (*Se llama a el mismo para leer mas caracteres*)
  | newline    { read lexbuf } (*aca se pone el punto y coma automatico*)
  
  | "package"  { PACKAGE }
  | "import"   { IMPORT }
  | "func"     { FUNC }
  | "for"      { FOR }
  | "if"       { IF }
  | "else"     { ELSE }
  | "return"   { RETURN }
  
  | ":="       { DECL_ASSIGN }
  | "="        { ASSIGN }
  | "++"       { INC }
  | "=="       { EQ_EQ }
  | "<="       { LTE }
  | "+"        { PLUS }
  | "*"        { STAR }
  | "%"        { MOD }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "{"        { LBRACE }
  | "}"        { RBRACE }
  | ","        { COMMA }
  | "."        { DOT }
  | ";"        { SEMICOLON }
  
  | digit+ as n { INTLIT (int_of_string n) }
  | '"' ([^ '"']* as s) '"' { STRINGLIT s }
  | ident as id { IDENT id }
  
  | eof { EOF }
  | _ as c { raise (SyntaxError ("Caracter inesperado: " ^ String.make 1 c)) }