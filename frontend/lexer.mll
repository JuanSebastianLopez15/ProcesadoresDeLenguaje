{
  open Lib.Token
  exception SyntaxError of string

  let last_token_can_end_stmt = ref false

  let token_ends_stmt = function
    | IDENT _
    | STRUCT_ID _
    | INTLIT _
    | FLOATLIT _
    | STRINGLIT _
    | IOTA
    | TRUE
    | FALSE
    | NIL
    | RETURN
    | INC
    | DEC
    | RPAREN
    | RBRACK
    | RBRACE
    | STRUCT -> true
    | _ -> false

  let emit tok =
    last_token_can_end_stmt := token_ends_stmt tok;
    tok

  let remove_underscores s =
    String.concat "" (String.split_on_char '_' s)

  let parse_int_literal s =
    let s = remove_underscores s in
    let len = String.length s in
    try
      if len > 1 && s.[0] = '0'
         && s.[1] <> 'x' && s.[1] <> 'X'
         && s.[1] <> 'b' && s.[1] <> 'B'
         && s.[1] <> 'o' && s.[1] <> 'O'
      then Int64.of_string ("0o" ^ String.sub s 1 (len - 1))
      else Int64.of_string s
    with _ -> 0L

  let parse_float_literal s =
    try float_of_string (remove_underscores s) with _ -> 0.0

  let parse_char_literal s =
    if String.length s = 0 then 0
    else if s.[0] <> '\\' then Char.code s.[0]
    else if String.length s = 1 then 0 (* Should not happen *)
    else match s.[1] with
      | 'n' -> Char.code '\n'
      | 'r' -> Char.code '\r'
      | 't' -> Char.code '\t'
      | '\\' -> Char.code '\\'
      | '\'' -> Char.code '\''
      | '"' -> Char.code '"'
      | 'a' -> Char.code '\x07'
      | 'b' -> Char.code '\b'
      | 'f' -> Char.code '\x0c'
      | 'v' -> Char.code '\x0b'
      | 'x' -> 
          (try int_of_string ("0x" ^ String.sub s 2 2) with _ -> 0)
      | 'u' -> 
          (try int_of_string ("0x" ^ String.sub s 2 4) with _ -> 0)
      | 'U' -> 
          (try int_of_string ("0x" ^ String.sub s 2 8) with _ -> 0)
      | '0' .. '7' -> 
          (try int_of_string ("0o" ^ String.sub s 1 3) with _ -> 0)
      | _ -> Char.code s.[1]
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let dec_int = digit (digit | '_')*
let hex_int = "0" ['x' 'X'] (hexdigit | '_')+
let bin_int = "0" ['b' 'B'] (['0' '1'] | '_')+
let oct_new = "0" ['o' 'O'] (['0'-'7'] | '_')+
let oct_old = "0" (['0'-'7'] | '_')+
let exp = ['e' 'E'] ['+' '-']? (digit | '_')+

rule read = parse
  | whitespace { read lexbuf }
  | "//" [^ '\n' '\r']* { read lexbuf }
  | "/*" { block_comment lexbuf; read lexbuf }
  | newline    {
      Lexing.new_line lexbuf;
      if !last_token_can_end_stmt then (last_token_can_end_stmt := false; emit SEMICOLON)
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
  | "const"    { emit CONST }
  | "iota"     { emit IOTA }
  | "defer"    { emit DEFER }
  | "go"       { emit GO }
  | "switch"   { emit SWITCH }
  | "case"     { emit CASE }
  | "default"  { emit DEFAULT }
  | "range"    { emit RANGE }
  | "type"     { emit TYPE }
  | "struct"   { emit STRUCT }
  | "make"     { emit MAKE }
  | "new"      { emit NEW }
  | "delete"   { emit DELETE }
  | "real"     { emit REAL }
  | "imag"     { emit IMAG }
  | "complex"  { emit COMPLEX }
  | "copy"     { emit COPY }
  | "recover"  { emit RECOVER }
  | "panic"    { emit PANIC }
  | "append"   { emit APPEND }
  | "cap"      { emit CAP }
  | "len"      { emit LEN }
  | "close"    { emit CLOSE }
  | "map"      { emit MAP }
  | "interface" { emit INTERFACE }
  | "chan"     { emit CHAN }
  | "true"     { emit TRUE }
  | "false"    { emit FALSE }
  | "nil"      { emit NIL }
  
  | "..."      { emit ELLIPSIS }
  | ":="       { emit DECL_ASSIGN }
  | "<<"       { emit SHL }
  | ">>"       { emit SHR }
  | "<-"       { emit ARROW }
  | "=="       { emit EQ_EQ }
  | "!="       { emit NOT_EQ }
  | "<="       { emit LTE }
  | ">="       { emit GTE }
  | "&&"       { emit AND_AND }
  | "||"       { emit OR_OR }
  | "++"       { emit INC }
  | "--"       { emit DEC }
  | "+="       { emit PLUS_ASSIGN }
  | "-="       { emit MINUS_ASSIGN }
  | "*="       { emit STAR_ASSIGN }
  | "/="       { emit SLASH_ASSIGN }
  | "%="       { emit MOD_ASSIGN }
  | "&="       { emit AMP_ASSIGN }
  | "|="       { emit PIPE_ASSIGN }
  | "^="       { emit CARET_ASSIGN }
  | "<<="      { emit SHL_ASSIGN }
  | ">>="      { emit SHR_ASSIGN }
  | "&^="      { emit AND_NOT_ASSIGN }
  | "&^"       { emit AND_NOT }
  | "="        { emit ASSIGN }
  | ":"        { emit COLON }
  | "!"        { emit BANG }
  | "<"        { emit LT }
  | ">"        { emit GT }
  | "+"        { emit PLUS }
  | "-"        { emit MINUS }
  | "*"        { emit STAR }
  | "/"        { emit SLASH }
  | "%"        { emit MOD }
  | "&"        { emit AMP }
  | "|"        { emit PIPE }
  | "^"        { emit CARET }
  | "("        { emit LPAREN }
  | ")"        { emit RPAREN }
  | "["        { emit LBRACK }
  | "]"        { emit RBRACK }
  | "{"        { emit LBRACE }
  | "}"        { emit RBRACE }
  | ","        { emit COMMA }
  | "."        { emit DOT }
  | ";"        { emit SEMICOLON }

  | '`' { emit (STRINGLIT (raw_string (Buffer.create 1024) lexbuf)) }
  | '"' { emit (STRINGLIT (interp_string (Buffer.create 1024) lexbuf)) }
  | '\'' ( [^ '\\' '\''] | '\\' _ )+ as s '\'' { emit (RUNELIT (parse_char_literal s)) }

  | "0" ['x' 'X'] (hexdigit | '_')+ "." (hexdigit | '_')* ['p' 'P'] ['+' '-']? (digit | '_')+ "i" as n
      { emit (FLOATLIT (parse_float_literal (String.sub n 0 (String.length n - 1)))) }
  | "0" ['x' 'X'] (hexdigit | '_')+ "." (hexdigit | '_')* ['p' 'P'] ['+' '-']? (digit | '_')+ as n
      { emit (FLOATLIT (parse_float_literal n)) }
  | (dec_int "." (digit | '_')* exp? | "." (digit | '_')+ exp? | dec_int exp) "i" as n
      { emit (FLOATLIT (parse_float_literal (String.sub n 0 (String.length n - 1)))) }
  | (dec_int "." (digit | '_')* exp? | "." (digit | '_')+ exp? | dec_int exp) as n
      { emit (FLOATLIT (parse_float_literal n)) }

  | (hex_int | bin_int | oct_new | oct_old | dec_int) "i" as n
      {
        let base = String.sub n 0 (String.length n - 1) in
        emit (FLOATLIT (Int64.to_float (parse_int_literal base)))
      }
  | hex_int as n { emit (INTLIT (parse_int_literal n)) }
  | bin_int as n { emit (INTLIT (parse_int_literal n)) }
  | oct_new as n { emit (INTLIT (parse_int_literal n)) }
  | oct_old as n { emit (INTLIT (parse_int_literal n)) }
  | dec_int as n { emit (INTLIT (parse_int_literal n)) }

  | ['a'-'z' 'A'-'Z' '_' '\128'-'\255'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\128'-'\255']* as id 
      { if id.[0] >= 'A' && id.[0] <= 'Z' then emit (STRUCT_ID id) else emit (IDENT id) }
  
  | eof {
      last_token_can_end_stmt := false;
      EOF
    }
  | _ as c { raise (SyntaxError ("Caracter inesperado: " ^ String.make 1 c)) }

and block_comment = parse
  | "*/" { () }
  | newline { Lexing.new_line lexbuf; block_comment lexbuf }
  | eof { raise (SyntaxError "Comentario de bloque sin cerrar") }
  | _ { block_comment lexbuf }

and raw_string buf = parse
  | '`' { Buffer.contents buf }
  | newline { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; raw_string buf lexbuf }
  | eof { raise (SyntaxError "String raw sin cerrar") }
  | [^ '`' '\n' '\r']+ as chunk { Buffer.add_string buf chunk; raw_string buf lexbuf }

and interp_string buf = parse
  | '"' { Buffer.contents buf }
  | "\\\"" { Buffer.add_char buf '"'; interp_string buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; interp_string buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; interp_string buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; interp_string buf lexbuf }
  | "\\t" { Buffer.add_char buf '\t'; interp_string buf lexbuf }
  | "\\'" { Buffer.add_char buf '\''; interp_string buf lexbuf }
  | '\\' [^ '\n' '\r'] as seq { Buffer.add_string buf seq; interp_string buf lexbuf }
  | newline { Lexing.new_line lexbuf; raise (SyntaxError "String sin cerrar") }
  | eof { raise (SyntaxError "String sin cerrar") }
  | [^ '"' '\\' '\n' '\r']+ as chunk { Buffer.add_string buf chunk; interp_string buf lexbuf }
