%{
open Ast

let parse_type = function
  | "int" | "int64" | "int32" | "rune" | "byte" -> TInt
  | "float64" | "float32" -> TFloat
  | "string" -> TString
  | "bool" -> TBool
  | n -> TName n
%}

%token PACKAGE IMPORT FUNC IF ELSE RETURN TYPE STRUCT VAR
%token TRUE FALSE
%token <string> IDENT
%token <int64> INTLIT
%token <float> FLOATLIT
%token <string> STRINGLIT
%token ASSIGN DECL_ASSIGN
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT GT LEQ GEQ
%token AND_AND OR_OR BANG
%token COMMA DOT COLON SEMICOLON
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token EOF

%left OR_OR
%left AND_AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS
%nonassoc BANG

%start <Ast.program> program

%%

program:
  | PACKAGE name=IDENT SEMICOLON
    imports=import_list
    decls=decl_list
    EOF
    { { package = name; imports; decls } }

import_list:
  | { [] }
  | IMPORT path=STRINGLIT SEMICOLON rest=import_list { path :: rest }
  | IMPORT LPAREN paths=import_paths RPAREN SEMICOLON rest=import_list { paths @ rest }

import_paths:
  | { [] }
  | p=STRINGLIT SEMICOLON rest=import_paths { p :: rest }

decl_list:
  | { [] }
  | d=decl SEMICOLON rest=decl_list { d :: rest }

decl:
  | f=func_decl    { FuncDecl f }
  | s=struct_decl  { StructDecl s }

func_decl:
  | FUNC name=IDENT LPAREN params=param_list RPAREN ret=return_type body=block
    { { name; params; ret; body } }

param_list:
  | { [] }
  | p=param { [p] }
  | p=param COMMA rest=param_list { p :: rest }

param:
  | name=IDENT t=go_type { (name, t) }

return_type:
  | { TVoid }
  | t=go_type { t }

go_type:
  | name=IDENT                 { parse_type name }
  | LBRACK RBRACK t=go_type    { TSlice t }

struct_decl:
  | TYPE name=IDENT STRUCT LBRACE fields=field_list RBRACE
    { { name; fields } }

field_list:
  | { [] }
  | f=field SEMICOLON rest=field_list { f @ rest }

field:
  | names=ident_list t=go_type  { List.map (fun n -> (n, t)) names }

ident_list:
  | n=IDENT { [n] }
  | n=IDENT COMMA rest=ident_list { n :: rest }

block:
  | LBRACE stmts=stmt_list RBRACE { stmts }

stmt_list:
  | { [] }
  | s=stmt SEMICOLON rest=stmt_list { s :: rest }

stmt:
  | x=IDENT DECL_ASSIGN e=expr           { ShortDecl (x, e) }
  | x=IDENT ASSIGN e=expr                { Assign (x, e) }
  | IF c=expr_ns t=block                    { If (c, t, None) }
  | IF c=expr_ns t=block ELSE e=block       { If (c, t, Some e) }
  | IF c=expr_ns t=block ELSE elif=stmt     { If (c, t, Some [elif]) }
  | RETURN                               { Return None }
  | RETURN e=expr                        { Return (Some e) }
  | e=expr                               { ExprStmt e }

expr:
  | l=expr OR_OR r=expr     { BinOp (Or, l, r) }
  | l=expr AND_AND r=expr   { BinOp (And, l, r) }
  | l=expr EQ r=expr        { BinOp (Eq, l, r) }
  | l=expr NEQ r=expr       { BinOp (Neq, l, r) }
  | l=expr LT r=expr        { BinOp (Lt, l, r) }
  | l=expr GT r=expr        { BinOp (Gt, l, r) }
  | l=expr LEQ r=expr       { BinOp (Leq, l, r) }
  | l=expr GEQ r=expr       { BinOp (Geq, l, r) }
  | l=expr PLUS r=expr      { BinOp (Add, l, r) }
  | l=expr MINUS r=expr     { BinOp (Sub, l, r) }
  | l=expr STAR r=expr      { BinOp (Mul, l, r) }
  | l=expr SLASH r=expr     { BinOp (Div, l, r) }
  | l=expr PERCENT r=expr   { BinOp (Mod, l, r) }
  | MINUS e=expr %prec UMINUS { UnOp (Neg, e) }
  | BANG e=expr             { UnOp (Not, e) }
  | p=primary               { p }

expr_ns:
  | l=expr_ns OR_OR r=expr_ns     { BinOp (Or, l, r) }
  | l=expr_ns AND_AND r=expr_ns   { BinOp (And, l, r) }
  | l=expr_ns EQ r=expr_ns        { BinOp (Eq, l, r) }
  | l=expr_ns NEQ r=expr_ns       { BinOp (Neq, l, r) }
  | l=expr_ns LT r=expr_ns        { BinOp (Lt, l, r) }
  | l=expr_ns GT r=expr_ns        { BinOp (Gt, l, r) }
  | l=expr_ns LEQ r=expr_ns       { BinOp (Leq, l, r) }
  | l=expr_ns GEQ r=expr_ns       { BinOp (Geq, l, r) }
  | l=expr_ns PLUS r=expr_ns      { BinOp (Add, l, r) }
  | l=expr_ns MINUS r=expr_ns     { BinOp (Sub, l, r) }
  | l=expr_ns STAR r=expr_ns      { BinOp (Mul, l, r) }
  | l=expr_ns SLASH r=expr_ns     { BinOp (Div, l, r) }
  | l=expr_ns PERCENT r=expr_ns   { BinOp (Mod, l, r) }
  | MINUS e=expr_ns %prec UMINUS  { UnOp (Neg, e) }
  | BANG e=expr_ns                { UnOp (Not, e) }
  | p=primary_ns                  { p }

primary:
  | n=INTLIT    { Lit (IntLit n) }
  | f=FLOATLIT  { Lit (FloatLit f) }
  | s=STRINGLIT { Lit (StringLit s) }
  | TRUE        { Lit (BoolLit true) }
  | FALSE       { Lit (BoolLit false) }
  | x=IDENT     { Var x }
  | LPAREN e=expr RPAREN { e }
  | LBRACK RBRACK t=go_type LPAREN e=expr RPAREN { Cast (TSlice t, e) }
  | LBRACK RBRACK t=go_type LBRACE elems=slice_elems RBRACE { SliceLit (t, elems) }
  | LBRACK RBRACK t=go_type LBRACE RBRACE { SliceLit (t, []) }
  | e=primary LPAREN args=arg_list RPAREN {
      match e with
      | Var name -> Call (name, args)
      | Selector (Var pkg, fn) -> Call (pkg ^ "." ^ fn, args)
      | _ -> failwith "Llamada a funcion invalida"
    }
  | e=primary LBRACK idx=expr RBRACK { Index (e, idx) }
  | e=primary DOT field=IDENT { Selector (e, field) }
  | name=IDENT LBRACE args=keyed_args RBRACE { StructLit (name, args) }

primary_ns:
  | n=INTLIT    { Lit (IntLit n) }
  | f=FLOATLIT  { Lit (FloatLit f) }
  | s=STRINGLIT { Lit (StringLit s) }
  | TRUE        { Lit (BoolLit true) }
  | FALSE       { Lit (BoolLit false) }
  | x=IDENT     { Var x }
  | LPAREN e=expr RPAREN { e }
  | LBRACK RBRACK t=go_type LPAREN e=expr RPAREN { Cast (TSlice t, e) }
  | LBRACK RBRACK t=go_type LBRACE elems=slice_elems RBRACE { SliceLit (t, elems) }
  | LBRACK RBRACK t=go_type LBRACE RBRACE { SliceLit (t, []) }
  | e=primary_ns LPAREN args=arg_list RPAREN {
      match e with
      | Var name -> Call (name, args)
      | Selector (Var pkg, fn) -> Call (pkg ^ "." ^ fn, args)
      | _ -> failwith "Llamada a funcion invalida"
    }
  | e=primary_ns LBRACK idx=expr RBRACK { Index (e, idx) }
  | e=primary_ns DOT field=IDENT { Selector (e, field) }

arg_list:
  | { [] }
  | e=expr { [e] }
  | e=expr COMMA rest=arg_list { e :: rest }

keyed_args:
  | { [] }
  | a=keyed_arg { [a] }
  | a=keyed_arg COMMA { [a] }
  | a=keyed_arg COMMA rest=keyed_args { a :: rest }

keyed_arg:
  | key=IDENT COLON value=expr { (Some key, value) }
  | value=expr                 { (None, value) }

slice_elems:
  | e=slice_elem { [e] }
  | e=slice_elem COMMA { [e] }
  | e=slice_elem COMMA rest=slice_elems { e :: rest }

slice_elem:
  | LBRACE args=keyed_args RBRACE { StructLit ("", args) }
  | e=expr { e }
