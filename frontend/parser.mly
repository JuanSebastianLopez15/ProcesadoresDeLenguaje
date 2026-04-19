%{
open Lib.Ast

let numeric_named_types =
  [ "int8"; "int16"; "int32"; "int64";
    "uint"; "uint8"; "uint16"; "uint32"; "uint64"; "uintptr";
    "byte"; "rune"; "float32"; "float64";
    "complex64"; "complex128" ]

let parse_type = function
	| "int" -> TInt
	| "rune" | "byte" -> TInt
	| "string" -> TString
	| "bool" -> TBool
	| "float64" | "float32" -> TFloat64
	| "any" -> TAny
	| name when List.mem name numeric_named_types -> TName name
	| name -> TName name
%}

/* DEFINICIÓN DE TOKENS */
%token PACKAGE IMPORT FUNC FOR IF ELSE RETURN VAR RANGE
%token TYPE STRUCT CONST DEFER GO SWITCH CASE DEFAULT
%token MAP INTERFACE CHAN
%token MAKE NEW DELETE REAL IMAG COMPLEX COPY RECOVER PANIC APPEND CAP LEN CLOSE
%token TRUE FALSE NIL
%token <string> IDENT
%token <string> STRUCT_ID
%token <int64> INTLIT
%token <float> FLOATLIT
%token <string> STRINGLIT
%token ASSIGN DECL_ASSIGN
%token COLON ELLIPSIS
%token PLUS MINUS STAR SLASH MOD
%token AMP PIPE CARET SHL SHR ARROW
%token EQ_EQ NOT_EQ LT GT LTE GTE
%token AND_AND OR_OR
%token BANG
%token INC DEC
%token PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN MOD_ASSIGN
%token AMP_ASSIGN PIPE_ASSIGN CARET_ASSIGN SHL_ASSIGN SHR_ASSIGN
%token AND_NOT AND_NOT_ASSIGN
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token COMMA
%token DOT
%token SEMICOLON
%token EOF

/* PRIORIDADES: De menor a mayor precedencia */
%left OR_OR
%left AND_AND
%left EQ_EQ NOT_EQ LT GT LTE GTE
%left PIPE CARET AMP
%left SHL SHR
%left PLUS MINUS
%left STAR SLASH MOD
%nonassoc INC DEC
%nonassoc BANG
%left DOT LBRACK LPAREN

%start <Lib.Ast.program> program

%%

any_ident:
	| id=IDENT { id }
	| id=STRUCT_ID { id }

program:
	| PACKAGE pkg=any_ident opt_semi imports=import_decls decls=decls EOF
			{ { package = pkg; imports; decls } }

opt_semi:
	|           { () }
	| SEMICOLON { () }

import_decls:
	|                                      { [] }
	| i=import_decl opt_semi rest=import_decls { i @ rest }

import_decl:
	| IMPORT path=STRINGLIT { [ path ] }
	| IMPORT LPAREN paths=import_paths RPAREN { paths }

import_paths:
	|                           { [] }
	| path=STRINGLIT opt_semi rest=import_paths { path :: rest }

decls:
	|                         { [] }
	| d=decl ds=decls_tail    { d @ ds }

decls_tail:
	|                       { [] }
	| SEMICOLON ds=decls    { ds }

decl:
	| f=func_decl { [FuncDecl f] }
	| v=var_decl { [v] }
	| c=const_decl { [c] }
	| TYPE name=any_ident type_rhs=tdecl_rhs { [TypeDecl (name, type_rhs)] }
	| VAR LPAREN vars=var_specs RPAREN { vars }
	| CONST LPAREN consts=const_specs RPAREN { consts }

tdecl_rhs:
	| STRUCT LBRACE fields=struct_fields RBRACE { TStruct fields }
	| FUNC LPAREN ps=rhs_tokens_group RPAREN ret=rhs_tokens { ignore ps; ignore ret; TAny }
	| ASSIGN t=go_type { t }
	| t=go_type { t }

struct_fields:
	|                                      { [] }
	| f=struct_field fs=struct_fields_tail { f @ fs }

struct_fields_tail:
	|                            { [] }
	| SEMICOLON fs=struct_fields { fs }

struct_field:
	| names=ident_list t=go_type tag=struct_tag_opt
			{ ignore tag; List.map (fun n -> (n, t)) names }
	| t=go_type tag=struct_tag_opt
			{ ignore tag; [ ("_embedded", t) ] }

struct_tag_opt:
	|           { () }
	| STRINGLIT { () }

var_specs:
	|                                   { [] }
	| v=var_spec vs=var_specs_tail      { v :: vs }

var_specs_tail:
	|                            { [] }
	| SEMICOLON vs=var_specs     { vs }

var_spec:
	| VAR name=any_ident typ=go_type init=typed_var_init_opt
			{ VarDecl (name, Some typ, init) }
	| VAR name=any_ident ASSIGN value=expr
			{ VarDecl (name, None, Some value) }
	| name=any_ident typ=go_type ASSIGN ignored_rhs
			{ VarDecl (name, Some typ, None) }
	| name=any_ident ASSIGN ignored_rhs
			{ VarDecl (name, Some TAny, None) }
	| name=any_ident typ=go_type
			{ VarDecl (name, Some typ, None) }

const_specs:
	|                                      { [] }
	| c=const_spec cs=const_specs_tail     { c :: cs }

const_specs_tail:
	|                             { [] }
	| SEMICOLON cs=const_specs    { cs }

const_spec:
	| name=any_ident ct=const_tail
			{ VarDecl (name, Some (match ct with Some t -> t | None -> TAny), None) }

const_tail:
	| typ=go_type const_init_opt { Some typ }
	| const_init_opt             { None }

const_decl:
	| CONST name=any_ident ct=const_tail
			{ VarDecl (name, Some (match ct with Some t -> t | None -> TAny), None) }

var_decl:
	| VAR name=any_ident typ=go_type init=typed_var_init_opt
			{ VarDecl (name, Some typ, init) }
	| VAR name=any_ident ASSIGN value=expr
			{ VarDecl (name, None, Some value) }

const_init_opt:
	|                    { () }
	| ASSIGN ignored_rhs { () }

ignored_rhs:
	| rhs_tokens { () }

rhs_tokens:
	|                                { () }
	| rhs_token rhs_tokens  { () }

rhs_tokens_group:
	|                                     { () }
	| rhs_token rhs_tokens_group { () }
	| SEMICOLON rhs_tokens_group    { () }

rhs_token:
	| id=IDENT { ignore id }
	| id=STRUCT_ID { ignore id }
	| n=INTLIT { ignore n }
	| f=FLOATLIT { ignore f }
	| s=STRINGLIT { ignore s }
	| TRUE { () }
	| FALSE { () }
	| NIL { () }
	| PACKAGE { () }
	| IMPORT { () }
	| MAKE { () } | NEW { () } | DELETE { () } | REAL { () } | IMAG { () }
	| COMPLEX { () } | COPY { () } | RECOVER { () } | PANIC { () }
	| APPEND { () } | CAP { () } | LEN { () } | CLOSE { () }
	| ASSIGN { () }
	| DECL_ASSIGN { () }
	| COLON { () }
	| ELLIPSIS { () }
	| PLUS { () } | MINUS { () } | STAR { () } | SLASH { () } | MOD { () }
	| AMP { () } | PIPE { () } | CARET { () }
	| SHL { () } | SHR { () } | ARROW { () }
	| EQ_EQ { () } | NOT_EQ { () } | LT { () } | GT { () } | LTE { () } | GTE { () }
	| AND_AND { () } | OR_OR { () }
	| BANG { () } | INC { () } | DEC { () }
	| PLUS_ASSIGN { () } | MINUS_ASSIGN { () } | STAR_ASSIGN { () } | SLASH_ASSIGN { () } | MOD_ASSIGN { () }
	| AMP_ASSIGN { () } | PIPE_ASSIGN { () } | CARET_ASSIGN { () } | SHL_ASSIGN { () } | SHR_ASSIGN { () }
	| AND_NOT { () } | AND_NOT_ASSIGN { () }
	| COMMA { () } | DOT { () }
	| MAP { () } | INTERFACE { () } | CHAN { () }
	| FUNC { () } | STRUCT { () } | RANGE { () }
	| VAR { () } | CONST { () } | TYPE { () }
	| IF { () } | ELSE { () } | FOR { () } | RETURN { () }
	| DEFER { () } | GO { () } | SWITCH { () } | CASE { () } | DEFAULT { () }
	| LPAREN inner=rhs_tokens_group RPAREN { ignore inner }
	| LBRACK inner=rhs_tokens_group RBRACK { ignore inner }
	| LBRACE inner=rhs_tokens_group RBRACE { ignore inner }

typed_var_init_opt:
	|                    { None }
	| ASSIGN value=expr  { Some value }

func_decl:
	| FUNC name=any_ident LPAREN params=params_opt RPAREN ret=ret_opt body=block
			{ { name; params; ret; body; } }
	| FUNC LPAREN rname=any_ident rtyp=go_type RPAREN name=any_ident LPAREN params=params_opt RPAREN ret=ret_opt body=opaque_block
			{ ignore rname; ignore rtyp; ignore body; { name; params; ret; body = []; } }

opaque_block:
	| LBRACE body=rhs_tokens_group RBRACE { ignore body; [] }

params_opt:
	|               { [] }
	| ps=param_list { ps }

param_list:
	| p=param { [ p ] }
	| p=param COMMA ps=param_list { p :: ps }

param:
	| name=any_ident t=go_type { (name, t) }

ret_opt:
	|                      { [] }
	| t=go_type            { [ t ] }
	| LPAREN ts=go_types RPAREN { ts }

go_types:
	| t=go_type_item                     { [ t ] }
	| t=go_type_item COMMA ts=go_types   { t :: ts }

go_type_item:
	| t=go_type { t }
	| names=ident_list t=go_type { ignore names; t }

ident_list:
	| id=any_ident { [id] }
	| id=any_ident COMMA ids=ident_list { id :: ids }

go_type:
	| t=any_ident { parse_type t }
	| STRUCT LBRACE fields=struct_fields RBRACE { TStruct fields }
	| LBRACK RBRACK t=go_type { TSlice t }
	| LBRACK INTLIT RBRACK t=go_type { TSlice t }
	| LBRACK ELLIPSIS RBRACK t=go_type { TSlice t }
	| MAP LBRACK k=go_type RBRACK v=go_type { TMap (k, v) }
	| STAR t=go_type { ignore t; TAny }
	| CHAN t=go_type { ignore t; TAny }
	| CHAN ARROW t=go_type { ignore t; TAny }
	| ARROW CHAN t=go_type { ignore t; TAny }
	| INTERFACE LBRACE RBRACE { TAny }
	| INTERFACE LBRACE body=rhs_tokens_group RBRACE { ignore body; TAny }
	| FUNC LPAREN ps=rhs_tokens_group RPAREN ret=rhs_tokens { ignore ps; ignore ret; TAny }

block:
	| LBRACE stmts=stmts RBRACE { stmts }

stmts:
	|                         { [] }
	| s=stmt ss=stmts_tail    { s :: ss }

stmts_tail:
	|                        { [] }
	| SEMICOLON ss=stmts     { ss }

stmt:
	| IF cond=expr then_block=block
			{ If (cond, then_block, None) }
	| IF cond=expr then_block=block ELSE else_block=block
			{ If (cond, then_block, Some else_block) }
	| FOR cond=expr body=block
			{ ForCond (cond, body) }
	| FOR key=any_ident COMMA value=any_ident DECL_ASSIGN RANGE collection=expr body=block
			{ ForRange (key, value, collection, body) }
	| RETURN exprs=exprs_opt
			{ Return exprs }
	| lhs=expr_list ASSIGN rhs=expr_list
			{ Assign (lhs, rhs) }
	| names=ident_list DECL_ASSIGN rhs=expr_list
			{ ShortDecl (names, rhs) }
	| name=any_ident INC
			{ ExprStmt (UnOp (Inc, Var name)) }
	| name=any_ident DEC
			{ ExprStmt (UnOp (Dec, Var name)) }
	| e=expr INC
			{ ExprStmt (UnOp (Inc, e)) }
	| e=expr DEC
			{ ExprStmt (UnOp (Dec, e)) }
	| DEFER e=expr { Defer e }
	| GO e=expr { Go e }
	| VAR name=any_ident _typ=go_type init=typed_var_init_opt
			{ (match init with Some e -> ShortDecl ([name], [e]) | None -> ShortDecl ([name], [Lit NilLit])) }
	| VAR name=any_ident ASSIGN value=expr
			{ ShortDecl ([name], [value]) }
	| CONST name=any_ident ct=const_tail
			{ ignore ct; ShortDecl ([name], [Lit NilLit]) }
	| e=expr { ExprStmt e }
	| rhs_tokens_group { ExprStmt (Lit NilLit) } /* Greedy approach for anything else */

expr:
	| e=primary             { e }
	| e=expr INC %prec INC  { UnOp (Inc, e) }
	| e=expr DEC %prec DEC  { UnOp (Dec, e) }
	| l=expr OR_OR r=expr   { BinOp (Or, l, r) }
	| l=expr AND_AND r=expr { BinOp (And, l, r) }
	| l=expr PIPE r=expr    { BinOp (Add, l, r) }
	| l=expr CARET r=expr   { BinOp (Sub, l, r) }
	| l=expr AMP r=expr     { BinOp (Mul, l, r) }
	| l=expr SHL r=expr     { BinOp (Mul, l, r) }
	| l=expr SHR r=expr     { BinOp (Div, l, r) }
	| l=expr EQ_EQ r=expr   { BinOp (Eq, l, r) }
	| l=expr NOT_EQ r=expr  { BinOp (Neq, l, r) }
	| l=expr LT r=expr      { BinOp (Lt, l, r) }
	| l=expr GT r=expr      { BinOp (Gt, l, r) }
	| l=expr LTE r=expr     { BinOp (Leq, l, r) }
	| l=expr GTE r=expr     { BinOp (Geq, l, r) }
	| l=expr PLUS r=expr    { BinOp (Add, l, r) }
	| l=expr MINUS r=expr   { BinOp (Sub, l, r) }
	| l=expr STAR r=expr    { BinOp (Mul, l, r) }
	| l=expr SLASH r=expr   { BinOp (Div, l, r) }
	| l=expr MOD r=expr     { BinOp (Mod, l, r) }
	| BANG e=expr           { UnOp (Not, e) }
	| MINUS e=expr %prec BANG { UnOp (Neg, e) }

primary:
	| n=INTLIT        { Lit (IntLit n) }
	| f=FLOATLIT      { Lit (FloatLit f) }
	| s=STRINGLIT     { Lit (StringLit s) }
	| TRUE            { Lit (BoolLit true) }
	| FALSE           { Lit (BoolLit false) }
	| NIL             { Lit NilLit }
	| name=any_ident  { Var name }
	| LPAREN e=expr RPAREN { e }
	| e=primary LPAREN args=args_opt RPAREN { Call (e, args) }
	| MAKE LPAREN t=go_type COMMA args=expr_list RPAREN { ignore t; Call (Var "make", args) }
	| MAKE LPAREN t=go_type RPAREN { ignore t; Call (Var "make", []) }
	| NEW LPAREN t=go_type RPAREN { ignore t; Call (Var "new", []) }
	| DELETE LPAREN args=args_opt RPAREN { Call (Var "delete", args) }
	| REAL LPAREN args=args_opt RPAREN { Call (Var "real", args) }
	| IMAG LPAREN args=args_opt RPAREN { Call (Var "imag", args) }
	| COMPLEX LPAREN args=args_opt RPAREN { Call (Var "complex", args) }
	| COPY LPAREN args=args_opt RPAREN { Call (Var "copy", args) }
	| RECOVER LPAREN args=args_opt RPAREN { Call (Var "recover", args) }
	| PANIC LPAREN args=args_opt RPAREN { Call (Var "panic", args) }
	| APPEND LPAREN args=args_opt RPAREN { Call (Var "append", args) }
	| CAP LPAREN args=args_opt RPAREN { Call (Var "cap", args) }
	| LEN LPAREN args=args_opt RPAREN { Call (Var "len", args) }
	| CLOSE LPAREN args=args_opt RPAREN { Call (Var "close", args) }
	| LBRACK RBRACK t=go_type LPAREN e=expr RPAREN { Cast (TSlice t, e) }
	| e=primary DOT field=any_ident { Selector (e, field) }
	| e=primary DOT field=any_ident LPAREN args=args_opt RPAREN { MethodCall (e, field, args) }
	| e=primary LBRACK i=expr RBRACK { Index (e, i) }
	| e=primary LBRACK low=expr_opt COLON high=expr_opt RBRACK { Slice (e, low, high, None) }
	| e=primary LBRACK low=expr_opt COLON high=expr COLON max=expr RBRACK { Slice (e, low, Some high, Some max) }
	| t_name=STRUCT_ID LBRACE args=args_opt RBRACE { StructLit (t_name, args) }
	| LBRACK RBRACK t=go_type LBRACE args=args_opt RBRACE { SliceLit (t, args) }


expr_opt:
    |            { None }
    | e=expr     { Some e }

args_opt:
	|                { [] }
	| args=expr_list { args }

exprs_opt:
	|               { [] }
	| es=expr_list  { es }

expr_list:
	| e=expr                    { [ e ] }
	| e=expr COMMA es=expr_list { e :: es }
