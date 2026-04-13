%{
open Lib.Ast

let parse_type = function
	| "int" -> TInt
	| "string" -> TString
	| "bool" -> TBool
	| "float64" -> TFloat64
	| "any" -> TAny
	| name -> failwith ("Tipo no soportado en parser: " ^ name)
%}

%token PACKAGE IMPORT FUNC FOR IF ELSE RETURN VAR RANGE
%token TRUE FALSE NIL
%token <string> IDENT
%token <int> INTLIT
%token <string> STRINGLIT
%token ASSIGN DECL_ASSIGN
%token PLUS MINUS STAR SLASH MOD
%token EQ_EQ NOT_EQ LT GT LTE GTE
%token AND_AND OR_OR
%token BANG
%token INC DEC
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token COMMA
%token DOT
%token SEMICOLON
%token EOF

%start <Lib.Ast.program> program

%%

program:
	| PACKAGE pkg=IDENT opt_semi imports=import_decls decls=decls EOF
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
	|                          { [] }
	| path=STRINGLIT opt_semi rest=import_paths { path :: rest }

decls:
	|                         { [] }
	| d=decl ds=decls_tail    { d :: ds }

decls_tail:
	|                       { [] }
	| SEMICOLON ds=decls    { ds }

decl:
	| f=func_decl { FuncDecl f }
	| v=var_decl { v }

var_decl:
	| VAR name=IDENT typ=go_type init=typed_var_init_opt
			{ VarDecl (name, Some typ, init) }
	| VAR name=IDENT ASSIGN value=expr
			{ VarDecl (name, None, Some value) }

typed_var_init_opt:
	|                    { None }
	| ASSIGN value=expr  { Some value }

func_decl:
	| FUNC name=IDENT LPAREN params=params_opt RPAREN ret=ret_opt body=block
			{
				{
					name;
					params;
					ret;
					body;
				}
			}

params_opt:
	|               { [] }
	| ps=param_list { ps }

param_list:
	| p=param { [ p ] }
	| p=param COMMA ps=param_list { p :: ps }

param:
	| name=IDENT t=go_type { (name, t) }

ret_opt:
	|                      { [] }
	| t=go_type            { [ t ] }
	| LPAREN ts=go_types RPAREN { ts }

go_types:
	| t=go_type                  { [ t ] }
	| t=go_type COMMA ts=go_types { t :: ts }

go_type:
	| t=IDENT { parse_type t }
	| LBRACK RBRACK t=go_type { TSlice t }

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
	| FOR key=IDENT COMMA value=IDENT DECL_ASSIGN RANGE collection=expr body=block
			{ ForRange (key, value, collection, body) }
	| RETURN exprs=exprs_opt
			{ Return exprs }
	| lhs=assign_lhs ASSIGN rhs=expr
			{ Assign ([ lhs ], [ rhs ]) }
	| name=IDENT DECL_ASSIGN rhs=expr
			{ ShortDecl (name, rhs) }
	| name=IDENT INC
			{ ExprStmt (UnOp (Inc, Var name)) }
	| name=IDENT DEC
			{ ExprStmt (UnOp (Dec, Var name)) }
	| c=call_expr
			{ ExprStmt c }

assign_lhs:
	| name=IDENT { Var name }
	| obj=IDENT DOT field=IDENT { Selector (Var obj, field) }

exprs_opt:
	|               { [] }
	| es=expr_list  { es }

expr_list:
	| e=expr                    { [ e ] }
	| e=expr COMMA es=expr_list { e :: es }

expr:
	| e=or_expr { e }

or_expr:
	| e=and_expr                  { e }
	| l=or_expr OR_OR r=and_expr  { BinOp (Or, l, r) }

and_expr:
	| e=eq_expr                    { e }
	| l=and_expr AND_AND r=eq_expr { BinOp (And, l, r) }

eq_expr:
	| e=rel_expr                   { e }
	| l=eq_expr EQ_EQ r=rel_expr   { BinOp (Eq, l, r) }
	| l=eq_expr NOT_EQ r=rel_expr  { BinOp (Neq, l, r) }

rel_expr:
	| e=add_expr                 { e }
	| l=rel_expr LT r=add_expr   { BinOp (Lt, l, r) }
	| l=rel_expr GT r=add_expr   { BinOp (Gt, l, r) }
	| l=rel_expr LTE r=add_expr  { BinOp (Leq, l, r) }
	| l=rel_expr GTE r=add_expr  { BinOp (Geq, l, r) }

add_expr:
	| e=mul_expr                  { e }
	| l=add_expr PLUS r=mul_expr  { BinOp (Add, l, r) }
	| l=add_expr MINUS r=mul_expr { BinOp (Sub, l, r) }

mul_expr:
	| e=unary_expr                  { e }
	| l=mul_expr STAR r=unary_expr  { BinOp (Mul, l, r) }
	| l=mul_expr SLASH r=unary_expr { BinOp (Div, l, r) }
	| l=mul_expr MOD r=unary_expr   { BinOp (Mod, l, r) }

unary_expr:
	| e=primary        { e }
	| BANG e=unary_expr  { UnOp (Not, e) }
	| MINUS e=unary_expr { UnOp (Neg, e) }

call_expr:
	| fn=IDENT LPAREN args=args_opt RPAREN
			{ Call (fn, args) }
	| obj=IDENT DOT method_name=IDENT LPAREN args=args_opt RPAREN
			{ MethodCall (Var obj, method_name, args) }

primary:
	| n=INTLIT        { Lit (IntLit n) }
	| s=STRINGLIT     { Lit (StringLit s) }
	| TRUE            { Lit (BoolLit true) }
	| FALSE           { Lit (BoolLit false) }
	| NIL             { Lit NilLit }
	| c=call_expr     { c }
	| obj=IDENT DOT field=IDENT { Selector (Var obj, field) }
	| name=IDENT      { Var name }
	| LPAREN e=expr RPAREN { e }

args_opt:
	|                { [] }
	| args=expr_list { args }