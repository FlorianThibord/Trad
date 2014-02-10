%{
  (* Florian Thibord  --  Projet CERCLES *)

  open Ast_scade
  open Ast_base

  (* Check if all the types in the list are the same. Used to check array coherence *)
  let check_type list =
    let typ = List.hd list in
    List.iter
      (fun t -> if t <> typ then raise Parsing.Parse_error else ()) list;
    typ

  (* Used for the distinction between Slice and Index 
  if e_list is [(a,b), (c,d), (e,f)] with a=b and c=d and e=f then it's an index [a,c,e] else it's a slice*)
  let handle_slice id elist =
    if Utils.a_b_list_equals elist then
      let (l, _) = List.split elist in
      PA_Index (id, l)
    else
      PA_Slice (id, elist)

  type equation_type = Eq of p_equation | Assume of p_expression | Guarantee of p_expression

%}

%token NODE RETURNS LET TEL VAR ASSUME GUARANTEE
%token DOUBLE_CHEVIN DOUBLE_CHEVOUT DOUBLE_COLON
%token IF THEN ELSE
%token FBY
%token PLUS MINUS MULT DIV DIV_INT MOD
%token EQ NEQ INF INFEQ SUP SUPEQ
%token AND OR NOT XOR SHARP
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOL COMMA DOT
%token DOTDOT CARET CONCAT
%token T_BOOL T_INT T_REAL
%token <bool> BOOL
%token <int> INT
%token <float> REAL
%token <string> IDENT
%token <string> PRAGMA
%token EOF

%nonassoc uminus
%nonassoc THEN
%nonassoc ELSE
%left DOTDOT
%left ARROW
%left OR XOR
%left AND
%left EQ NEQ INF INFEQ SUP SUPEQ
%left PLUS MINUS
%left MULT DIV DIV_INT MOD
%nonassoc T_REAL
%nonassoc T_INT
%nonassoc NOT PRE
%left CARET CONCAT

%start prog
%type <Ast_scade.p_node> prog

%%

prog :
 | node EOF { $1 }
;

node :
 | NODE IDENT LPAREN decl RPAREN RETURNS LPAREN decl RPAREN semi_opt
   var_decl
   LET eq_list TEL semi_opt
   { let (assumes, guarantees, eqs) = 
       List.fold_left (fun (a, g, e) eq -> match eq with
       | Eq p_eq -> (a, g, p_eq :: e)
       | Assume p_cond -> (p_cond :: a, g, e)
       | Guarantee p_cond -> (a, p_cond :: g, e)) ([], [], []) $13 in
     { p_id = $2;
       p_param_in = $4;
       p_param_out = $8;
       p_vars = $11;
       p_assumes = assumes;
       p_eqs = eqs;
       p_guarantees = guarantees; } }
;

var_decl :
 |   { [] }
 | VAR decl { $2 }
;

decl :
 | id_list COLON typ semi_opt { let typ = $3 in
				List.map (fun id -> (id, typ)) $1 }
 | id_list COLON typ SEMICOL decl { let typ = $3 in
				    (List.map (fun id -> (id, typ)) $1) @ $5 }
;

id_list :
 | IDENT { [$1] }
 | IDENT COMMA id_list { $1 :: $3 }
;

typ :
 | base_type { PT_Base $1 }
 | array_type { $1 }
;

base_type :
 | T_BOOL { T_Bool }
 | T_INT { T_Int }
 | T_REAL { T_Float }
;

/* 2 facons de déclarer un tableau */
array_type :
 | LBRACKET typ_list RBRACKET { let type_list = $2 in
				let typ = check_type type_list in
				PT_Array (typ, PE_Value (Int (List.length type_list))) }
 | typ CARET expr { PT_Array ($1, $3) }
;

typ_list :
 | typ { [$1] }
 | typ COMMA typ_list { $1 :: $3 }
;

eq_list :
 |   { [] }
 | eq eq_list { $1 :: $2 }
;

eq :
 | ASSUME IDENT COLON expr SEMICOL { Assume $4 }
 | GUARANTEE IDENT COLON expr SEMICOL { Guarantee $4 }
 | left_part EQ expr SEMICOL { Eq ($1, $3) }
;

left_part : 
 | LPAREN left_part RPAREN { $2 }
 | IDENT { PLP_Ident $1 }
 | IDENT COMMA id_list { PLP_Tuple ($1 :: $3) }
;

expr :
 | IDENT { PE_Ident $1 }
 | INT { PE_Value (Int $1) }
 | BOOL { PE_Value (Bool $1) }
 | REAL { PE_Value (Float $1) }
 | expr PLUS expr { PE_Op_Arith2 (Op_add, $1, $3) }
 | expr MINUS expr { PE_Op_Arith2 (Op_sub, $1, $3) }
 | expr MULT expr { PE_Op_Arith2 (Op_mul, $1, $3) }
 | expr DIV expr { PE_Op_Arith2 (Op_div_f, $1, $3) }
 | expr DIV_INT expr { PE_Op_Arith2 (Op_div, $1, $3) }
 | expr MOD expr { PE_Op_Arith2 (Op_mod, $1, $3) }
 | expr EQ expr { PE_Op_Arith2 (Op_eq, $1, $3) }
 | expr NEQ expr { PE_Op_Arith2 (Op_neq, $1, $3) }
 | expr INF expr { PE_Op_Relat (Op_lt, $1, $3) }
 | expr INFEQ expr { PE_Op_Relat (Op_le, $1, $3) }
 | expr SUP expr { PE_Op_Relat (Op_gt, $1, $3) }
 | expr SUPEQ expr { PE_Op_Relat (Op_ge, $1, $3) }
 | MINUS expr { PE_Op_Arith1 (Op_minus, $2) }
 | T_REAL expr { PE_Op_Arith1 (Op_cast_real, $2) }
 | T_INT expr { PE_Op_Arith1 (Op_cast_int, $2) }
 | SHARP LPAREN expr COMMA expr_list RPAREN { PE_Op_Sharp ($3 :: $5) }
 | expr AND expr { PE_Op_Logic (Op_and, $1, $3) }
 | expr OR expr { PE_Op_Logic (Op_or, $1, $3) }
 | expr XOR expr { PE_Op_Logic (Op_xor, $1, $3) }
 | NOT expr { PE_Op_Not $2 }
 | FBY LPAREN expr SEMICOL expr SEMICOL expr RPAREN { PE_Fby ($3, $5, $7) }
 | IF expr THEN expr ELSE expr { PE_If ($2, $4, $6) }
 | LPAREN PRAGMA IDENT DOUBLE_COLON IDENT DOUBLE_CHEVIN expr_list DOUBLE_CHEVOUT RPAREN LPAREN expr_list RPAREN 
     { PE_Call ($2, $5, $11) }
 | PRAGMA IDENT LPAREN expr_list RPAREN { PE_Call ($1, $2, $4) }
 | PRAGMA IDENT DOUBLE_COLON IDENT LPAREN expr_list RPAREN { PE_Call ($1, $4, $6) }
 | LPAREN expr RPAREN { $2 }
 | array_expr { PE_Array $1 }
;

expr_list :
 |   { [] }
 | expr { [$1] }
 | expr COMMA expr_list { $1 :: $3 }
;

array_expr :
 | LBRACKET expr_list RBRACKET { PA_Def $2 } /* OK POUR TABLEAU MULTI-DIM */
 | IDENT array_list { handle_slice $1 $2 }
 | expr CARET expr { PA_Caret ($1, $3) }
 | expr CONCAT expr { PA_Concat ($1, $3) }
;

array_list :
 | LBRACKET expr RBRACKET { [($2, $2)] }
 | LBRACKET expr DOTDOT expr RBRACKET { [($2, $4)] }
 | LBRACKET expr RBRACKET array_list { ($2, $2) :: $4 }
 | LBRACKET expr DOTDOT expr RBRACKET array_list { ($2, $4) :: $6 }
;

semi_opt :
 |   { () }
 | SEMICOL { () }
;

%%

