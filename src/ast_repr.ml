(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type p_expression =
  PE_Ident of ident
| PE_Value of value
| PE_Array of p_array_expr
| PE_App of ident * p_expression list
| PE_Bop of bop * p_expression * p_expression
| PE_Unop of unop * p_expression
| PE_Fby of p_expression * p_expression * p_expression
| PE_If of p_expression * p_expression * p_expression
| PE_Sharp of p_expression list

and p_array_expr =
  (* Initialisation avec [e1, e2, ...] (ex: [3,4,5] ) *)
  PA_Def of p_expression list
  (* Initialisation avec e1^ e2 (ex: false^4) *)
| PA_Caret of p_expression * p_expression
| PA_Concat of p_expression * p_expression
| PA_Slice of ident * (p_expression * p_expression) list
| PA_Index of ident * p_expression list


type p_left_part =
  PLP_Ident of ident
| PLP_Tuple of ident list

type p_equation = p_left_part * p_expression

type p_type =
  PT_Base of base_type
| PT_Array of p_type * p_expression

type p_decl = ident * p_type

type p_node =
  {  p_id: ident;
     p_param_in: p_decl list;
     p_param_out: p_decl list;
     p_assumes: p_expression list;
     p_guarantees: p_expression list;
     p_vars: p_decl list;
     p_eqs: p_equation list; 
  }

type p_prog = p_node list
