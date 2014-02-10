(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type n_expression =
    NE_Ident of ident
  | NE_Value of value
  | NE_Op_Arith1 of op_arith1 * n_expression
  | NE_Op_Arith2 of op_arith2 * n_expression * n_expression
  | NE_Op_Relat of op_relat * n_expression * n_expression
  | NE_Op_Logic of op_logic * n_expression * n_expression
  | NE_Op_Sharp of n_expression list
  | NE_Op_Not of n_expression
  | NE_Array of n_array_expr

and n_array_expr =
    NA_Def of n_expression list
  | NA_Caret of n_expression * n_expression
  | NA_Concat of n_expression * n_expression
  | NA_Slice of ident * (n_expression * n_expression) list
  | NA_Index of ident * n_expression list

type n_type =
    NT_Base of base_type
  | NT_Array of n_type * n_expression

type n_left_part =
    NLP_Ident of ident
  | NLP_Tuple of ident list

type n_alternative =
    { n_alt_lp: n_left_part;
      n_alt_cond: n_expression;
      n_alt_then: n_expression;
      n_alt_else: n_expression;
    }

type n_call =
    { n_fun_lp: n_left_part;
      n_fun_id: ident;
      n_fun_params: n_expression list;
      n_fun_pragma: ident;
    }

type n_operation =
    { n_op_lp: n_left_part;
      n_op_expr: n_expression;
    }

type n_registre =
    { n_reg_lpid: ident;
      n_reg_ini: n_expression;
      n_reg_delai: n_expression;
      n_reg_val: n_expression;
      n_reg_type: n_type;
    }

type n_equation =
    N_Alternative of n_alternative
  | N_Call of n_call
  | N_Operation of n_operation
  | N_Registre of n_registre

type n_condition =
    ident * n_type * n_expression option

type n_decl =
    ident * n_type

type n_lambda =
    { n_l_ident: ident;
      n_l_cond: n_condition; 
      n_l_index: int;
    }

module Env = Map.Make(
  struct
    type t = ident
    let compare = compare
  end
)

type env = n_condition Env.t

type n_node =
    { n_id: ident;
      n_lambdas: n_lambda list;
      n_env: env;
      n_param_in: n_decl list;
      n_param_out: n_decl list;
      n_vars: n_decl list;
      n_pre: n_condition list;
      n_post: n_condition list;
      n_eqs: n_equation list;
    }
