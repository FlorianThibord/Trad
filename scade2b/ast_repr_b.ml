(* Florian Thibord  --  Projet CERCLES *)

open Ast_base

type b_expression =
  BE_Ident of ident
| BE_Value of value
| BE_Op_Arith1 of op_arith1 * b_expression
| BE_Op_Arith2 of op_arith2 * b_expression * b_expression
| BE_Op_Sharp of b_expression list
| BE_Array of array_expr
| BE_Pred of b_predicate

and b_predicate =
| BP_Op_Logic of op_logic * b_predicate * b_predicate
| BP_Op_Relat of op_relat * b_expression * b_expression
| BP_Not of b_predicate
| BP_Expr of b_expression

and array_expr =
  BA_Def of b_expression list
| BA_Caret of b_expression * b_expression
| BA_Concat of b_expression * b_expression
| BA_Slice of ident * (b_expression * b_expression) list
| BA_Index of ident * b_expression list

type left_part =
  BLP_Ident of ident
| BLP_Tuple of ident list

type alternative =
  { alt_lp: left_part;
    alt_cond: b_expression;
    alt_then: b_expression;
    alt_else: b_expression;
  }

type call =
  { call_lp: left_part;
    call_id: ident;
    call_params: b_expression list;
    call_instance: ident;
  }

type op_base =
  { op_lp: left_part;
    op_expr: b_expression;
  }

type registre =
  { reg_lpid: ident;
    reg_val: b_expression;
  }

type substitution =
  Alternative of alternative
| Call of call
| Op_Base of op_base

type initialisation =
  ident * b_expression

type condition =
  Base_no_expr of ident * base_type * ident
| Fun_no_expr of ident * base_type * b_expression list * ident
| Base_expr of ident * base_type * b_expression * ident
| Fun_expr of ident * base_type * b_expression list * b_expression * ident

type op_decl =
  { id: ident;
    param_in: ident list;
    param_out: ident list;
  }


type imports_b = 
    { b_import_name : ident; 
      b_params_expr : b_expression list option; 
      b_instance_id : ident } 



(* module MAP_import = Map.Make( *)
(*   struct *)
(*     type t = ident *)
(*     let compare = compare *)
(*   end *)
(* ) *)
(* type map_inst_int = { map_int : int list; map_iident : ident} *)
(* type map_inst_imp = { map_expr : b_expression list option; map_ident : ident} *)
(* (\* params map called side *\) *)
(* type b_import_expr = b_expression list option MAP_import.t *)
(* (\* params map caller side *\) *)
(* type b_import_index = int list option MAP_import.t *)
(* type b_import_test = map_inst_imp MAP_import.t *)


type impl_operation =
  { op_decl: op_decl;
    vars: ident list;
    op_1: substitution list;
    op_2: registre list;
  }

type b_impl =
  { name: ident;
    params: ident list;
    refines: ident;
    sees: ident list;
    imports: imports_b list;
    concrete_variables: ident list;
    invariant: condition list;
    initialisation: initialisation list;
    operation: impl_operation;
  }

type abst_operation =
  { abstop_decl: op_decl;
    abstop_pre: condition list;
    abstop_post: condition list;
  }

type b_abst =
  { machine: ident;
    abst_params: ident list;
    abst_constraints: condition list;
    abst_sees: ident list;
    abst_operation: abst_operation;
  }

type prog =
  { machine_abstraite: b_abst;
    implementation: b_impl;
  }
