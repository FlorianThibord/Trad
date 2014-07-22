(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base
open Printer

let print_then_condition ppt = function
  | Base_expr (id, t, expr, ens_id) -> 
    fprintf ppt "%a :: { %a | %a : %a & %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      print_basetype t
      print_expr_in_pred expr
  | Base_no_expr (id, t, ens_id) -> 
    fprintf ppt "%a :: { %a | %a : %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      print_basetype t
  | Fun_expr (id, t, e_list, expr, ens_id, index) ->
    fprintf ppt "%a :: { %a | %a : %a & !%s.(%s : dom(%a) => %a) } "
      print_bid id
      print_bid ens_id
      print_bid ens_id
      (print_array_type t) e_list 
      index index
      print_bid ens_id
      print_expr_in_pred expr;
  | Fun_no_expr (id, t, e_list, ens_id) ->
    fprintf ppt "%a :: { %a | %a : %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      (print_array_type t) e_list

let rec print_thenlist_rec ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a||@,%a" print_then_condition c print_thenlist_rec l 

let print_thenlist ppt posts =
  if (List.length posts = 0) then fprintf ppt "skip" 
  else print_thenlist_rec ppt posts

let print_pre_condition ppt = function
  | Base_expr (id, t, expr, _) -> 
    fprintf ppt "%a : %a & %a"
      print_bid id
      print_basetype t
      print_expr_in_pred expr 
  | Base_no_expr (id, t, _) ->
      fprintf ppt "%a : %a"
	print_bid id
	print_basetype t
  | Fun_expr (id, t, e_list, expr,_, index) ->
    fprintf ppt "%a : %a & !%s.(%s : dom(%a) => %a)"
      print_bid id
      (print_array_type t) e_list
      index index
      print_bid id
      print_expr_in_pred expr
  | Fun_no_expr (id, t, e_list, _) ->
    fprintf ppt "%a : %a"
      print_bid id
      (print_array_type t) e_list

let rec print_prelist_rec ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a &@,%a" print_pre_condition c print_prelist_rec l 

let print_prelist ppt pres =
  if (List.length pres = 0) then fprintf ppt "TRUE = TRUE" 
  else print_prelist_rec ppt pres

let print_op_decl ppt op_decl =
  if (List.length op_decl.param_out = 0) && (List.length op_decl.param_in = 0) then
    fprintf ppt "%s" op_decl.id
  else if (List.length op_decl.param_out = 0) then
    fprintf ppt "%s(%a)" op_decl.id print_idlist_comma op_decl.param_in
  else if (List.length op_decl.param_in = 0) then
    fprintf ppt "%a <-- %s" print_idlist_comma op_decl.param_out op_decl.id
  else
    fprintf ppt "%a <-- %s(%a)"
      print_idlist_comma op_decl.param_out
      op_decl.id
      print_idlist_comma op_decl.param_in
 
let print_operation ppt abstop =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v>%a@]@]@\n@[<v 3> THEN@,@[<v>%a@]@]@\n END"
    print_op_decl abstop.abstop_decl
    print_prelist abstop.abstop_pre
    print_thenlist abstop.abstop_post

let print_constraints ppt constraints =
  if (List.length constraints) = 0 then () 
  else 
    fprintf ppt "CONSTRAINTS@\n  %a" print_prelist constraints

let print_id_machine ppt id_machine =
  fprintf ppt "%s" id_machine

let print_machine_root ppt b_abst =
  fprintf ppt
    "MACHINE %a%a@\n%a@\n%a@\n%a @\nEND"
    print_id_machine b_abst.machine
    print_params_machine b_abst.abst_params
    print_sees b_abst.abst_sees
    print_constraints b_abst.abst_constraints
    print_operation b_abst.abst_operation

let print_machine ppt b_abst =
  fprintf ppt
    "MACHINE %a%a@\n%a@\n%a@\n%a @\nEND"
    print_id_machine b_abst.machine
    print_params_machine b_abst.abst_params
    print_sees b_abst.abst_sees
    print_constraints b_abst.abst_constraints
    print_operation b_abst.abst_operation


let print_prog b_abst file is_root =
  if is_root then
    fprintf (formatter_of_out_channel file) "%a@." print_machine_root b_abst
  else 
    fprintf (formatter_of_out_channel file) "%a@." print_machine b_abst
