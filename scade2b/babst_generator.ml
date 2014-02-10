(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base
open Printer

let rec print_dim_list ppt = function
  | [] -> ()
  | [BE_Value (Int i)] -> fprintf ppt "0 .. %a" print_value (Int (i-1))
  | BE_Value (Int i) :: l -> fprintf ppt "0 .. %a, %a " print_value (Int (i-1)) print_dim_list l
  | [d] -> fprintf ppt "0 .. (%a-1)" print_expr d
  | d :: l -> fprintf ppt "0 .. (%a-1), %a " print_expr d print_dim_list l

let print_array_type t ppt e_list =
  fprintf ppt "(%a) --> %a" print_dim_list e_list print_basetype t


let print_then_condition ppt = function
  | Base_expr (id, t, expr, ens_id) -> 
    fprintf ppt "%a :: { %a | %a : %a & %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      print_basetype t
      print_expr expr
  | Base_no_expr (id, t, ens_id) -> 
    fprintf ppt "%a :: { %a | %a : %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      print_basetype t
  | Fun_expr (id, t, e_list, expr, ens_id) ->
    fprintf ppt "%a :: { %a | %a : %a & %a } "
      print_bid id
      print_bid ens_id 
      print_bid ens_id
      (print_array_type t) e_list 
      print_expr expr;
  | Fun_no_expr (id, t, e_list, ens_id) ->
    fprintf ppt "%a :: { %a | %a : %a }"
      print_bid id
      print_bid ens_id
      print_bid ens_id
      (print_array_type t) e_list

let rec print_thenlist ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a||@,%a" print_then_condition c print_thenlist l 


let print_pre_condition ppt = function
  | Base_expr (id, t, expr, _) -> 
    fprintf ppt "%a : %a & %a"
      print_bid id
      print_basetype t
      print_expr expr 
  | Base_no_expr (id, t, _) ->
      fprintf ppt "%a : %a"
	print_bid id
	print_basetype t
  | Fun_expr (id, t, e_list, expr, _) ->
    fprintf ppt "%a : %a & %a "
      print_bid id
      (print_array_type t) e_list
      print_expr expr
  | Fun_no_expr (id, t, e_list, _) ->
    fprintf ppt "%a : %a"
      print_bid id
      (print_array_type t) e_list

let rec print_prelist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a &@,%a" print_pre_condition c print_prelist l 


let print_op_decl ppt op_decl =
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
    "MACHINE %a%a@\n%a@\n%a @\nEND"
    print_id_machine b_abst.machine
    print_params_machine b_abst.abst_params
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
