(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base


let print_bid ppt id =
  fprintf ppt "%s" id

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f


let rec print_opa_list op ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a %a" print_op_arith op print_expr v
  | v::l -> fprintf ppt "%a %a" print_expr v print_e_list l

and print_opl_list op ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a %a = TRUE" print_op_logic op print_expr v
  | v::l -> fprintf ppt "%a %a = TRUE %a" print_op_logic op print_expr v print_e_list l

and print_e_list ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a" print_expr v
  | v::l -> fprintf ppt "%a, %a" print_expr v print_e_list l

and print_expr ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Op_Arith (op, e_list) -> (
      match op with 
	| Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge ->
	    fprintf ppt "bool(%a %a)" print_expr (List.hd e_list) (print_opa_list op) (List.tl e_list)
	| Op_minus ->  
	    fprintf ppt "(%a%a)" print_op_arith op print_expr (List.hd e_list)
	| _ -> fprintf ppt "%a %a" print_expr (List.hd e_list) (print_opa_list op) (List.tl e_list)
    )
  | BE_Op_Logic (op, e_list) when op = Op_sharp -> 
      fprintf ppt "sharp(%a)" print_e_list e_list
  | BE_Op_Logic (op, e_list) when op = Op_xor -> 
      fprintf ppt "xor(%a)" print_e_list e_list
  | BE_Op_Logic (op, e_list) when op = Op_not -> 
      fprintf ppt "bool(%a(%a = TRUE))" print_op_logic op print_expr (List.hd e_list)
  | BE_Op_Logic (op, e_list) -> 
      fprintf ppt "bool(%a = TRUE %a)" print_expr (List.hd e_list) (print_opl_list op) (List.tl e_list)
  
and print_array ppt = function 
  | BA_Def e_list -> fprintf ppt "{%a}" print_def_list e_list
  | BA_Index (id, e_list) -> fprintf ppt "%a(%a)" print_bid id print_expr_list e_list
  | BA_Caret (e1, e2) -> fprintf ppt "caret(%a, %a)" print_expr e1 print_expr e2
  | BA_Concat (e1, e2) -> fprintf ppt "concat(%a, %a)" print_expr e1 print_expr e2
  | BA_Slice (id, e_list) -> fprintf ppt "slice(%a, %a)" print_bid id print_slice_list e_list

and print_def_list ppt e_list = 
  let rec fun_rec n ppt = function 
    | [] -> ()
    | [v] -> fprintf ppt "%d |-> %a" n print_expr v
    | v::l -> fprintf ppt "%d |-> %a, %a" n print_expr v (fun_rec (n+1)) l
  in
  fun_rec 0 ppt e_list

and print_slice_list ppt = function
  | [] -> ()
  | [(e1, e2)] -> fprintf ppt "(%a, %a)" print_expr e1 print_expr e2
  | (e1, e2)::l -> fprintf ppt "(%a, %a), %a" print_expr e1 print_expr e2 print_slice_list l

and print_expr_list ppt = function
  | [] -> ()
  | [(e)] -> fprintf ppt "%a" print_expr e
  | (e)::l -> fprintf ppt "%a, %a" print_expr e print_expr_list l


and print_op_arith ppt = function
  | Op_eq -> fprintf ppt "="
  | Op_neq -> fprintf ppt "/="
  | Op_lt -> fprintf ppt "<"
  | Op_le -> fprintf ppt "<="
  | Op_gt -> fprintf ppt ">"
  | Op_ge -> fprintf ppt ">="
  | Op_add -> fprintf ppt "+"
  | Op_sub -> fprintf ppt "-"
  | Op_mul -> fprintf ppt "*"
  | Op_div -> fprintf ppt "/"
  | Op_mod -> fprintf ppt "mod"
  | Op_div_f -> fprintf ppt "/"
  | Op_minus -> fprintf ppt "-"

and print_op_logic ppt = function
  | Op_and -> fprintf ppt "&"
  | Op_or -> fprintf ppt "or"
  | Op_not -> fprintf ppt "not "
  | Op_xor -> assert false
  | Op_sharp -> assert false


let print_lp ppt = function
  | BLP_Ident id -> print_bid ppt id
  | BLP_Tuple id_list -> print_idlist_comma ppt id_list


let print_alternative ppt a =
  fprintf ppt "IF %a = TRUE THEN %a := %a ELSE %a := %a END" 
    print_expr a.alt_cond
    print_lp a.alt_lp
    print_expr a.alt_then     
    print_lp a.alt_lp
    print_expr a.alt_else

let print_call ppt f =
  fprintf ppt "%a <-- %s(%a)"
    print_lp f.call_lp
    f.call_id
    print_e_list f.call_params
    
let print_op ppt o =
  fprintf ppt "%a := %a"
    print_lp o.op_lp
    print_expr o.op_expr
  
let print_eq ppt = function
  | Alternative a -> fprintf ppt "%a" print_alternative a
  | Call f -> fprintf ppt "%a" print_call f
  | Op_Base o -> fprintf ppt "%a" print_op o

let rec print_eq_list ppt = function
  | [] -> ()
  | [eq] -> fprintf ppt "%a" print_eq eq
  | eq::l -> fprintf ppt "%a; @,%a" print_eq eq print_eq_list l 
  
let print_registre ppt r =
  fprintf ppt "%a := %a"
    print_bid r.reg_lpid
    print_expr r.reg_val
  
let rec print_reg_list ppt = function
  | [] -> ()
  | [r] -> fprintf ppt "%a" print_registre r
  | r::l -> fprintf ppt "%a; @,%a" print_registre r print_reg_list l 


let print_vars ppt var_list =
  if (List.length var_list) = 0 then () 
  else
    fprintf ppt "VAR %a IN" print_idlist_comma var_list

let print_op_decl ppt op_decl =
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma op_decl.param_out
    op_decl.id
    print_idlist_comma op_decl.param_in
 
let print_operation ppt operations =
  let sep = if (List.length operations.op_2) > 0 then ";" else "" in
  let print_end = if (List.length operations.vars) > 0 then "END" else "" in
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n %a@\n@[<v 3>   %a%s@,%a@]@\n %s"
    print_op_decl operations.op_decl
    print_vars operations.vars
    print_eq_list operations.op_1
    sep
    print_reg_list operations.op_2
    print_end


let rec print_initialisation_list ppt = function
  | [] -> ()
  | [(id, e)] -> fprintf ppt "%a := %a" print_bid id print_expr e
  | (id, e)::l -> fprintf ppt "%a := %a ; @,%a" print_bid id print_expr e print_initialisation_list l 

let print_initialisation ppt ini_list = 
  if (List.length ini_list) = 0 then () 
  else 
    fprintf ppt "INITIALISATION @\n@[<v 3>   %a@]" print_initialisation_list ini_list 


let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let rec print_dim_list ppt = function
  | [] -> ()
  | [BE_Value (Int i)] -> fprintf ppt "0 .. %a" print_value (Int (i-1))
  | BE_Value (Int i) :: l -> fprintf ppt "0 .. %a, %a " print_value (Int (i-1)) print_dim_list l
  | [d] -> fprintf ppt "0 .. (%a-1)" print_expr d
  | d :: l -> fprintf ppt "0 .. (%a-1), %a " print_expr d print_dim_list l

let print_array_type t ppt e_list =
  fprintf ppt "%a --> (%a)" print_dim_list e_list print_basetype t

let print_condition ppt = function
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

let rec print_invariant_list ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_condition c
  | c::l -> fprintf ppt "%a & @,%a" print_condition c print_invariant_list l 

let print_invariant ppt inv_list = 
  if (List.length inv_list) = 0 then () 
  else 
    fprintf ppt "INVARIANT @\n@[<v 3>   %a@]" print_invariant_list inv_list 


let print_concrete_var ppt reg_list =
  if (List.length reg_list) = 0 then () 
  else 
    fprintf ppt "CONCRETE_VARIABLES %a" print_idlist_comma reg_list 

let print_imports ppt imports_m =
  let print_import ppt import =
    match import with
    | id, None -> fprintf "%a" print_id id
    | id, Some p -> fprintf "%a(%a)" print_id id print_expr_list p
  in 
  let rec print_import_list_comma ppt = function
  | [] -> ()
  | [import] -> fprintf ppt "%a" print_import id
  | import :: l -> fprintf ppt "%a, %a" print_import id print_import_list_comma l
  in
  if MAP_import.is_empty imports_m then () 
  else 
    fprintf ppt "IMPORTS %a" print_import_list_comma (MAP_import.bindings import_list)

let print_sees ppt sees_l =
  if (List.length sees_l) = 0 then () 
  else 
    fprintf ppt "SEES %a" print_idlist_comma sees_l

let print_refines ppt id =
  fprintf ppt "REFINES %s" id

let print_params_machine ppt params_machine =
  if (List.length params_machine) = 0 then () 
  else 
    fprintf ppt "(%a)" print_idlist_comma params_machine

let print_implementation ppt impl_name =
  fprintf ppt "%s" impl_name


let print_machine ppt b_impl =
  fprintf ppt
    "IMPLEMENTATION %a%a@\n%a@\n%a@\n%a@\n@\n%a@\n%a@\n%a@\n@\n%a @\nEND"
    print_implementation b_impl.name
    print_params_machine b_impl.params
    print_refines b_impl.refines
    print_sees b_impl.sees
    print_imports b_impl.imports
    print_concrete_var b_impl.concrete_variables
    print_invariant b_impl.invariant
    print_initialisation b_impl.initialisation
    print_operation b_impl.operation


let print_prog b_impl file =
  fprintf (formatter_of_out_channel file) "%a@." print_machine b_impl
