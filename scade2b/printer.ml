open Ast_base
open Ast_repr_b
open Ast_scade_norm
open Format

let env = ref None

let print_bid ppt id =
    let bid = match !env with
      | None -> id
      | Some e -> Env.find id e
    in
    fprintf ppt "%s" bid

let with_env e f =
    (match !env with
        | Some _ -> failwith "with_env: env should be None"
        | None -> ());
    env := Some e;
    let r = f () in
    env := None;
    r

let rec print_list print_elem ppt = function
  | [] -> ()
  | [x] -> fprintf ppt "%a" print_elem x
  | x::xs -> fprintf ppt "%a, %a" print_elem x (print_list print_elem) xs

let print_idlist_comma = print_list print_bid

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let print_op_arith1 ppt = function
  | Op_minus -> fprintf ppt "-"
  | Op_cast_real -> fprintf ppt "REAL"
  | Op_cast_int -> fprintf ppt "INT"

let print_op_arith2 ppt = function
  | Op_eq -> fprintf ppt "="
  | Op_neq -> fprintf ppt "/="
  | Op_add -> fprintf ppt "+"
  | Op_sub -> fprintf ppt "-"
  | Op_mul -> fprintf ppt "*"
  | Op_div -> fprintf ppt "/"
  | Op_mod -> fprintf ppt "mod"
  | Op_div_f -> fprintf ppt "/"

let print_op_relat ppt = function
  | Op_lt -> fprintf ppt "<"
  | Op_le -> fprintf ppt "<="
  | Op_gt -> fprintf ppt ">"
  | Op_ge -> fprintf ppt ">="

let rec print_expr ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Op_Arith1 (op, e) -> (
      match op with
	| Op_minus ->
	    fprintf ppt "(%a%a)" print_op_arith1 op print_expr e
	| Op_cast_real | Op_cast_int ->
	    fprintf ppt "(%a (%a))" print_op_arith1 op print_expr e
    )
  | BE_Op_Arith2 (op, e1, e2) -> (
      match op with
	| Op_eq | Op_neq ->
            fprintf ppt "bool(%a %a %a)" print_expr e1 print_op_arith2 op print_expr e2
        | _ -> fprintf ppt "%a %a %a" print_expr e1 print_op_arith2 op print_expr e2
    )
  | BE_Op_Sharp e_list ->
      fprintf ppt "sharp(%a)" print_expr_list e_list
  | BE_Pred p ->
      fprintf ppt "bool%a" print_pred p

and print_pred ppt = function
  | BP_Op_Logic (op, p1, p2) ->
    fprintf ppt "(%a %a %a)" print_pred p1 print_op op print_pred p2
  | BP_Op_Relat (op, e1, e2) ->
    fprintf ppt "(%a %a %a)" print_expr e1 print_op_relat op print_expr e2
  | BP_Not p ->
    fprintf ppt "(not %a)" print_pred p
  | BP_Expr e ->
    fprintf ppt "(%a = TRUE)" print_expr e

and print_op ppt = function
  | Op_and -> fprintf ppt "&"
  | Op_or  -> fprintf ppt "or"
  | Op_xor -> fprintf ppt "/="

and print_expr_list ppt = print_list print_expr ppt

and print_array ppt = function
  | BA_Def e_list -> fprintf ppt "{%a}" print_def_list e_list
  | BA_Index (id, e_list) -> fprintf ppt "%a(%a)" print_bid id print_expr_list e_list
  | BA_Caret (e1, e2) -> fprintf ppt "caret(%a, %a)" print_expr e1 print_expr e2
  | BA_Concat (e1, e2) -> fprintf ppt "concat(%a, %a)" print_expr e1 print_expr e2
  | BA_Slice (id, e_list) -> fprintf ppt "slice(%a, %a)" print_bid id (print_list print_slice) e_list

and print_def_list ppt e_list =
  let rec fun_rec n ppt = function
    | [] -> ()
    | [v] -> fprintf ppt "%d |-> %a" n print_expr v
    | v::l -> fprintf ppt "%d |-> %a, %a" n print_expr v (fun_rec (n+1)) l
  in
  fun_rec 0 ppt e_list

and print_slice ppt (e1, e2) =
    fprintf ppt "(%a, %a)" print_expr e1 print_expr e2

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"

let print_sees ppt = function
    | [] -> ()
    | sees_l -> fprintf ppt "SEES %a" print_idlist_comma sees_l

let print_params_machine ppt = function
    | [] -> ()
    | params_machine -> fprintf ppt "(%a)" print_idlist_comma params_machine
