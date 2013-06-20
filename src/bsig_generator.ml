(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_repr_b
open Ast_base



(* too naive, change that. *)
let fun_cond = ref false 
let var_cond = ref []

let print_bid ppt id =
  if !fun_cond = true then
    let indice = 
      try
	"(" ^ (List.assoc id !var_cond) ^ ")"
      with Not_found -> ""
    in
    fprintf ppt "%s%s" id indice
  else fprintf ppt "%s" id

let rec print_idlist_comma ppt = function
  | [] -> ()
  | [id] -> fprintf ppt "%a" print_bid id
  | id::l -> fprintf ppt "%a, %a" print_bid id print_idlist_comma l

let print_value ppt = function
  | Bool b -> fprintf ppt "%s" (if b then "TRUE" else "FALSE")
  | Int i -> fprintf ppt "%d" i
  | Float f -> fprintf ppt "%f" f

let rec print_e_list ppt = function 
  | [] -> ()
  | [v] -> fprintf ppt "%a" print_expr v
  | v::l -> fprintf ppt "%a, %a" print_expr v print_e_list l

and print_expr ppt = function
  | BE_Ident id -> print_bid ppt id
  | BE_Tuple e_list -> fprintf ppt "(@[%a@])" print_e_list e_list
  | BE_Value v -> print_value ppt v
  | BE_Array ar -> print_array ppt ar
  | BE_Bop (bop, e1, e2) when bop = Op_xor -> fprintf ppt "xor(%a, %a)" print_expr e1 print_expr e2
  | BE_Bop (bop, e1, e2) -> fprintf ppt "%a %a %a" print_expr e1 print_bop bop print_expr e2
  | BE_Unop (unop, e) -> fprintf ppt "%a%a" print_unop unop print_expr e
  | BE_Sharp e_list -> fprintf ppt "sharp(%a)" print_e_list e_list

and print_array ppt = function 
  | BA_Def e_list -> fprintf ppt "{%a}" print_def_list e_list
  | BA_Index (id, e_list) -> fprintf ppt "%a(%a)" print_bid id print_index_list e_list
  | BA_Caret (e1, e2) -> fprintf ppt "caret(%a, %a)" print_expr e1 print_expr e2
  | BA_Concat (e1, e2) -> fprintf ppt "concat(%a, %a)" print_expr e1 print_expr e2
  | BA_Slice (id, e_list) -> fprintf ppt "slice(%a, %a)" print_bid id print_slice_list e_list

and print_def_list ppt e_list = 
  let rec fun_rec n ppt = function 
    | [] -> ()
    | [v] -> fprintf ppt "%d |-> %a" n print_expr v
    | v::l -> fprintf ppt "%d |-> %a, %a" n print_expr v (fun_rec (n+1)) l
  in
  fun_rec 1 ppt e_list

and print_slice_list ppt = function
  | [] -> ()
  | [(e1, e2)] -> fprintf ppt "(%a, %a)" print_expr e1 print_expr e2
  | (e1, e2)::l -> fprintf ppt "(%a, %a), %a" print_expr e1 print_expr e2 print_slice_list l

and print_index_list ppt = function
  | [] -> ()
  | [(e)] -> fprintf ppt "%a" print_expr e
  | (e)::l -> fprintf ppt "%a, %a" print_expr e print_index_list l

and print_bop ppt = function
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
  | Op_add_f -> fprintf ppt "+"
  | Op_sub_f -> fprintf ppt "-"
  | Op_mul_f -> fprintf ppt "*"
  | Op_div_f -> fprintf ppt "/"
  | Op_and -> fprintf ppt "&"
  | Op_or -> fprintf ppt "or"
  | Op_xor -> assert false

and print_unop ppt = function 
  | Op_not -> fprintf ppt "not "
  | Op_minus -> fprintf ppt "-"

let print_basetype ppt = function
  | T_Bool -> fprintf ppt "%s" "BOOL"
  | T_Int -> fprintf ppt "%s" "INT"
  | T_Float -> fprintf ppt "%s" "REAL"


let rec print_dim_list ppt = function
  | [] -> ()
  | [d] -> fprintf ppt "1 .. %a" print_expr d
  | d :: l -> fprintf ppt "1 .. %a, %a " print_expr d print_dim_list l

let print_array_type t ppt e_list =
  fprintf ppt "(%a) --> %a" print_dim_list e_list print_basetype t

let print_then_condition ppt = function
  | Base_expr (id, t, expr) -> 
    fprintf ppt "%a :: { %a | %a : %a & %a }"
      print_bid id
      print_bid id
      print_bid id
      print_basetype t
      print_expr expr
  | Fun_expr (id, t, e_list, expr) ->
    var_cond := (id, "iii") :: !var_cond;
    fprintf ppt "%a :: { %a | %a : %a & !%s. (%s : (%a) => "
      print_bid id
      print_bid id
      print_bid id
      (print_array_type t) e_list
      "iii"
      "iii"
      print_dim_list e_list;
    fun_cond := true;    
    fprintf ppt "%a )}" print_expr expr;
    var_cond := List.tl !var_cond;
    fun_cond := false


let rec print_thenlist ppt = function
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a||@,%a" print_then_condition c print_thenlist l 

let print_pre_condition ppt = function
  | Base_expr (id, t, expr) -> 
    fprintf ppt "%a : %a & %a"
      print_bid id
      print_basetype t
      print_expr expr 
  | Fun_expr (id, t, e_list, expr) ->
    var_cond := (id, "iii") :: !var_cond;
    fprintf ppt "%a : %a & !%s. (%s : (%a) => "
      print_bid id
      (print_array_type t) e_list
      "iii"
      "iii"
      print_dim_list e_list;
    fun_cond := true;    
    fprintf ppt "%a )" print_expr expr;
    var_cond := List.tl !var_cond;
    fun_cond := false

let rec print_prelist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a &@,%a" print_pre_condition c print_prelist l 

let print_op_decl ppt op_decl =
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma op_decl.param_out
    op_decl.id
    print_idlist_comma op_decl.param_in

let print_operation ppt sigop =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v> %a@]@]@\n@[<v 3> THEN@,@[<v> %a@]@]@\n END"
    print_op_decl sigop.sigop_decl
    print_prelist sigop.sigop_pre
    print_thenlist sigop.sigop_post

(* The file list can be configured in utils.ml *)
let print_sees ppt mach_list =
  if (List.length mach_list) = 0 then () 
  else 
    fprintf ppt "SEES %a" print_idlist_comma mach_list

let print_id_machine ppt id_machine =
  fprintf ppt "%s" id_machine

let print_machine ppt b_sig =
  fprintf ppt
    "MACHINE %a@\n%a@\n%a @\nEND"
    print_id_machine b_sig.machine
    print_sees b_sig.sig_sees
    print_operation b_sig.sig_operation

let print_prog b_sig file =
  fprintf (formatter_of_out_channel file) "%a@." print_machine b_sig
