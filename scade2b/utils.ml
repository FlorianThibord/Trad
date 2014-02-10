(* Florian Thibord  --  Projet CERCLES *)

open Ast_repr_b
open Ast_scade_norm
open Ast_scade
open Ast_base
open Ast_kcg
open Ast_prog
open Ast_xml
open Xml_utils

(*************************** IDENT COLLISION FUNCTIONS ***************************)

exception Underscore of string
exception Reserved of string
exception Character of string
exception Collision of string

let reserved_b_words = ["ABSTRACT_CONSTANTS"; "ABSTRACT_VARIABLES"; "ANY"; "ASSERT"; "ASSERTIONS"; "BE"; "BEGIN"; "BOOL"; "CASE"; "CHOICE"; "CONCRETE_CONSTANTS"; "CONCRETE_VARIABLES"; "CONSTANTS"; "CONSTRAINTS"; "DEFINITIONS"; "DO"; "EITHER"; "ELSE"; "ELSIF"; "END"; "EXTENDS"; "FALSE"; "FIN"; "FIN1"; "IF"; "IMPLEMENTATION"; "IMPORTS"; "IN"; "INCLUDES"; "INITIALISATION"; "INT"; "INTEGER"; "INTER"; "INVARIANT"; "LET"; "LOCAL_OPERATIONS"; "MACHINE"; "MAXINT"; "MININT"; "NAT"; "NAT1"; "NATURAL"; "NATURAL1"; "OF"; "OPERATIONS"; "OR"; "PI"; "POW"; "POW1"; "PRE"; "PROMOTES"; "PROPERTIES"; "REFINES"; "REFINEMENT"; "SEES"; "SELECT"; "SETS"; "SIGMA"; "STRING"; "THEN"; "TRUE"; "UNION"; "USES"; "VALUES"; "VAR"; "VARIANT"; "VARIABLES"; "WHEN"; "WHERE"; "WHILE"; "arity"; "bin"; "bool"; "btree"; "card"; "closure"; "closure1"; "conc"; "const"; "dom"; "father"; "first"; "fnc"; "front"; "id"; "infix"; "inter"; "iseq"; "iseq1"; "iterate"; "last"; "left"; "max"; "min"; "mirror"; "mod"; "not"; "or"; "perm"; "postfix"; "pred"; "prefix"; "prj1"; "prj2"; "ran"; "rank"; "rec"; "rel"; "rev"; "right"; "seq"; "seq1"; "size"; "sizet"; "skip"; "son"; "sons"; "struct"; "subtree"; "succ"; "tail"; "top"; "tree"; "union"]

let is_reserved ident = List.mem ident reserved_b_words

let is_b_compliant ident = 
  if String.length ident < 2 then raise (Character ident)
  else if ident.[0] = '_' then raise (Underscore ident)
  else if is_reserved ident then raise (Reserved ident)
  else true

let check_no_collision ident env =
  if (Env.exists (fun _ (bid, _, _) -> ident = bid) env) then raise (Collision ident)
  else true

let make_b_ident ident env = 
  let new_ident = ref "" in
  let switch_underscore s = 
    try 
      new_ident := (String.sub s 1 ((String.length s)-1));
      false
    with 
    | Invalid_argument _ -> failwith ("String error in switch_underscore(Utils) with " ^ s)
  in
  let double_character s = new_ident := s^s; false in
  let add_underscore s = new_ident := s^"_"; false in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision ident env)
    with
    | Underscore s ->
        switch_underscore s 
    | Character s ->
        double_character s
    | Reserved s ->
        add_underscore s
    | Collision s ->
        add_underscore s
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident

let make_env id_type_expr_list =
  List.fold_left
    (fun env (ident, typ, expr) -> Env.add ident ((make_b_ident ident env), typ, expr) env)
    Env.empty id_type_expr_list

let make_params_ident env lambda_list =
  List.fold_left (fun env lambda -> let (param, typ, expr) = Env.find lambda.n_l_ident env in
    Env.add lambda.n_l_ident ((String.uncapitalize param), typ, expr) env) env lambda_list



(******************** GLOBAL ENV CHECK *************************)

let check_no_collision_global ident env =
  if (Env.exists (fun _ bid -> ident = bid) env) then raise (Collision ident)
  else true

let make_b_ident_global ident env = 
  let new_ident = ref "" in
  let switch_underscore s = 
    try 
      new_ident := (String.sub s 1 ((String.length s)-1));
      false
    with 
    | Invalid_argument _ -> failwith ("String error in switch_underscore(Utils) with " ^ s)
  in
  let double_character s = new_ident := s^s; false in
  let add_underscore s = new_ident := s^"_"; false in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision_global ident env)
    with
    | Underscore s ->
        switch_underscore s 
    | Character s ->
        double_character s
    | Reserved s ->
        add_underscore s
    | Collision s ->
        add_underscore s
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  ident_generator ident



(******************** INSTANCES ENV BUILDER *************************)

let make_inst_ident inst =
  let node_name, import_name, inst_id = inst in
  try
    (String.sub node_name 0 1) ^ (String.sub import_name 0 1) ^ inst_id 
  with Invalid_argument _ -> Printf.printf " ERROR !!!!!! : %s %s" node_name import_name; node_name ^ import_name ^ inst_id

let check_no_collision_global ident env =
  if (Env_instances.exists (fun _ bid -> ident = bid) env) then raise (Collision ident)
  else true

let make_instance_bid instance env = 
  let new_ident = ref "" in
  let switch_underscore s = 
    try 
      new_ident := (String.sub s 1 ((String.length s)-1));
      false
    with 
    | Invalid_argument _ -> failwith ("String error in switch_underscore(Utils) with " ^ s)
  in
  let double_character s = new_ident := s^s; false in
  let add_underscore s = new_ident := s^"_"; false in
  let ident_check ident =
    try 
      (is_b_compliant ident) && (check_no_collision_global ident env)
    with
    | Underscore s ->
        switch_underscore s 
    | Character s ->
        double_character s
    | Reserved s ->
        add_underscore s
    | Collision s ->
        add_underscore s
  in
  let rec ident_generator ident =
    if ident_check ident then ident else ident_generator !new_ident
  in
  let ident = make_inst_ident instance in
  ident_generator ident




(*** RECONNAISSANCE DE L'INITIALISATION D'UN REGISTRE PAR UNE ENTREE ***)

(*                          1) Normalisation                           *)


let is_linked eqs ins v =
  let linked_fold = function
    N_Operation op -> (
      match op.n_op_lp, op.n_op_expr with 
	  NLP_Ident lp, NE_Ident i when lp = v -> 
	    if List.exists (fun (id, typ) -> id = i) ins then Some i else None
	| _ -> None )
    | _ -> None
  in
  List.fold_left (fun id eq -> match linked_fold eq with Some i -> Some i | None -> id) None eqs


let remove_from_ins ins i =
  let index_ref = ref 0 in
  let ins, _ = List.fold_left (fun (acc, index) (ident, typ) -> 
				 if i = ident then (index_ref := index; (acc, index + 1))
				 else ((ident, typ) :: acc, index + 1)
			      ) ([], 0) ins in
  ins, !index_ref

let remove_from_pres pres i =
  let cond_ref = ref ("", NT_Base T_Bool, None) in
  let pres =  List.fold_left (fun acc ((ident, _ , _) as pre) -> 
		    if i = ident then (cond_ref := pre; acc) 
		    else pre :: acc) [] pres in
  pres, !cond_ref
    
let build_lambda cond index = 
  let id, _, _ = cond in
  { n_l_ident = id;
    n_l_cond = cond;
    n_l_index = index;
  }


let search_input_in_reg eqs ins pres lambdas =
  let eqs_out = ref [] in
  let rec fun_rec eqs ins pres lambdas =
    match eqs with
    | [] -> (ins, pres, lambdas, (List.rev !eqs_out))
    | eq :: eqs_bis -> begin
      match eq with
      | N_Registre reg -> (
	match reg.n_reg_ini with
	  NE_Ident v -> (
	    match is_linked eqs ins v with
	      Some i ->
		let pres, cond = remove_from_pres pres i in
		let ins, index = remove_from_ins ins i in
		eqs_out := (N_Registre {reg with n_reg_ini = NE_Ident i}) :: !eqs_out;
		fun_rec eqs_bis ins pres ((build_lambda cond index) :: lambdas)
	    | None ->
	      eqs_out := eq :: !eqs_out;
	      fun_rec eqs_bis ins pres lambdas
	    )
	| _ ->
	  eqs_out := eq :: !eqs_out;
	  fun_rec eqs_bis ins pres lambdas)
      | _ as eq ->
	eqs_out := eq :: !eqs_out;
	fun_rec eqs_bis ins pres lambdas
    end
  in
  fun_rec eqs ins pres lambdas


	
(*                          2) Traduction                           *)


let print_imports_out imps =
  List.iter (fun imp -> match imp.b_params_expr with 
		 None -> Printf.printf "\nTTT %s nope %s " imp.b_import_name imp.b_instance_id
	       | Some a -> Printf.printf "\nTTT %s %d %s" imp.b_import_name (List.length a) imp.b_instance_id ) imps

let print_imports_in imps =
  List.iter (fun imp -> Printf.printf "\n  name : %s  id : %s  \n  " imp.import_name imp.instance_id) imps 

let check_imports_params imports eqs =
  let imports_out = ref [] in
  (* print_imports_in imports; *)
  let find_in_imp_list imp_id inst_id =
    List.find (fun imp -> (imp.import_name = imp_id) && (imp.instance_id = inst_id) ) imports
  in
  let cip_fun_rec eq =
    match eq with
      Call c -> (
	let imp = find_in_imp_list c.call_id c.call_instance in
	let params_index_list, instname = imp.params_index, imp.instance_id in
	match params_index_list with 
	  | None -> 
	      imports_out := { b_import_name = imp.import_name; 
			       b_params_expr = None;
			       b_instance_id = instname; } :: !imports_out; eq
	  | Some param_index_list ->
	      if (List.length param_index_list) = 0 then (
		imports_out :=  { b_import_name = imp.import_name; 
				  b_params_expr = None;
				  b_instance_id = instname; } :: !imports_out; eq)
	      else (
		if instname = c.call_instance then
		  ( 
		    let _, params_op, params_m =
		      List.fold_left (fun (index, params_op, params_m) param_expr -> 
					if List.mem index param_index_list then
					  (index+1, params_op, (List.nth c.call_params index) :: params_m)
					else
					  (index+1, (List.nth c.call_params index) :: params_op, params_m)
				     ) (0, [], []) c.call_params in
		    imports_out :=  { b_import_name = imp.import_name; 
				      b_params_expr = Some params_m;
				      b_instance_id = instname; } :: !imports_out;
		    Call {c with call_params = params_op}
		  )
		else (
		  imports_out := { b_import_name = imp.import_name; 
				   b_params_expr = None;
				   b_instance_id = instname; } :: !imports_out; eq)
	      )
      )
      | _ -> eq
  in
  (* print_imports_out !imports_out; *)
  (* Printf.printf " taille : %d " (List.length !imports_out); *)
  List.map cip_fun_rec eqs, !imports_out 



(*************************** DIVERS ***************************)


let sees_list env const_list =
  if List.exists (fun cst -> Env.mem cst env) const_list then ["M_Consts"; "M_Enum"] else ["M_Enum"]

(* a_b_list_equals (l: ('a, 'a) list) returns true if a = b for every pairs *)
let a_b_list_equals l=
  List.for_all (fun (a, b) -> a = b) l

(* string_of_list (l: string list) returns the concat of every strings in list *)
(* NOT USED *)
let string_of_list l = 
  List.fold_left (fun acc str -> str^" "^acc ) "" l



(* Find the type related to a variable (dans normalizer) *)
let rec find_type id declist =
  match declist with
  | [] -> None
  | (ident, typ)::l -> if ident = id then Some typ else find_type id l

(* caret_to_def v n returns [v; ...; v] with n index  *)
and caret_to_def e1 e2 = 
  let rec funrec v dim acc =
    if dim = 0 then acc
    else funrec v (dim-1) (v :: acc)
  in
  NA_Def (funrec e1 e2 [])



exception Two_ident of (string * string)

(* Find an ident in an expr. Used in handle_assume/guarantee (normalizer), find the ident linked to a condition *)
let find_ident_in_pexpr expr consts =
  Printf.printf "\nPASSE:";
  List.iter (fun c -> Printf.printf " %s " c) consts;
  let is_not_const id = not(List.mem id consts) in
  let id = ref "" in
  let rec ident_finder = function
    | PE_Ident iden -> if (!id <> iden && (is_not_const iden) && (is_not_const !id)) 
      then raise (Two_ident (!id, iden)) else Printf.printf "\n%s" iden; id := iden
    | PE_Value v -> ()
    | PE_Array array -> idarray_finder array
    | PE_Call (_, _, elist) -> List.iter ident_finder elist
    | PE_Op_Arith1 (_, e) -> ident_finder e
    | PE_Op_Arith2 (_, e1, e2)
    | PE_Op_Relat (_, e1, e2) -> ident_finder e1;ident_finder e2
    | PE_Op_Sharp elist -> List.iter ident_finder elist
    | PE_Op_Not e -> ident_finder e
    | PE_Op_Logic (_, e1, e2) -> ident_finder e1 ; ident_finder e2
    | PE_Fby (e1, e2, e3) -> ident_finder e1; ident_finder e2; ident_finder e3
    | PE_If (e1, e2, e3) -> ident_finder e1; ident_finder e2; ident_finder e3
  and idarray_finder = function
    | PA_Def elist -> List.iter ident_finder elist
    | PA_Caret (e1, e2) -> ident_finder e1; ident_finder e2 
    | PA_Concat (e1, e2) -> ident_finder e1; ident_finder e2 
    | PA_Slice (iden, _) -> if (!id <> "" && !id <> iden) then raise (Two_ident (!id, iden)) else id := iden
    | PA_Index (iden, _) -> if (!id <> "" && !id <> iden) then raise (Two_ident (!id, iden)) else id := iden
  in
  ident_finder expr;
  !id


(* Fonctions de rennomage *)
let rec rename_id_expr old new_i = function
  | NE_Ident i -> if i = old then NE_Ident new_i else NE_Ident i
  | NE_Value v -> NE_Value v
  | NE_Array ar -> NE_Array (rename_id_array old new_i ar)
  | NE_Op_Arith1 (op, e) -> NE_Op_Arith1 (op, rename_id_expr old new_i e)
  | NE_Op_Arith2 (op, e1, e2) -> NE_Op_Arith2 (op, rename_id_expr old new_i e1,
                                                   rename_id_expr old new_i e2)
  | NE_Op_Relat (op, e1, e2) -> NE_Op_Relat (op, rename_id_expr old new_i e1,
                                                 rename_id_expr old new_i e2)
  | NE_Op_Logic (op, e1, e2) -> NE_Op_Logic (op, rename_id_expr old new_i e1,
                                                 rename_id_expr old new_i e2)
  | NE_Op_Sharp (e_list) -> NE_Op_Sharp (List.map (rename_id_expr old new_i) e_list)
  | NE_Op_Not e -> NE_Op_Not (rename_id_expr old new_i e)

and rename_id_array old new_i = function
  | NA_Def e_list -> NA_Def (List.map (rename_id_expr old new_i) e_list)
  | NA_Caret (e1, e2) -> NA_Caret (rename_id_expr old new_i e1, rename_id_expr old new_i e2)
  | NA_Concat (e1, e2) -> NA_Concat (rename_id_expr old new_i e1, rename_id_expr old new_i e2)
  | NA_Slice (i, e_list) -> 
    NA_Slice ((if i = old then new_i else i), 
	      (List.map (fun (e1, e2) ->
		(rename_id_expr old new_i e1, rename_id_expr old new_i e2)) e_list))
  | NA_Index (i, e_list) -> 
    NA_Index ((if i = old then new_i else i), 
	      (List.map (rename_id_expr old new_i) e_list))




(* ast_repr to ast_repr_b functions *)
let rec p_expr_to_b_expr = function
  | PE_Ident id ->  BE_Ident id
  | PE_Value v -> BE_Value v
  | PE_Array ar -> BE_Array (p_array_to_b_array ar)
  | PE_Op_Arith1 (op, e) -> BE_Op_Arith1 (op, p_expr_to_b_expr e)
  | PE_Op_Arith2 (op, e1, e2) -> BE_Op_Arith2 (op, p_expr_to_b_expr e1, p_expr_to_b_expr e2)
  | PE_Op_Relat (op, e1, e2) -> BE_Pred (BP_Op_Relat (op, p_expr_to_b_expr e1, p_expr_to_b_expr e2))
  | _ -> assert false

and p_array_to_b_array = function
  | PA_Def e_list -> BA_Def (List.map p_expr_to_b_expr e_list)
  | PA_Caret (e1, PE_Value (Int i)) -> p_array_to_b_array (caret_to_def_bis e1 i)
  | PA_Caret (e1, e2) -> BA_Caret (p_expr_to_b_expr e1, p_expr_to_b_expr e2)
  | PA_Concat (e1, e2) -> BA_Concat (p_expr_to_b_expr e1, p_expr_to_b_expr e2)
  | PA_Slice (id, e_list) -> 
      BA_Slice (id, 
		(List.map (fun (e1, e2) -> 
			     (p_expr_to_b_expr e1, p_expr_to_b_expr e2)) e_list))
  | PA_Index (id, e_list) -> 
      BA_Index (id, (List.map p_expr_to_b_expr e_list))

and caret_to_def_bis e1 e2 = 
  let rec funrec v dim acc =
    if dim = 0 then acc
    else funrec v (dim-1) (v :: acc)
  in
  PA_Def (funrec e1 e2 [])

let p_const_to_b_const const =
  let flatten_array a =
    let base_t = ref T_Bool in (* default ref *)
    let rec fun_rec = function
      | PT_Base t -> base_t := t; []
      | PT_Array (t, expr) -> (p_expr_to_b_expr expr) :: (fun_rec t)
    in
    (!base_t, fun_rec a)
  in
  let (id, t, e) = (const.c_id, const.c_typ, const.c_expr) in 
  match t with 
    | PT_Base typ -> Const_Base (id, typ, p_expr_to_b_expr e)
    | PT_Array (_, _) -> (
      let typ, dims = flatten_array t in 
      Const_Fun (id, typ, dims, p_expr_to_b_expr e))



let generate_error_machine node_xml main_dir =
  let node_name = node_xml.xml_node_name in
  let babst_err =
    open_out (Filename.concat main_dir ("M_" ^ node_name ^ ".mch")) in
  Babsterror_generator.print_machine_base node_xml babst_err;
  close_out babst_err
