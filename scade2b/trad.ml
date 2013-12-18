(* Florian Thibord  --  Projet CERCLES *)


open Ast_base
open Ast_repr_b
open Ast_scade_norm
open Utils


exception Register_cond_error of string



(******************** ast_repr_norm to ast_repr_b functions ********************)

let id_to_bid env id =
try
  let bid, _, _ = Env.find id env in bid
with Not_found -> failwith ("Identifier not found: '"^id^"'")

let rec n_expr_to_b_expr env = function
  | NE_Ident id ->  BE_Ident (id_to_bid env id)
  | NE_Value v -> BE_Value v
  | NE_Array ar -> BE_Array (n_array_to_b_array env ar)
  | NE_Op_Arith (op, e_list) -> BE_Op_Arith (op, (List.map (n_expr_to_b_expr env) e_list))
  | NE_Op_Logic (op, e_list) -> BE_Op_Logic (op, (List.map (n_expr_to_b_expr env) e_list))

and n_array_to_b_array env = function
  | NA_Def e_list -> BA_Def (List.map (n_expr_to_b_expr env) e_list)
  | NA_Caret (e1, NE_Value (Int i)) -> n_array_to_b_array env (Utils.caret_to_def e1 i)
  | NA_Caret (e1, e2) -> BA_Caret (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | NA_Concat (e1, e2) -> BA_Concat (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)
  | NA_Slice (id, e_list) -> 
      BA_Slice (id_to_bid env id, 
		(List.map (fun (e1, e2) -> 
			     (n_expr_to_b_expr env e1, n_expr_to_b_expr env e2)) e_list))
  | NA_Index (id, e_list) -> 
      BA_Index (id_to_bid env id, (List.map (n_expr_to_b_expr env) e_list))

let nlp_to_blp env = function
  | NLP_Ident id -> BLP_Ident (id_to_bid env id)
  | NLP_Tuple id_list -> BLP_Tuple (List.map (id_to_bid env) id_list)

let n_decl_to_decl env (id, _) =
  id_to_bid env id

let n_condition_to_condition env (id, t, e) =
  (* flatten a NT_Array into a n_expr list (a list of dimensions) *)
  let flatten_array env a =
    let base_t = ref T_Int in (* default ref *)
    let rec fun_rec = function
      | NT_Base t -> base_t := t; []
      | NT_Array (t, expr) -> (n_expr_to_b_expr env expr) :: (fun_rec t)
    in
    (!base_t, fun_rec a)
  in
  let compr_ens_ident = Utils.make_b_ident "ii" env in
  match t with 
    | NT_Base typ ->  (
	match e with 
	  | None -> Base_no_expr (id_to_bid env id, typ, compr_ens_ident)
	  | Some expr -> 
	      let env2 = Env.add id (compr_ens_ident, t, None) env in
	      Base_expr (id_to_bid env id, typ, n_expr_to_b_expr env2 expr, compr_ens_ident))
    | NT_Array (_, _) -> (
	let typ, dims = flatten_array env t in
	(* let index_tab_ident = Utils.make_b_ident "jj" env in *)
	match e with 
	  | None -> Fun_no_expr (id_to_bid env id, typ, dims, compr_ens_ident)
	  | Some expr -> 
	      let env2 = Env.add id (compr_ens_ident, t, None) env in
	      Fun_expr (id_to_bid env id, typ, dims, n_expr_to_b_expr env2 expr, compr_ens_ident))
	



(******************** Traduction des registres ********************)

let get_concrete_vars env reg = 
  id_to_bid env reg.n_reg_lpid

let retrieve_cond_expr reg node env =
  let params_in, params_out, eqs = node.n_param_in, node.n_param_out, node.n_eqs in
  let id_var = match reg.n_reg_val with
    | NE_Ident i -> i
    | _ -> assert false
  in
  let eqs_folder condition eq =
    match eq with
      | N_Operation op -> begin
	  match op.n_op_lp, op.n_op_expr with
	    | NLP_Ident idl, NE_Ident ide -> begin
		try 
		  let id_in, _ = List.find (fun (p_in, _) ->
					      p_in = ide && idl = id_var) params_in in
		  match Env.find id_in env with
		    | _, _, Some c -> Some (Utils.rename_id_expr id_in reg.n_reg_lpid c) 
		    | _, _, None -> raise Not_found
		with Not_found -> try
		  let id_out, _ = List.find (fun (p_out, _) -> 
					       p_out = idl && ide = id_var) params_out in
		  match Env.find id_out env with
		    | _, _, Some c -> Some (Utils.rename_id_expr id_out reg.n_reg_lpid c)
		    | _, _, None -> raise Not_found
		with Not_found -> condition
	      end
	    | _ -> condition
	end
      | _ -> condition
  in
  List.fold_left eqs_folder None eqs
  
let get_invariant env node reg =
  n_condition_to_condition env (reg.n_reg_lpid, reg.n_reg_type, retrieve_cond_expr reg node env)
  
let get_initialisation env reg =
  (id_to_bid env reg.n_reg_lpid, n_expr_to_b_expr env reg.n_reg_ini)




(******************** DIVERS ********************)

let rec trad_list env to_call = function
  | [] -> []
  | elt::l -> (to_call env elt)::(trad_list env to_call l)




(******************** Traduction du noeud vers l'implantation ********************)

let bimpl_translator env node imports const_list =
  let implem_name = "M_" ^ node.n_id ^ "_i" in
  let params_id = List.map (fun l -> l.n_l_ident) node.n_lambdas in
  let refines = "M_" ^ node.n_id in
  let sees = Utils.sees_list env const_list in
  let concrete_vars = ref [] in
  let invariant = ref [] in
  let initialisation = ref [] in
  let translate_eqs env eqs =
    let translator = function
      | N_Alternative a ->
	Alternative { alt_lp = nlp_to_blp env a.n_alt_lp;
		      alt_cond = n_expr_to_b_expr env a.n_alt_cond;
		      alt_then = n_expr_to_b_expr env a.n_alt_then;
		      alt_else = n_expr_to_b_expr env a.n_alt_else;
    		    }
      | N_Call f ->
	Call { call_lp = nlp_to_blp env f.n_fun_lp;
	       call_id = f.n_fun_id;
	       call_params = List.map (n_expr_to_b_expr env) f.n_fun_params;
	       call_instance = f.n_fun_pragma;
	     }
      | N_Operation o ->
	Op_Base { op_lp = nlp_to_blp env o.n_op_lp;
		  op_expr = n_expr_to_b_expr env o.n_op_expr;
		}
      | _ -> assert false
    in
    List.map translator eqs
  in
  let translate_regs env regs =
    let translator = function
      | N_Registre r ->
	concrete_vars := (get_concrete_vars env r):: !concrete_vars;
	invariant := (get_invariant env node r):: !invariant;
	initialisation := (get_initialisation env r):: !initialisation;
	{ reg_lpid = id_to_bid env r.n_reg_lpid;
	  reg_val =  n_expr_to_b_expr env r.n_reg_val;
	}
      | _ -> assert false
    in
    List.map translator regs
  in
  let op_decl = { id = node.n_id;
		  param_in = trad_list env n_decl_to_decl node.n_param_in;
		  param_out = trad_list env n_decl_to_decl node.n_param_out;
		} in
  let (eqs, regs) = List.partition
    (fun eq -> match eq with N_Registre _ -> false | _ -> true) node.n_eqs in
  let op_1 = translate_eqs env eqs in
  let op_2 = translate_regs env regs in
  let op_1, imports = Utils.check_imports_params imports op_1 in 
  let reg_ids = !concrete_vars in
  let vars = trad_list env n_decl_to_decl node.n_vars in
  let vars_without_regs =
    List.filter (fun id -> not(List.mem id reg_ids)) vars in
  let operation = { op_decl = op_decl;
		     vars = vars_without_regs;
		     op_1 = op_1;
		     op_2 = op_2;
		   } in
  { name = implem_name;
    params = List.map (id_to_bid env) params_id;
    refines = refines;
    sees = sees;
    imports = imports;
    concrete_variables = !concrete_vars;
    invariant = !invariant;
    initialisation = !initialisation;
    operation = operation;
  }




(******************** Traduction du noeud vers la machine abstraite ********************)

let babst_translator env node const_list =
  let machine = "M_" ^ node.n_id in
  let params_id, params_cond = 
    List.split (List.map (fun lambda -> lambda.n_l_ident, lambda.n_l_cond) node.n_lambdas) in
  let sees = Utils.sees_list env const_list in
  let constraints = trad_list env n_condition_to_condition params_cond in
  let abstop_decl = { id = node.n_id;
		      param_in = trad_list env n_decl_to_decl node.n_param_in;
		      param_out = trad_list env n_decl_to_decl node.n_param_out;
		    } in 
  let abstop_pre = trad_list env n_condition_to_condition node.n_pre in
  let abstop_post = trad_list env n_condition_to_condition node.n_post in
  let abst_operation = { abstop_decl = abstop_decl;
			 abstop_pre = abstop_pre;
			 abstop_post = abstop_post;
		       } in
  { machine = machine;
    abst_params = List.map (id_to_bid env) params_id;
    abst_constraints = constraints;
    abst_sees = sees;
    abst_operation = abst_operation;
  }




(******************** Traduction du noeud en un couple de machines B ********************)

let translate node imports const_list =
  let env = node.n_env in
  let babst = babst_translator env node const_list in
  let bimpl = bimpl_translator env node imports const_list in
  { machine_abstraite = babst;
    implementation = bimpl;
  }
