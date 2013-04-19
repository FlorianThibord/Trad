(* Florian Thibord  --  Projet CERCLES *)

(* Extraction des registres, 
   V�rification que les op�rations sont atomiques?
   Diff�rentier pre/post conditions *)

open Ast_repr
open Ast_norm_repr


(* ATTENTION AUX TUPLES !!*) 

let handle_register eq node_out = ()


let handle_assert asser node_out = ()


(* Parcours de eq_list, et pour chaque eq :
   si Assert -> v�rifier que sur un seul ident, puis regarder si param_in ou param_out
   si FBY -> v�rifier que c'est : C -> pre X . Puis c�er registre x, type, C.
   V�rifier Atomicit�: en parcourant et en recr�ant l'arbre? 
*)
let folder eq_list node_out = ()

(* extraction du main *)
(* appel fold eq_list *)
let main (ast:prog) main =
  let main_node =
    try 
      List.find (fun node -> node.p_id = main) ast
    with Not_found -> assert false
  in
  let node_out = ref
    { n_id = main_node.p_id;
      n_param_in = main_node.p_param_in;
      n_param_out = main_node.p_param_out;
      n_vars = main_node.p_vars;
      n_reg = [];
      n_pre = [];
      n_post = [];
      n_eqs = []; } in
  let res = folder main_node.p_eqs node_out in
  ()
