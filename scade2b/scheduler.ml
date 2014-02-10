(* Florian Thibord  --  Projet CERCLES *)

open Ast_base
open Ast_scade_norm
open Utils

module L = Set.Make(
  struct
    type t = ident
    let compare = compare
  end
)

module EQs = Set.Make(
  struct 
    (* equation, left_part ids, expression ids *)
    type t = n_equation * L.t * L.t
    let compare = compare 
  end
)

(* Retrieve all the idents in a n_expression *)
let ident_of_expr expr =
  let id = ref L.empty in 
  let rec idexpr_rec = function
    | NE_Ident iden -> id := L.add iden !id
    | NE_Value v -> ()
    | NE_Op_Arith1 (_, e) -> idexpr_rec e
    | NE_Op_Arith2 (_, e1, e2)
    | NE_Op_Relat (_, e1, e2) -> idexpr_rec e1;idexpr_rec e2
    | NE_Op_Logic (_, e1, e2) -> idexpr_rec e1;idexpr_rec e2
    | NE_Op_Sharp e_list -> List.iter idexpr_rec e_list
    | NE_Op_Not e -> idexpr_rec e
    | NE_Array array -> idarray_rec array
  and idarray_rec = function
    | NA_Def elist -> List.iter idexpr_rec elist
    | NA_Caret (e1, e2) -> idexpr_rec e1; idexpr_rec e2 
    | NA_Concat (e1, e2) -> idexpr_rec e1; idexpr_rec e2 
    | NA_Slice (iden, l) -> id := L.add iden !id;
      List.iter (fun (e1, e2) -> idexpr_rec e1; idexpr_rec e2) l
    | NA_Index (iden, l) -> id := L.add iden !id;
      List.iter idexpr_rec l
  in
  idexpr_rec expr;
  !id

let ident_of_left = function
  | NLP_Ident id -> L.add id L.empty
  | NLP_Tuple idl -> List.fold_left (fun s id ->  L.add id s) L.empty idl

let ident_of_eq = function
  | N_Alternative a ->
    let ident_expr = 
      L.union (L.union (ident_of_expr a.n_alt_cond) (ident_of_expr a.n_alt_then)) (ident_of_expr a.n_alt_else)
    in
    (ident_of_left a.n_alt_lp, ident_expr)
  | N_Registre r ->
    (L.add r.n_reg_lpid L.empty, L.empty)
  | N_Call f ->
    (ident_of_left f.n_fun_lp, List.fold_left (fun set e -> L.union set (ident_of_expr e)) L.empty f.n_fun_params)
  | N_Operation o ->
    (ident_of_left o.n_op_lp, ident_of_expr o.n_op_expr)



let retrieve_reg_ids eqs =
  let ids_of_reg = function
    | (N_Registre _, lp_ids, _) -> L.elements lp_ids
    | _ -> []
  in 
  List.fold_left (fun ids eq -> (ids_of_reg eq)@ids) [] eqs


(*
  1- recupération des idents a gauche et à droite de chaque eq
  2- récupération des ids des registres
  3- formation de l'ensemble des ids connus (ids params_in + ids registres) : l
  4- appel de schedul_rec [] l eqs:
  Pour chaque eq, si les ids a droite sont tous dans l, on place eq dans res et ses ids à gauche dans l             
*)
let scheduler eqs inputs =
  let rec schedul_rec res l eqs =
    if EQs.is_empty eqs then res else begin
      let pred = fun l e_idset -> L.is_empty (L.diff e_idset l) in
      let (ok,nok) =
	EQs.fold
	  (fun ((_, _, e_idset) as eq) (ok,nok) ->
	     if pred l e_idset then (eq::ok, nok) else (ok, EQs.add eq nok)
	  ) eqs ([], EQs.empty) in
      let res' = List.fold_left (fun r (eq, _, _) -> eq::r) res ok in
      let l' = List.fold_left (fun s (_, l_idset, _) -> L.union s l_idset) l ok in
      let eqs' = nok in
      (* Stop loop if there is no change *)
      if res' = res && l' = l && eqs'= eqs then failwith "scheduler issue... causality loop?" else ();
      schedul_rec res' l' eqs'
    end
  in
  let eqs = List.fold_left (fun acc eq -> let (left_set, expr_set) = ident_of_eq eq in
					  EQs.add (eq, left_set, expr_set) acc) EQs.empty eqs in
  let id_registres = retrieve_reg_ids (EQs.elements eqs) in
  let l = List.fold_left (fun acc id -> L.add id acc) L.empty (inputs@id_registres) in
  List.rev (schedul_rec [] l eqs)
