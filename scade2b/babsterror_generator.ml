(* Florian Thibord  --  Projet CERCLES *)


open Format
open Ast_repr_b
open Ast_base
open Ast_xml
open Xml_utils
open Printer

let print_type ppt typ =
  match typ with
    | "kcg_int" -> fprintf ppt "%s" "INT"
    | "kcg_real" -> fprintf ppt "%s" "REAL"
    | "kcg_bool" -> fprintf ppt "%s" "BOOL"
    | "array_int_2" -> fprintf ppt "%s" "(0 .. 1) --> INT"
    | "array_int_2_2" -> fprintf ppt "%s" "(0 .. 1, 0 .. 1) --> INT"
    | _ as a -> fprintf ppt "%s" a

let print_then_condition ppt output =
  fprintf ppt "%s :: { ii | ii : %a }" output.var_id print_type output.var_type

let print_pre_condition ppt input =
  fprintf ppt "%s : %a" input.var_id print_type input.var_type

let rec print_thenlist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_then_condition c
  | c::l -> fprintf ppt "%a||@,%a" print_then_condition c print_thenlist l 

let rec print_prelist ppt = function 
  | [] -> ()
  | [c] -> fprintf ppt "%a" print_pre_condition c
  | c::l -> fprintf ppt "%a &@,%a" print_pre_condition c print_prelist l 

let print_op_decl ppt xml_decl =
  let in_ids = List.map (fun var -> var.var_id) xml_decl.ins in
  let out_ids = List.map (fun var -> var.var_id) xml_decl.outs in
  fprintf ppt "%a <-- %s(%a)"
    print_idlist_comma out_ids
    xml_decl.xml_node_name
    print_idlist_comma in_ids

let print_operation ppt xml_decl =
  fprintf ppt 
    "OPERATIONS@\n@\n@[%a =@]@\n@[<v 3> PRE@,@[<v>%a@]@]@\n@[<v 3> THEN@,@[<v>%a@]@]@\n END"
    print_op_decl xml_decl
    print_prelist xml_decl.ins
    print_thenlist xml_decl.outs

let print_id_machine ppt id_machine =
  fprintf ppt "M_%s" id_machine


let print_machine ppt xml_decl =
  fprintf ppt
    "MACHINE %a@\n%a @\nEND"
    print_id_machine xml_decl.xml_node_name
    print_operation xml_decl


let print_machine_base xml_node file =
    fprintf (formatter_of_out_channel file) "%a@." print_machine xml_node
