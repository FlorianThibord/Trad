(* Florian Thibord  --  Projet CERCLES *)

open Format
open Ast_kcg
open Ast_scade_norm
open Ast_prog
open Printer

let print_enum ppt enum =
  fprintf ppt "%s = {%a}" 
    enum.p_enum_id
    print_idlist_comma enum.p_enum_list

let rec print_enum_list ppt = function 
  | [] -> ()
  | [e] -> fprintf ppt "%a" print_enum e
  | e::l -> fprintf ppt "%a; @\n%a" print_enum e print_enum_list l 

let print_sets_clause ppt enum_list =
  if (List.length enum_list) = 0 then () 
  else 
    fprintf ppt "SETS %a" print_enum_list enum_list 

let print_machine ppt enum_list =
  fprintf ppt
    "MACHINE M_Enum@\n@\n%a@\n@\nEND"
    print_sets_clause enum_list

let print_m_enum enum_list file env_prog =
    with_env env_prog (fun () ->
        fprintf (formatter_of_out_channel file) "%a@." print_machine enum_list
    )
