(* Florian Thibord  --  Projet CERCLES *)

open Lexing
open Ast_repr_b
open Ast_prog
open Utils 

let usage = "usage: "^Sys.argv.(0)^" [options] dir/project/"


let handle_error (start,finish) =
  let line = start.pos_lnum in
  let first_char = start.pos_cnum - start.pos_bol + 1 in
  let last_char = finish.pos_cnum - start.pos_bol + 1 in
  Printf.eprintf "line %d, characters %d-%d:\n" line first_char last_char


let parse_only = ref false
let norm_only = ref false
let verbose = ref false


let spec =
  ["-parse-only", Arg.Set parse_only, "stops after parsing";
   "-norm-only", Arg.Set norm_only, "stops after normalization";
   "-v", Arg.Set verbose, "print intermediate transformations";
  ]


let scade_file, xml_file, main_dir =
  let dir = ref None in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> dir := Some s
    | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  (match !dir with 
    Some n -> n^"KCG/kcg_xml_filter_out.scade", n^"KCG/kcg_trace.xml", n^"Machines_B/"
  | None -> Arg.usage spec usage; exit 1)

(* MAIN *)
let () =
  
  (* R�cup�ration des noms de noeuds, et des noeuds import�s pour chaque noeud, � partir du xml *) 
  let channel = open_in xml_file in
  let lexbuf = Lexing.from_channel channel in
  let xml_map =
    try
      Parser_xml.model Lexer_xml.token lexbuf
    with
      | Lexer_xml.Lexical_error s ->
	  Format.eprintf "Lexical Error XML: %s\n@." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
      | Parsing.Parse_error ->
	  Format.eprintf "Syntax Error XML\n@.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
  in
  close_in channel;
  
  (* R�cup�ration d'une map de noeuds qui sont index�s par leur nom, ainsi qu'une map de constantes �galement index�es par leur nom. *)
  let channel = open_in scade_file in
  let lexbuf = Lexing.from_channel channel in
  let prog = 
    try
      Parser_prog.prog Lexer_prog.token lexbuf
    with
      | Lexer_prog.Lexical_error s ->
	  Format.eprintf "Lexical Error kcg file: %s\n@." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	exit 1
      | Parsing.Parse_error ->
	  Format.eprintf "Syntax Error kcg file \n@.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
  in
  close_in channel;

  

  (* Traduction de chaque noeud du programme *)
  let node_translator node_name node =
    let import_list = try 
      XML_prog.find node_name xml_map 
    with Not_found -> []
    in
   (* ne pas oublier const_list dans normalizer *)
    let lexbuf = Lexing.from_string node in
    try
      let ast = Parser.prog Lexer.token lexbuf in
      let ast_n = Normalizer.normalize_node ast prog.const_list in
      let ast_b = Trad.translate ast_n in
      let babst_file = open_out (Filename.concat (Filename.dirname main_dir) (String.capitalize(node_name^".mch"))) in
      Babst_generator.print_prog ast_b.machine_abstraite babst_file;
      Printf.printf "%s" (Filename.concat (main_dir) (String.capitalize(node_name^".mch")));
      let bimpl_file = open_out (Filename.concat (Filename.dirname main_dir) (String.capitalize(node_name^"_i.imp"))) in
      Bimpl_generator.print_prog ast_b.implementation bimpl_file;
      close_out babst_file;
      close_out bimpl_file
    with
      | Lexer.Lexical_error s ->
	  Format.eprintf "Lexical Error: %s\n@." s;
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
      | Parsing.Parse_error ->
	  Format.eprintf "Syntax Error \n@.";
	  handle_error (lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	  exit 1
      | Normalizer.Assert_id_error e ->
	  Format.eprintf "Error: Assert  %s.\n@." e
      | Normalizer.Ident_Call_Error e ->
	  Format.eprintf "Error: The node name %s is reserved in B.\n@." e
      | Trad.Register_cond_error e ->
	  Format.eprintf "Register condition error: %s isn't related to an input/output \n@." e
      | e ->
	  Format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	  exit 2
  in
  T_Node.iter (fun name node -> if XML_prog.mem name xml_map then node_translator name node) prog.node_map
