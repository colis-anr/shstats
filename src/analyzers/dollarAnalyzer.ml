(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Libmorbig.CST
open Options
open Commands
open Messages

   (* 
      finds the files that contain a '$ symbol in some word. This might
      be useful to estiamte how many files have parameters that have not
      been expanded by the expander. 
    *)
   
let unWord word = Libmorbig.CSTHelpers.unWord word

let options = []
            
let name = "dollar"

let identifiers = ((Hashtbl.create 13): (string,int) Hashtbl.t)
let register_identifier x =
  if Hashtbl.mem identifiers x
  then Hashtbl.replace identifiers x (1+(Hashtbl.find identifiers x))
  else Hashtbl.add identifiers x 1

let scripts_with_dollar = ref ([]: string list)

let process_script filename cst =
  let detect_dollar =
    object (self)
      inherit [_] Libmorbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word _env word =
        let re_parname =
          "[a-zA-Z_][a-zA-Z_0-9@*?$!]*" in
        let re_parameter =
          Str.regexp ("\\$\\(" ^ re_parname ^ "\\)" ^
                        "\\|\\${\\(" ^ re_parname ^ "\\)}") in
        (* does not match stuff like $1, $2, $@, etc on purpose *)
        let s = (UnQuote.on_string (unWord word)) in
        if
          Str.string_match re_parameter s 0
        then begin
            (try
              register_identifier (Str.matched_group 1 s)
            with
              Not_found -> register_identifier (Str.matched_group 2 s));
            true
          end
        else
          false
    end
  in
  if detect_dollar#visit_complete_command_list () cst
  then 
    scripts_with_dollar := filename::!scripts_with_dollar

let output_report report =
  Report.add report "* Number of scripts with $ after expansion: %d\n"
    (List.length !scripts_with_dollar);
  Report.add report "* Number of different parameters: %d\n"
    (Hashtbl.length identifiers);
  Report.add report "** Parameters\n";
  List.iter
    (function (k,v) -> Report.add report "    %6d | %s\n" v k)
    (List.sort
       (fun (k1,v1) (k2,v2) -> 1 - Pervasives.compare v1 v2)
       (Hashtbl.fold
          (fun key v listaccu -> (key,v)::listaccu)
          identifiers
          []
       ));
  Report.add report "** Files:\n";
  List.iter
    (function scriptname ->
       Report.add report "    - %s\n" (Report.link_to_source report scriptname))
    !scripts_with_dollar

  
              
