(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Morbig.CST
open Options
open Messages

   (* 
      finds the files that contain a '$ symbol in some word. This might
      be useful to estiamte how many files have parameters that have not
      been expanded by the expander. 
    *)
   
let unWord word = Morbig.CSTHelpers.unWord word
let unName' name' = Morbig.CSTHelpers.unName name'.value

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
      inherit [_] Morbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word for_variables word =
        let re_parname =
          "[a-zA-Z_][a-zA-Z_0-9@*?$!]*" in
        let re_parameter =
          Str.regexp ("\\$\\(" ^ re_parname ^ "\\)" ^
                        "\\|\\${\\(" ^ re_parname ^ "\\)}") in
        (* does not match stuff like $1, $2, $@, etc on purpose *)
        let s = (Morbig.API.remove_quotes (unWord word)) in
        if
          Str.string_match re_parameter s 0
        then
          let parameter_name =  
            try Str.matched_group 1 s
            with Not_found -> Str.matched_group 2 s
          in
          if not (List.mem parameter_name for_variables)
          then begin
              register_identifier parameter_name;
              true
            end
          else
            false
        else
          false

      method! visit_for_clause for_variables = function
        (* FIXME: consider only for loops for which the list is
           statistically known. *)
        | ForClause_For_Name_DoGroup(name',do_group')
          | ForClause_For_Name_SequentialSep_DoGroup
              (name',_,do_group')
          | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup 
              (name',_,_,do_group')
          | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup
              (name',_,_,_,do_group') ->
           self#visit_do_group' ((unName' name')::for_variables) do_group'
    
    end
  in
  if detect_dollar#visit_program [] cst
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

  
              
