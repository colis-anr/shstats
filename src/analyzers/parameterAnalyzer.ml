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
      finds the files that contain a parameter that is neither a special
      parameter nor a positional parameter. This might be useful to estimate
      how many files have parameters that have not been expanded by the
      expander. 
    *)
   
let unWord word = Morbig.CSTHelpers.unWord word
let unName' name' = Morbig.CSTHelpers.unName name'.value

let options = []
            
let name = "parameters"

let identifiers = ((Hashtbl.create 13): (string,int) Hashtbl.t)
let register_identifier x =
  if Hashtbl.mem identifiers x
  then Hashtbl.replace identifiers x (1+(Hashtbl.find identifiers x))
  else Hashtbl.add identifiers x 1

let scripts_with_parameter = ref ([]: string list)

let process_script filename cst =
  let detect_parameter =
    object (self)
      inherit [_] Morbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word_component for_variables = function
        | WordVariable (VariableAtom(s,_)) ->
           let re_special_or_positional_parameter =
             (* special parameters: see section 2.5.2 of POSIX standard *)
             (* positional parameters: see section 2.5.1 of POSIX standard *)
             Str.regexp "^\\([-@*!?$#]\\|[0-9]+\\)$"
           in
           if not (Str.string_match re_special_or_positional_parameter s 0)
           then
             if not (List.mem s for_variables)
             then begin
                 register_identifier s;
                 true
               end
             else (* this a loop variable *)
               false
           else (* this is a special or positional parameter *)
             false
        | x -> super#visit_word_component for_variables x

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
  if detect_parameter#visit_program [] cst
  then 
    scripts_with_parameter := filename::!scripts_with_parameter

let output_report report =
  Report.add report "* Number of scripts with $ after expansion: %d\n"
    (List.length !scripts_with_parameter);
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
    !scripts_with_parameter

  
              
