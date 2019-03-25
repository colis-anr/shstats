(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
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
and files       = ((Hashtbl.create 13): (string,int) Hashtbl.t)
let register_identifier x f =
  if Hashtbl.mem identifiers x
  then Hashtbl.replace identifiers x (1+(Hashtbl.find identifiers x))
  else Hashtbl.add identifiers x 1;
  if Hashtbl.mem files f
  then Hashtbl.replace files f (1+(Hashtbl.find files f))
  else Hashtbl.add files f 1

let expandable_word w =
    MoreCSTHelpers.contains_parameter w
    || MoreCSTHelpers.contains_glob w
    || MoreCSTHelpers.contains_subshell w

let rec expandable_wordlist = function
  | WordList_WordList_Word(wordlist',word') ->
     expandable_wordlist wordlist'.value || expandable_word word'.value
  | WordList_Word(word') -> expandable_word word'.value
  
let process_script filename cst =
  let detect_parameter =
    object (self)
      inherit [_] Morbig.CST.iter as super
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
             then register_identifier s filename;
        | x -> super#visit_word_component for_variables x

      method! visit_for_clause for_variables = function
        | ForClause_For_Name_DoGroup(name',do_group')
          | ForClause_For_Name_SequentialSep_DoGroup
              (name',_,do_group')
          | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup 
              (name',_,_,do_group') ->
           self#visit_do_group' ((unName' name')::for_variables) do_group'
        | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup
          (name',_,word_list',_,do_group') ->
           self#visit_do_group'
             (if expandable_wordlist word_list'.value
              then for_variables
              else (unName' name')::for_variables)
             do_group'
    
    end
  in detect_parameter#visit_program [] cst

let output_report report =
  Report.add report "* Number of scripts with $ after expansion: %d\n"
    (Hashtbl.length files);
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
  Hashtbl.iter
    (fun scriptname _ ->
       Report.add report "    - %s\n" (Report.link_to_source report scriptname))
    files

  
              
