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
      finds words that contain ".."
    *)
   
let unWord word = Morbig.CSTHelpers.unWord word
let unName' name' = Morbig.CSTHelpers.unName name'.value

let options = []
            
let name = "dotdotPath"

let scripts_with_dotdot = ref ([]: string list)

let process_script filename cst =
  let detect_dotdot =
    object (self)
      inherit [_] Morbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word for_variables word =
        let re_dotdot = Str.regexp "\\.\\." in
        let s = (Morbig.remove_quotes (unWord word)) in
        Str.string_match re_dotdot s 0
    end
  in
  if detect_dotdot#visit_program [] cst
  then 
    scripts_with_dotdot := filename::!scripts_with_dotdot

let output_report report =
  Report.add report "* Number of scripts with '..' after expansion: %d\n"
    (List.length !scripts_with_dotdot);
  Report.add report "** Files:\n";
  List.iter
    (function scriptname ->
       Report.add report "    - %s\n" (Report.link_to_source report scriptname))
    !scripts_with_dotdot

  
              
