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

let scripts_with_dollar = ref ([]: string list)

let process_script filename cst =
  let detect_dollar =
    object (self)
      inherit [_] Libmorbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word _env word =
        String.contains (UnQuote.on_string (unWord word)) '$'
    end
  in
  if detect_dollar#visit_complete_command_list () cst
  then 
    scripts_with_dollar := filename::!scripts_with_dollar

let output_report report =
  Report.add report "* Number of scripts with $ after expansion: %n\n"
    (List.length !scripts_with_dollar);
  Report.add report "** Files:\n";
  List.iter
    (function scriptname ->
       Report.add report "    - %s\n" (Report.link_to_source report scriptname))
    !scripts_with_dollar

  
              
