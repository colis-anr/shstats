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

let options = []

let name = "wordNesting"

let max_depth = ref 0
let most_nested_word = ref ""
let script_with_most_nested_word = ref ""

let process_script filename csts =
  let nesting_counter =
    object(self)
      inherit [_] Morbig.CST.reduce as super
      method zero = 0
      method plus = max
      method! visit_word _env (Word(s,wcst)) =
        self#visit_word_cst s wcst
      method! visit_word_component env w =
        let depth = match w with
          | WordDoubleQuoted word | WordSingleQuoted word ->
             1 + self#visit_word env word
          | WordSubshell (_subshell_type,program') ->
             1 + self#visit_program' env program'
          | x -> super#visit_word_component env x
        in
        if depth > !max_depth then
          begin
            max_depth := depth;
            most_nested_word := env;
            script_with_most_nested_word := filename
          end;
        depth
    end
  in let _ = nesting_counter#visit_program "" csts in ()
  
let output_report report =
  Report.add report "* Maximal word nesting level: %i\n" !max_depth;
  Report.add report "** Nested word: \n%s\n" !most_nested_word;
  Report.add report "\n ** Occuring in:\n";
  Report.add report "%s\n"
    (Report.link_to_source report !script_with_most_nested_word); 
  Report.add report "\n"
