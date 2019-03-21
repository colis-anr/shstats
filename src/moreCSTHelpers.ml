(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open Morbig.CST
open Morbig.CSTHelpers

let lnum located =
  located.position.start_p.pos_lnum
   
let rec words_of_suffix acc = function
  | CmdSuffix_IoRedirect _ ->
     acc
  | CmdSuffix_CmdSuffix_IoRedirect (s, _) ->
     words_of_suffix' acc s
  | CmdSuffix_Word w ->
     w :: acc
  | CmdSuffix_CmdSuffix_Word (s, w) ->
     words_of_suffix' (w :: acc) s

and words_of_suffix' acc s = words_of_suffix acc s.value

let words_of_suffix s = words_of_suffix [] s
let words_of_suffix' s' = words_of_suffix' [] s'

let unWord' word' = unWord word'.value
let unCmdWord' {value=(CmdWord_Word word')} = unWord word'.value
let unCmdName' {value=(CmdName_Word word')} = unWord word'.value

let contains_parameter w =
  let detect_parameter =
    object(self)
      inherit [_] Morbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word_component _env = function
        | WordVariable _ -> true
        | _ -> false
    end
  in detect_parameter#visit_word () w

let contains_subshell w =
  let detect_subshell =
    object(self)
      inherit [_] Morbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word_component _env = function
        | WordSubshell _ -> true
        | _ -> false
    end
  in detect_subshell#visit_word () w

let contains_glob w =
  let detect_glob =
    object(self)
      inherit [_] Morbig.CST.reduce as super
      method zero = false
      method plus = (||)
      method! visit_word_component _env = function
        | WordGlobAll | WordGlobAny | WordReBracketExpression _ -> true
        | _ -> false
    end
  in detect_glob#visit_word () w
