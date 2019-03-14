(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open Morbig.CST

val lnum : 'a located -> int

val words_of_suffix : cmd_suffix -> word' list
val words_of_suffix' : cmd_suffix' -> word' list

val unWord' : word' -> string
val unCmdWord' : cmd_word' -> string
val unCmdName' : cmd_name' -> string

(* check whether a word contains a parameter *)
val contains_parameter : word -> bool

(* check whether a word contains a subshell *)
val contains_subshell : word -> bool

(* check whether a word contains a glob ( * or ? ) *)
val contains_glob : word -> bool
