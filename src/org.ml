(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives

type t = one list

and one =
  | HHeading of int * string * t
  | String of string
  | List of t list
  | Link of string * string * string (* kind, target, text *)

  (* shortcuts (see [replace_shortcuts]) *)
  | Heading of string * t

let f = Format.asprintf

(* printing *)

let replace_shortcuts = function
  | Heading (s, o) ->
     HHeading (1, s, o)
  | _ as one -> one

let rec pp_print lvl fmt =
  List.iter (replace_shortcuts ||> pp_print_one lvl fmt)

and pp_print_one lvl fmt =
  let p = Format.fprintf in function
  | HHeading (i, s, o) ->
     p fmt "%s %s\n%a" (String.make (lvl+i) '*') s (pp_print (lvl+i)) o
  | String s ->
     Format.pp_print_string fmt s (*FIXME: replace \n by @\n*)
  | List ol ->
     List.iter
       (fun o ->
         p fmt "@[<h 2>- %a@]@\n" (pp_print lvl) o)
       ol
  | Link (kind, target, text) ->
     p fmt "[[%s:%s][%s]]" kind target text
  | Heading _ ->
     assert false

let pp_print = pp_print 0
let pp_print_one = pp_print_one 0
