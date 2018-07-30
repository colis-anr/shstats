(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

type t = one list

and one =
  | HHeading of int * string * t
  | Heading of string * t        (* shortcut for HHeading (1, ...) *)
  | String of string
  | List of t list

let f = Format.sprintf

let rec pp_print lvl fmt =
  List.iter (pp_print_one lvl fmt)

and pp_print_one lvl fmt =
  let p = Format.fprintf in function
  | HHeading (i, s, o) ->
     p fmt "%s %s\n%a" (String.make (lvl+i) '*') s (pp_print (lvl+i)) o
  | Heading (s, o) ->
     pp_print_one lvl fmt (HHeading (1, s, o))
  | String s ->
     Format.pp_print_string fmt s (*FIXME: replace \n by @\n*)
  | List ol ->
     List.iter
       (fun o ->
         p fmt "@[<h 2>- %a@]@\n" (pp_print lvl) o)
       ol

let pp_print = pp_print 0
let pp_print_one = pp_print_one 0
