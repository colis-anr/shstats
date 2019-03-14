(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

class occCounter :
  string ->
  object
    val values : (string, string Queue.t) Hashtbl.t
    method add : string -> string -> unit
    method iter : (string -> string -> unit) -> unit
    method n_elements : unit -> int
    method n_files : unit -> int
    method n_keys : unit -> int
    method n_occurrences : unit -> int
    method output_occurrences : Report.t -> unit
  end
val pp_shell_repr : (Format.formatter -> 'a -> 'b) -> 'a -> string
