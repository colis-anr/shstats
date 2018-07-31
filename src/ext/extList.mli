(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

val batch : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** Create batches of consecutive values that are equal according to
   the given equal function. *)

val sort_batch_sort : ('a -> 'a -> int) -> 'a list -> 'a list list
(** Same as {!batch} but takes a comparison function instead of an
   equality function. It first sorts according to this comparison,
   then batches according to the equality derived from this comparison
   and then sorts from biggest batch to smallest. *)

val split_delim : ('a -> bool) -> 'a list -> 'a list list

val uniq : 'a list -> 'a list
