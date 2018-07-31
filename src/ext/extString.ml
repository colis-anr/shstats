(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

include String

let to_char_list s =
  let rec aux i s =
    if i >= String.length s then
      []
    else
      s.[i] :: aux (i+1) s
  in
  aux 0 s

let split_on_char sep s =
  (* In the stdlib since 4.04.0 *)
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep then
      (
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      )
  done;
  String.sub s 0 !j :: !r
