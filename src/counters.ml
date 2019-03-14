(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

(* Not an analyzer in itself, but just kind of a "standard library"
for the analyzers. *)

class ['a, 'b] counter (name: string) = object (self)
  val values : ('a, 'b Queue.t) Hashtbl.t = Hashtbl.create 8

  method n_keys () =
    Hashtbl.length values

  method n_elements () =
    Hashtbl.fold (fun _ elmts n -> n + Queue.length elmts) values 0

  method add key elmt =
    try
      let elmts = Hashtbl.find values key in
      Queue.push elmt elmts
    with
      Not_found ->
      let elmts = Queue.create () in
      Queue.push elmt elmts;
      Hashtbl.add values key elmts

  method iter f =
    Hashtbl.iter
      (fun key -> Queue.iter (f key))
      values
end

let indent_string s n =
  let sep = "\n" ^ (String.make n ' ') in
  String.concat sep (String.split_on_char '\n' s)

class occCounter (name: string) = object (self)
  inherit [string, string] counter name as super

  method output_occurrences report =
    super#iter (fun f r -> Report.add report "- %s\n  %s\n" (Report.link_to_source report f) (indent_string r 2))

  method n_files = super#n_keys
  method n_occurrences = super#n_elements
end

let pp_shell_repr pp s =
  "#+BEGIN_SRC sh\n" ^ (indent_string (ExtPervasives.pp_to_string pp s) 2) ^ "\n#+END_SRC\n"
