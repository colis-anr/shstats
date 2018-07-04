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

module Self : Analyzer.S = struct

  let options = []

  let name = "expander"
                                 
  let process_script filename csts =
    let module Expander = struct

    class expand' = object(self)

	inherit [_] Libmorbig.CST.map as super

	val complex_context_level = ref 0 (* Nesting level of complex contexts. *)

	method enter_complex_context =
	  incr complex_context_level

	method exit_complex_context =
	  decr complex_context_level

	method on_complex_context f x =
	  self#enter_complex_context;
	  f x;
	  self#exit_complex_context

	method is_under_complex_context =
	  !complex_context_level > 0

    end (* class expand' = object ... *)
    end (* module Expander = struct ... *)
    in
    let expanded_csts =
      List.map ((new Expander.expand')#map_complete_command ()) csts
    in
    let cout = open_out (filename^".expanded")
    in begin
        JsonHelpers.save_as_json true cout cst;
        close_out cout
      end

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
