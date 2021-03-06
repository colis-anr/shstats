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

(*

   an identifier is a constant of a script if the script contains
   exactly one assignment to that identifier, and the assignment is in
   a non-complex context.

   an identifier is a variable of a script if the script contains at
   least two assignments to that identifier, or at least one
   assignment in a complex context.

 *)

let options = []

let name = "assignment"

type identifier = Constant | Variable

let corpus_number_variables = ref 0
and corpus_number_constants = ref 0
and corpus_scripts_with_variables = ref []

let process_script filename csts =
  let identifiers = Hashtbl.create 13 in
  let register_identifier in_complex_context ((Name s) as x) =
    if in_complex_context then
      Hashtbl.replace identifiers x Variable
    else
      Hashtbl.replace
        identifiers x
        (if Hashtbl.mem identifiers x then Variable else Constant)
  in
  let counter = object (self)
                  inherit [_] Morbig.CST.iter as super

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

                  method! visit_cmd_prefix () = function
                    | CmdPrefix_IoRedirect _ ->
	               ()
	            | CmdPrefix_CmdPrefix_IoRedirect (p, _) ->
	               self#visit_cmd_prefix' () p
	            | CmdPrefix_AssignmentWord aw ->
                       let (v, _) = aw.value in
	               register_identifier self#is_under_complex_context v
	            | CmdPrefix_CmdPrefix_AssignmentWord (p, aw) ->
                       let (v, _) = aw.value in
	               self#visit_cmd_prefix' () p;
	               register_identifier self#is_under_complex_context v

                  method! visit_compound_command () = self#on_complex_context (function
                                                          | CompoundCommand_BraceGroup b ->
	                                                     self#visit_brace_group' () b
	                                                  | CompoundCommand_Subshell s ->
	                                                     self#visit_subshell' () s
	                                                  | CompoundCommand_ForClause f ->
	                                                     self#visit_for_clause' () f
	                                                  | CompoundCommand_CaseClause c ->
	                                                     self#visit_case_clause' () c
	                                                  | CompoundCommand_IfClause i ->
	                                                     self#visit_if_clause' () i
	                                                  | CompoundCommand_WhileClause w ->
	                                                     self#visit_while_clause' () w
	                                                  | CompoundCommand_UntilClause u ->
	                                                     self#visit_until_clause' () u
	                                                )
                end in
  counter#visit_program () csts;
  let number_variables = ref 0
  and number_constants = ref 0
  in
  Hashtbl.iter (fun _ident_name -> function
      | Constant -> incr number_constants
      | Variable -> incr number_variables
    ) identifiers;
  corpus_number_constants := !corpus_number_constants + !number_constants;
  corpus_number_variables := !corpus_number_variables + !number_variables;
  if !number_variables > 0 then
    corpus_scripts_with_variables :=
      filename::(!corpus_scripts_with_variables)

let output_report report =
  Report.add report
    "* Identifiers

     An identifier is a constant of a script if the script contains
     exactly one assignment to that identifier, and the assignment is in
     a non-complex context.

     An identifier is a variable of a script if the script contains at
     least two assignments to that identifier, or at least one
     assignment in a complex context.

     - Number of pairs (script,id) where id is constant in script: %d
     - Number of pairs (script,id) where id is variable in script: %d
     - Number of scripts containing variables: %d\n\n"

    (!corpus_number_constants)
    (!corpus_number_variables)
    (List.length !corpus_scripts_with_variables);

  Report.add report "** Scripts using variables\n";

  List.iter
    (function scriptname ->
       Report.add report "    - %s\n" (Report.link_to_source report scriptname))
    !corpus_scripts_with_variables
