open ExtPervasives
open Libmorbig.CST
open Options
open Commands
open Messages

(*

   an identifier is a constant of a script if the script contains
   exactly one assignment to that identifier, and the assignment is in
   a non-complex context.

   an identifier is a variable of a script if the script contains at
   least two assignments to that identifier, or at least one
   assignment in a complex context.

 *)

module Self : Analyzer.S = struct

  let options = []

  let name = "identifiers"

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
    let module Counter = struct
    class iterator' = object(self)

	inherit [_] Libmorbig.CST.iter as super

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

	method! visit_cmd_prefix venv = function
      | CmdPrefix_IoRedirect _ ->
	     ()
	  | CmdPrefix_CmdPrefix_IoRedirect (p, _) ->
	     self#visit_cmd_prefix' venv p
	  | CmdPrefix_AssignmentWord aw ->
             let (v, _) = aw.value in
	     register_identifier self#is_under_complex_context v
	  | CmdPrefix_CmdPrefix_AssignmentWord (p, aw) ->
             let (v, _) = aw.value in
	     self#visit_cmd_prefix' venv p;
	     register_identifier self#is_under_complex_context v

	method! visit_compound_command venv = self#on_complex_context (function
      | CompoundCommand_BraceGroup b ->
	     self#visit_brace_group' venv b
	  | CompoundCommand_Subshell s ->
	     self#visit_subshell' venv s
	  | CompoundCommand_ForClause f ->
	     self#visit_for_clause' venv f
	  | CompoundCommand_CaseClause c ->
	     self#visit_case_clause' venv c
	  | CompoundCommand_IfClause i ->
	     self#visit_if_clause' venv i
	  | CompoundCommand_WhileClause w ->
	     self#visit_while_clause' venv w
	  | CompoundCommand_UntilClause u ->
	     self#visit_until_clause' venv u
	)
    end (* class iterator' = object ... *)
    end (* module Counter = struct ... *)
    in
    List.iter ((new Counter.iterator')#visit_complete_command ()) csts;
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

  let output_report path =
    let path = path ^ ".org" in
    let oc = open_out path in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt
"* Identifiers

   An identifier is a constant of a script if the script contains
   exactly one assignment to that identifier, and the assignment is in
   a non-complex context.

   An identifier is a variable of a script if the script contains at
   least two assignments to that identifier, or at least one
   assignment in a complex context.

  - Number of pairs (script,id) where id is constant in script: %d
  - Number of pairs (script,id) where id is variable in script: %d
  - Number of scripts containing variables: %d
 "
(!corpus_number_constants)
(!corpus_number_variables)
(List.length !corpus_scripts_with_variables);

    Format.fprintf fmt "** Scripts using variables\n";

    List.iter
      (function scriptname ->
                Format.fprintf fmt "    - [[file:%s]]\n" scriptname)
      !corpus_scripts_with_variables;

    flush oc;
    close_out oc;
    false

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
