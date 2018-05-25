open ExtPervasives
open Libmorbig.CST
open Options
open Commands
open Messages

module Name =
  struct
    type t = Libmorbig.CST.name
    let compare = Pervasives.compare
  end
    
module NameSet = Set.Make(Name)
  
module Self : Analyzer.S = struct

  let options = []

  let name = "variables"

  let constants = Hashtbl.create 13

  let variables = Hashtbl.create 13

  let process_script filename csts =
    let vars = Hashtbl.create 13 in
    let count_variable complex_context ((Name s) as x) = Hashtbl.(
      if complex_context then
	replace vars x max_int
      else
	replace vars x (1 + try find vars x with Not_found -> 0)
    )
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
	     count_variable self#is_under_complex_context v
	  | CmdPrefix_CmdPrefix_AssignmentWord (p, aw) ->
             let (v, _) = aw.value in
	     self#visit_cmd_prefix' venv p;
	     count_variable self#is_under_complex_context v

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
      end
    end
    in
    List.iter ((new Counter.iterator')#visit_complete_command ()) csts;
    Hashtbl.iter (fun ((Name x) as v) nb_occ ->
      let where = if nb_occ > 1 then variables else constants in
      Hashtbl.(
	replace where v (filename :: try find where v with _ -> [])
      )
    ) vars

  let number_of_constants () = Hashtbl.length constants

  let number_of_variables () = Hashtbl.length variables

  let names_of_table table =
    Hashtbl.fold (fun n _ s -> NameSet.add n s) table NameSet.empty

  let number_of_distinct_variables () =
    NameSet.(
      cardinal (union (names_of_table constants) (names_of_table variables))
    )

  let show =
    Hashtbl.iter (fun (Name v) fs ->
      Format.printf "*** %s\n" v;
      List.iter (fun f -> Format.printf "    - [[file:%s]]\n" f) fs
    )

  let show_constants () = show constants
  let show_variables () = show variables

  let output_report () =
    Format.printf
"* Variables

  For each filename, if a variable is assigned twice, it is marked as a
  \"real\" variable. Otherwise, it is marked as a constant. Notice that
  this is an over-approximation of the number of \"real\" variables because
  two assignments may appear in two disjoint execution paths. Also, a variable
  may be a constant in one file and a \"real\" variable in another.

  - Number of constants: %d
  - Number of \"real\" variables: %d
  - Number of distinct variable identifiers: %d
" (number_of_constants ()) (number_of_variables ()) (number_of_distinct_variables ());

  Format.printf
"
** Constants
";
  show_constants ();

  Format.printf
"
** Real variables
";
  show_variables ()

end

let install = Analyzer.register (module Self)
