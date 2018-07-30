(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open MoreCSTHelpers
open Options
open Commands
open Messages

(*
   finds occurence of local that are not inside a function definition,
   or inside a branching control structure.
 *)

let name = "local"
let options = []

let local_outside_function = ref ( [] : string list )
and local_in_branching = ref ( [] : string list )
and local_ok = ref ( [] : string list)
let register_outside_function s =
  local_outside_function := s::!local_outside_function
and register_in_branching s =
  local_in_branching := s::!local_in_branching
and register_ok s =
  local_ok := s::!local_ok

type env = {
    in_function : bool;
    in_branching : bool
  }

let register_local env cmd script =
  if cmd = "local" then begin
      if not env.in_function then register_outside_function script;
      if env.in_branching then register_in_branching script;
      if env.in_function && not env.in_branching then register_ok script
    end

let process_script filename cst =
  let detect_local =
    object (self)
      inherit [_] Libmorbig.CST.iter as super

      method! visit_simple_command env = function
        | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix(_,cmd_word',_)
          | SimpleCommand_CmdPrefix_CmdWord(_,cmd_word') ->
           register_local env (unCmdWord' cmd_word') filename
        | SimpleCommand_CmdPrefix(cmd_prefix') ->
           self#visit_cmd_prefix' env cmd_prefix'
        | SimpleCommand_CmdName_CmdSuffix(cmd_name',_)
          | SimpleCommand_CmdName (cmd_name') ->
           register_local env (unCmdName' cmd_name') filename

      method! visit_function_definition env cst =
        super#visit_function_definition
          {in_function=true;in_branching=false} cst
          (* a function definition may be inside a conditional so we
             reset in_branching when entering a function definition. *)

      method! visit_for_clause env cst =
        super#visit_for_clause {env with in_branching=true} cst

      method! visit_case_clause env cst =
        super#visit_case_clause {env with in_branching=true} cst

      method! visit_if_clause env cst =
        super#visit_if_clause {env with in_branching=true} cst

      method! visit_while_clause env cst =
        super#visit_while_clause {env with in_branching=true} cst

      method! visit_until_clause env cst =
        super#visit_until_clause {env with in_branching=true} cst

    end
  in
  detect_local#visit_complete_command_list
    {in_function=false;in_branching=false}
    cst

let output_report report =
  Report.add report
    "* Number of local in function, outside branching cmd: %d\n"
    (List.length !local_ok);
  Report.add report "** Files:\n";
  List.iter
    (function scriptname ->
       Report.add report "- %s\n" (Report.link_to_source report scriptname))
    !local_ok;
  Report.add report
    "* Number of local outside function definition: %d\n"
    (List.length !local_outside_function);
  Report.add report "** Files:\n";
  List.iter
    (function scriptname ->
       Report.add report "- %s\n" (Report.link_to_source report scriptname))
    !local_outside_function;
  Report.add report
    "* Number of local in branching command: %d\n"
    (List.length !local_in_branching);
  Report.add report "** Files:\n";
  List.iter
    (function scriptname ->
       Report.add report "- %s\n" (Report.link_to_source report scriptname))
    !local_in_branching;
