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

(* 
   finds occurence of local that are not inside a function definition,
   or inside a branching control structure.
 *)
   
let options = []
            
let name = "local"
         
let unCmdWord' {value=(Libmorbig.CST.CmdWord_Word word')} =
  Libmorbig.CSTHelpers.unWord word'.value
let unCmdName' {value=(Libmorbig.CST.CmdName_Word word')} =
  Libmorbig.CSTHelpers.unWord word'.value

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

      method! visit_function_definition env = function
        | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody
          (_,_,function_body') ->
           self#visit_function_body' {env with in_function=true} function_body'

      method! visit_compound_command env cst =
        super#visit_compound_command {env with in_branching=true} cst

    end
  in
  detect_local#visit_complete_command_list
    {in_function=false;in_branching=false}
    cst

let output_report report =
  ()
  
              
