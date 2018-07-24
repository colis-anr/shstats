(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

let name = "redirection"
let options = []

type result = ()
class result_monoid = object
  method zero = ()
  method plus () () = ()
end

let results : (string, result) Hashtbl.t =
  Hashtbl.create 8

(* cmd_prefix -> redirection list *)
(* cmd_suffix -> redirection list *)
(* redirect_list -> redirection list *)
  
let process_script filename csts =
  let open Libmorbig.CST in
  let visitor = object (self)
    inherit [_] reduce as super
    inherit result_monoid as monoid

    method! visit_command () = function
      | Command_CompoundCommand_RedirectList (compound_command', redirect_list') ->
         ()
      | _ as command -> super#visit_command () command

    method! visit_function_body () = function
      | FunctionBody_CompoundCommand_RedirectList (compound_command', redirect_list') ->
         ()
      | _ as function_body -> super#visit_function_body () function_body

    method! visit_simple_command () = function
      | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', cmd_word', cmd_suffix') -> ()
      | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', cmd_word') -> ()
      | SimpleCommand_CmdPrefix cmd_prefix' -> ()
      | SimpleCommand_CmdName_CmdSuffix (cmd_name', cmd_suffix') -> ()
      | SimpleCommand_CmdName cmd_name' -> ()
    end in
  visitor#visit_complete_command_list () csts
  |> Hashtbl.add results filename

let output_report report =
  Report.add
    report
    "Redirections@."
