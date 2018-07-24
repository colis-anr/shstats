(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open Libmorbig.CST

let name = "redirection"
let options = []

(* where a redirection can occur. *)
(* FIXME: prefix/suffix? *)
(* FIXME: better name *)
type location =
  Simple | Compound | Function

type result =
  { filename : string ;
    location : location ;
    content : io_redirect' list }

(* FIXME: lists are really not efficient *)
let results : result list ref = ref []
  
let cmd_prefix_to_io_redirect_list cmd_prefix =
  let rec aux acc = function
    | CmdPrefix_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | CmdPrefix_CmdPrefix_IoRedirect (cmd_prefix', io_redirect') ->
       aux (io_redirect' :: acc) cmd_prefix'.value
    | CmdPrefix_AssignmentWord _ ->
       acc
    | CmdPrefix_CmdPrefix_AssignmentWord (cmd_prefix', _) ->
       aux acc cmd_prefix'.value
  in
  aux [] cmd_prefix

let cmd_suffix_to_io_redirect_list cmd_suffix =
  let rec aux acc = function
    | CmdSuffix_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix', io_redirect') ->
       aux (io_redirect' :: acc) cmd_suffix'.value
    | CmdSuffix_Word _ ->
       acc
    | CmdSuffix_CmdSuffix_Word (cmd_suffix', _) ->
       aux acc cmd_suffix'.value
  in
  aux [] cmd_suffix

let redirect_list_to_io_redirect_list redirect_list =
  let rec aux acc = function
    | RedirectList_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | RedirectList_RedirectList_IoRedirect (redirect_list', io_redirect') ->
       aux (io_redirect' :: acc) redirect_list'.value
  in
  aux [] redirect_list

let process_script filename csts =
  let visitor = object (self)
    inherit [_] reduce as super

    method zero : 'a list = []
    method plus (r : 'a list) (s : 'a list) = r @ s
          
    method! visit_command () = function
      | Command_CompoundCommand_RedirectList (compound_command', redirect_list') -> self#zero
      | _ as command -> super#visit_command () command

    method! visit_function_body () = function
      | FunctionBody_CompoundCommand_RedirectList (compound_command', redirect_list') -> self#zero
      | _ as function_body -> super#visit_function_body () function_body

    method! visit_simple_command () = function
      | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', cmd_word', cmd_suffix') -> self#zero
      | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', cmd_word') -> self#zero
      | SimpleCommand_CmdPrefix cmd_prefix' -> self#zero
      | SimpleCommand_CmdName_CmdSuffix (cmd_name', cmd_suffix') -> self#zero
      | SimpleCommand_CmdName cmd_name' -> self#zero
    end in
  visitor#visit_complete_command_list () csts
  |> ((@) !results)
  |> ((:=) results)

let output_report report =
  Report.add
    report
    "Redirections@."
