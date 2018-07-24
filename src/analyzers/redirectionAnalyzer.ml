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
type location_simple =
  Prefix | Suffix | Both | PrefixOnly
type location =
  Simple of location_simple | Compound | Function

type result =
  { filename : string ;
    location : location ;
    content : io_redirect' list }

(* FIXME: lists are really not efficient *)
let results : result list ref = ref []

let cmd_prefix_to_io_redirect_list cmd_prefix' =
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
  aux [] cmd_prefix'.value

let cmd_suffix_to_io_redirect_list cmd_suffix' =
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
  aux [] cmd_suffix'.value

let redirect_list_to_io_redirect_list redirect_list' =
  let rec aux acc = function
    | RedirectList_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | RedirectList_RedirectList_IoRedirect (redirect_list', io_redirect') ->
       aux (io_redirect' :: acc) redirect_list'.value
  in
  aux [] redirect_list'.value

let process_script filename csts =
  let visitor = object (self)
    inherit [_] reduce as super

    method zero : 'a list = []
    method plus (r : 'a list) (s : 'a list) = r @ s

    method! visit_command () command =
      self#plus
        (match command with
         | Command_CompoundCommand_RedirectList (_, redirect_list') ->
            (
              let content = redirect_list_to_io_redirect_list redirect_list' in
              assert (content <> []);
              [{ filename ; location = Compound ; content }]
            )
         | _ -> self#zero)
        (super#visit_command () command)

    method! visit_function_body () function_body =
      self#plus
        (match function_body with
         | FunctionBody_CompoundCommand_RedirectList (_, redirect_list') ->
            (
              let content = redirect_list_to_io_redirect_list redirect_list' in
              assert (content <> []);
              [{ filename ; location = Function ; content }]
            )
         | _ -> self#zero)
        (super#visit_function_body () function_body)

    method! visit_simple_command () simple_command =
      self#plus
        (match simple_command with
         | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', _, cmd_suffix') ->
            (
              let in_prefix = cmd_prefix_to_io_redirect_list cmd_prefix' in
              let in_suffix = cmd_suffix_to_io_redirect_list cmd_suffix' in
              match in_prefix, in_suffix with
              | [], [] -> []
              | _, [] -> [{ filename ; location = Simple Prefix ; content = in_prefix }]
              | [], _ -> [{ filename ; location = Simple Suffix ; content = in_suffix }]
              | _, _ -> [{ filename ; location = Simple Both ; content = in_prefix @ in_suffix }]
            )
         | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', _) ->
            (
              let content = cmd_prefix_to_io_redirect_list cmd_prefix' in
              if content = []
              then []
              else [{ filename ; location = Simple Prefix ; content }]
            )
         | SimpleCommand_CmdPrefix cmd_prefix' ->
            (
              let content = cmd_prefix_to_io_redirect_list cmd_prefix' in
              if content = []
              then []
              else [{ filename ; location = Simple PrefixOnly ; content }]
            )
         | SimpleCommand_CmdName_CmdSuffix (_, cmd_suffix') ->
            (
              let content = cmd_suffix_to_io_redirect_list cmd_suffix' in
              if content = []
              then []
              else [{ filename ; location = Simple Suffix ; content }]
            )
         | _ -> self#zero)
        (super#visit_simple_command () simple_command)
    end in
  visitor#visit_complete_command_list () csts
  |> ((@) !results)
  |> ((:=) results)

let output_report report =
  Report.add
    report
    "Redirections@."
