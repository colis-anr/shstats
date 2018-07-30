(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open MoreCSTHelpers

let name = "commands"

let options = [
    "--specification", Arg.String Commands.load_commands_specification, "FILE Load commands specification from FILE"
  ]

(* ===================== [ Specifications of commands ] ===================== *)

module Specification = struct
  type arguments =
    | NoArgument
    | Exactly of int
    | MoreThanOne
    | UntilSemicolon
    | UseArgumentAsEOF
    | Unhandled

  type option =
    { names : string list ;
      arguments : arguments }

  type options =
    (string, option) Hashtbl.t

  type command =
    { names : string list ;

      options : options ;
      options_accumulate : bool ;
      options_are_at_the_beginning : bool ;
      everything_after_ddash_is_raw : bool }

  type commands =
    (string, command) Hashtbl.t

  let commands = Hashtbl.create 13

  let load_options_from_line options line =
    Scanf.sscanf line "** %s %s"
      (fun names arguments ->
        let names = String.split_on_char ',' names in
        let option =
          { names ;
            arguments =
              match arguments with
	      | "*" -> MoreThanOne
	      | ";" -> UntilSemicolon
	      | "#" -> UseArgumentAsEOF
	      | "?" -> Unhandled
	      | "0" -> NoArgument
	      |  d  -> Exactly (int_of_string d) }
        in
        List.iter
          (fun name -> Hashtbl.add options name option)
          names)

  let rec load_options_from_lines options = function
    | [] -> []
    | line :: lines when line.[1] = '*' ->
       line :: lines
    | line :: lines ->
       load_options_from_line options line;
       load_options_from_lines options lines

  let load_command_from_line line =
    Scanf.sscanf line "* %s %B %B %B"
      (fun names options_accumulate options_are_at_the_beginning everything_after_ddash_is_raw ->
        let options = Hashtbl.create 13 in
        let names = String.split_on_char ',' names in
        let command =
          { names ; options ;
            options_accumulate ;
            options_are_at_the_beginning ;
            everything_after_ddash_is_raw } in
        List.iter
          (fun name -> Hashtbl.add commands name command)
          names;
        options)

  let rec load_commands_from_lines = function
    | [] -> ()
    | line :: lines ->
       let options = load_command_from_line line in
       load_options_from_lines options lines
       |> load_commands_from_lines

  let load_commands_from_file filename =
    open_in filename
    |> lines_of_channel
    |> List.map snd
    |> List.filter (fun line -> line <> "" && line.[0] <> '%')
    |> load_commands_from_lines
end

(* ====================== [ Simple Arguments Parser ] ======================= *)

let has_accumulating_arguments command =
  try
    (Hashtbl.find Specification.commands command).options_accumulate
  with
    Not_found -> false

let destruct_argument arg =
  if String.length arg >= 2 && arg.[1] = '-' then
    (true, String.sub arg 2 (String.length arg - 2))
  else
    (false, String.sub arg 1 (String.length arg - 1))

let parse_arguments command args =
  let rec aux acc = function
    | [] -> List.rev acc 
    | arg :: rest when arg = "" || arg.[0] <> '-' -> aux acc rest
    | arg :: rest ->
       let (is_long, arg) = destruct_argument arg in
       if is_long then
         aux (("--"^arg) :: acc) rest
       else
         (
           if has_accumulating_arguments command && String.length arg > 1 then
             aux acc ((String.to_char_list arg |> List.map (String.make 1 ||> ((^) "-"))) @ rest)
           else
             aux (("-"^arg) :: acc) rest
         )
  in
  aux [] args

(* ============================ [ CST Crawler ] ============================= *)

type command =
  { filename : string ;
    line : int ;
    name : string ;
    arguments : string list }

let add_command, get_commands =
  let commands = ref [] in
  (fun command -> commands := command :: !commands),
  (fun () -> List.rev !commands)

let arguments_of_suffix' command =
  words_of_suffix'
  ||> List.map unWord'
  ||> parse_arguments command

let process_script filename csts =
  let visitor = object (self)
    inherit [_] Libmorbig.CST.iter as super

    method! visit_simple_command () = function
      | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (_, cmd_word', cmd_suffix') ->
         let name = unCmdWord' cmd_word' in
         add_command
           { filename ; line = lnum cmd_word' ; name ;
             arguments = arguments_of_suffix' name cmd_suffix' }
      | SimpleCommand_CmdPrefix_CmdWord (_, cmd_word') ->
         add_command
           { filename ; line = lnum cmd_word' ; name = unCmdWord' cmd_word' ;
             arguments = [] }
      | SimpleCommand_CmdPrefix _ -> ()
      | SimpleCommand_CmdName_CmdSuffix (cmd_name', cmd_suffix') ->
         let name = unCmdName' cmd_name' in
         add_command
           { filename ; line = lnum cmd_name' ; name ;
             arguments = arguments_of_suffix' name cmd_suffix' }
      | SimpleCommand_CmdName cmd_name' ->
         add_command
           { filename ; line = lnum cmd_name' ; name = unCmdName' cmd_name' ;
             arguments = [] }
  end in
  visitor#visit_complete_command_list () csts

let output_report report =
  ()
