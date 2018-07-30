(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Messages

type command_name = string
type option_name = string

type number_of_arguments =
  | NoArgument
  | Exactly of int
  | MoreThanOne
  | UntilSemicolon
  | UseArgumentAsEOF
  | Unhandled

type option_specification = {
    name : option_name list;
    number_of_arguments : number_of_arguments
  }

type command_options = {
    command : command_name;
    accumulated_short_options : bool;
    start_with_options : bool;
    double_dash_for_raw : bool;
    options : option_specification list;
    specified : bool;
  }

(* Synonyms for commands *)

let remember_synonym, canonical_command_name =
  let command_synonyms = Hashtbl.create 13 in
  (fun (c:string) (c':string) -> Hashtbl.add command_synonyms c' c),
  (fun (c:string) -> try Hashtbl.find command_synonyms c with Not_found -> c)

(* Command specification *)

let command_specification =
  ref (fun _ -> Options.failwith "--specification option is required by the command analyzer")

let load_commands_specification commands_specification =
  let parse_command_option lines =
    (* The header function parses the first line for a command ("*
       [,test true true false") into a command_options. *)
    let header line =
      try
	Scanf.sscanf line "* %s %B %B %B"
          (fun command accumulated_short_options start_with_options double_dash_for_raw ->
            (* The user might have specified several command names for
               the same one (like "[,test,other_name"). In that case,
               we take the first one to be the real name, and the
               others to be synonyms *)
	    let command = match String.split_on_char ',' command with
	      | [] -> assert false
	      | command :: synonyms ->
                 List.iter (remember_synonym command) synonyms;
                 command
	    in
	    {
	      command ;
	      accumulated_short_options ;
	      start_with_options ;
              double_dash_for_raw ;
	      options = [];
	      specified = true
	    }
	  )
      with _ -> assert false
    in
    (* The option function parses the option lines for a command. *)
    let option line =
      try
	Scanf.sscanf line "** %s %s"
          (fun options i ->
	    let number_of_arguments =
	      match i with
	      | "*" -> MoreThanOne
	      | ";" -> UntilSemicolon
	      | "#" -> UseArgumentAsEOF
	      | "?" -> Unhandled
	      | "0" -> NoArgument
	      | d   -> Exactly (int_of_string d)
	    in
	    let options = String.split_on_char ',' options in
	    { name = options; number_of_arguments })
      with _ -> assert false
    in
    List.fold_left
      (fun command options ->
        { command with options = options :: command.options })
      (header (List.hd lines))
      (List.map option (List.tl lines))

  in
  let no_comment line =
    line = ""
    || (String.length line > 0 && line.[0] <> '%')
  in
  let command_options =
    open_in commands_specification   (* open command specs *)
    |> lines_of_channel              (* get all lines in a list *)
    |> List.map snd                  (* forget about line numbers *)
    |> List.filter no_comment        (* remove comment lines *)
    |> List.split_delim ((=) "")     (* use empty lines to split between commands *)
    |> List.map parse_command_option (* parse each command *)
  in
  (* Put all commands in a Hashtbl *)
  let table = Hashtbl.create 13 in
  List.iter
    (fun command ->
      Hashtbl.add table command.command command)
    command_options;
  (* command_specification is not a function that tries to find
     commands in the table. If they are not there, it returns an
     unspecified command over which we have no knowledge. *)
  command_specification :=
    fun command ->
    try
      let command = canonical_command_name command in
      Hashtbl.find table command
    with Not_found ->
      { command ;
	accumulated_short_options = false;
	start_with_options = false;
        double_dash_for_raw = false;
	options = [];
	specified = false }

let lookup_command command = !command_specification command

let lookup_options command option =
  List.find (fun o -> List.mem option o.name) (!command_specification command).options

let lookup_option_number_of_arguments command option =
  let command = canonical_command_name command in
  try
    (lookup_options command option).number_of_arguments
  with Not_found ->
    NoArgument

let is_option_of_command ?(who_is_asking="") command option =
  let option_shape = String.length option > 0 && (option.[0] = '-') in
  try
    let command_specification = !command_specification command in
    if command_specification.specified then
      (ignore (lookup_options command option); true)
    else
      option_shape
  with Not_found ->
    if option_shape then warning (
      Format.sprintf
	"`%s': unknown option `%s' %s."
	command option
	(if who_is_asking = "" then
	   ""
	 else
	   Format.sprintf "\n(See %s)" who_is_asking)
                           );
    false

let canonical_option_name command option =
  let command = canonical_command_name command in
  let found = ref true in
  try
    List.hd (lookup_options command option).name
  with Not_found ->
    found := false;
    option
