(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

let list_of_files = ref "-"
let report_path = ref ""
let expander = ref true
let cache = ref false
let save_json = ref false
          
let set reference value () =
  reference := value

let analyzers_options = ref []
let register_analyzers_options l =
  analyzers_options := l

(* All options *)

let options () =
  ("--report-path",
   Arg.Set_string report_path,
   "PATH Put the report in PATH")
  ::
    ("--from-stdin",
     Arg.Unit (set list_of_files "-"),
     " Get the list of files from standard input (default)")
  ::
    ("--from-file",
     Arg.Set_string list_of_files,
     "FILE Get the list of files from FILE")
  ::
    ("--expander",
     Arg.Unit (set expander true),
     " Enable static expansion of variables (default)")
  ::
    ("--no-expander",
     Arg.Unit (set expander false),
     " Disable static expansion of variables")
  ::
    ("--cache",
     Arg.Unit (set cache true),
     " Enable use of cache files")
  ::
    ("--no-cache",
     Arg.Unit (set cache false),
     " Disable use of cache files (default)")
  ::
    ("--save-json",
     Arg.Unit (set save_json true),
     " Save JSon version of CSTs")
  ::
    (!analyzers_options
     |> List.sort (fun (o1, _, _) (o2, _, _) -> compare o1 o2))

  |> Arg.align

let usage_message =
  Format.sprintf
    "Usage: %s [options]\n\nRun several statistical analysis on the files provided by standard input.\n\nOptions are:"
    Sys.argv.(0)

let failwith ?(return_code=2) message =
  Format.eprintf "%s\n@." message;
  Arg.usage (options ()) usage_message;
  exit return_code

let parse_command_line () =
  Arg.parse
    (options ())
    (fun argument -> failwith (Format.sprintf "Unexpected argument: %s." argument))
    usage_message;
  (* checks *)
  if !report_path = "" then
    failwith "--report-path is mandatory";
  if Shell.test_e !report_path then
    failwith (Format.sprintf "The report path (%s) must not exist." !report_path)

let files =
  let all_files = ref None in
  fun () ->
  match !all_files with
  | None ->
     let files =
       ExtPervasives.lines_of_channel
         (match !list_of_files with
          | "" -> failwith "One of --from-stdin or --from-file must be specified."
          | "-" -> stdin
          | file -> open_in file)
     in
     all_files := Some files;
     files
  | Some files ->
     files
