(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

(** Compute some statistics about a CST. *)

open Commands

open CommandAnalyzer
open StructuresAnalyzer
open MiscAnalyzer
open FunctionsAnalyzer
open AssignmentAnalyzer
open TestAnalyzer

(* Inputs *)

let load_morbig_file filename =
  let cin = open_in filename in
  let (_, csts) : string * Libmorbig.CST.complete_command_list =
    input_value cin in
  close_in cin;
  csts

let load_file filename =
  if Filename.check_suffix filename ".morbig" then
    (
      let csts = load_morbig_file filename in
      Some (Filename.chop_suffix filename ".morbig", csts)
    )
  else
    (
      try
        Some (filename, Libmorbig.API.parse_file filename)

      with
        _ ->
        Format.eprintf "%s: parse error@." filename;
        None
    )

(* Helpers *)

let list_of_option_list l =
  let rec aux acc = function
    | [] -> acc
    | None :: t -> aux acc t
    | Some h :: t -> aux (h :: acc) t
  in
  aux [] l |> List.rev

let () =
  (* Tell the Analyzer engine about all the available analyzers *)

  Analyzer.register (module RedirectionAnalyzer);

  (* Parse command line *)

  Options.register_analyzers_options (Analyzer.options ());
  Options.parse_command_line ();
  let files = Options.files () in

  (* Parse files *)

  let files =
    Progress.List.map
      "Parsing files..."
      (fun (_, filename) -> load_file filename)
      files
  in
  let files = list_of_option_list files in

  (* Expand files *)

  let files =
    if !Options.expander then
      Progress.List.map
        "Expanding assignments..."
        (fun (filename, csts) -> (filename, Expander.expand csts))
        files
    else
      files
  in

  (* Give all the files to the analyzers *)

  Progress.List.iter
    "Analyzing CSTs..."
    (fun (filename, csts) -> Analyzer.process_script filename csts)
    files;

  (* Create the report and end *)

  Format.eprintf "Creating report... @?";
  let report = Report.create "Statistics Report" in
  Analyzer.output_report report;

  Format.eprintf "Writing it on disk... @?";
  Report.commit report !Options.report_path;

  Format.eprintf "Done!\nYou can now open emacs on %s/index.org@." !Options.report_path
