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

module ProgressLine =
  struct
    type t =
      { name : string ;
        total : int ;
        mutable curr : int }

    let create name total =
      { name ; total ; curr = 0 }

    let percentage ?(scale=100) l =
      ExtPervasives.percentage_i ~scale l.curr l.total

    let eprint l =
      let open Format in
      eprintf "\r%s%s%s[%s%s] %3d%%@?"
        l.name
        (String.make (28 - String.length l.name) ' ')
        (String.make 5 ' ')
        (String.make (percentage ~scale:40 l) '=')
        (String.make (40 - percentage ~scale:40 l) ' ')
        (percentage l)

    let close l =
      eprint l;
      Format.eprintf "@."

    let update l i =
      l.curr <- i;
      eprint l
  end

let process = function
  | None -> ()
  | Some (filename, csts) ->
     Analyzer.process_script filename csts

let () =
  Options.register_analyzers_options (Analyzer.options ());
  Options.parse_command_line ();

  Format.eprintf "Reading file list... @?";
  let files = Options.files () in
  let number_of_files = List.length files in
  Format.eprintf "%d files found@." number_of_files;

  let progress_line = ProgressLine.create "Parsing files..." number_of_files in
  let files =
    List.mapi
      (fun i (_, filename) ->
        ProgressLine.update progress_line (i + 1);
        if Filename.check_suffix filename ".morbig" then
          (
            let cin = open_in filename in
            let (_, csts) : string * Libmorbig.CST.complete_command list = input_value cin in
            close_in cin;
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
      ))
      files
  in
  ProgressLine.close progress_line;

  let progress_line = ProgressLine.create "Analyzing files..." number_of_files in
  List.iteri
    (fun i file ->
      ProgressLine.update progress_line (i + 1);
      process file)
    files;
  ProgressLine.close progress_line;

  Format.eprintf "Creating report...@.";
  let report = Report.create "Statistics Report" in
  Analyzer.output_report report;

  Format.eprintf "Writing report on disk...@.";
  Report.commit report !Options.report_path;

  Format.eprintf "Done! You can now open emacs on %s/index.org@." !Options.report_path
