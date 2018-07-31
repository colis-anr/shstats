(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

(** Compute some statistics about a CST. *)

let list_map_filter f l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: q ->
       match f h with
       | None -> aux acc q
       | Some x -> aux (x :: acc) q
  in
  aux [] l

(* Inputs *)

type input =
  | Source
  | Parsed of Libmorbig.CST.complete_command_list
  | Expanded of Libmorbig.CST.complete_command_list
  | Loaded of Libmorbig.CST.complete_command_list
  | Error of string

let cache filename =
  filename ^ ".shstats-cache"

let load_cache_file filename =
  let cin = open_in filename in
  let csts : Libmorbig.CST.complete_command_list =
    input_value cin in
  close_in cin;
  csts

let save_cache_file filename csts =
  let cout = open_out filename in
  output_value cout csts;
  close_out cout

let json filename =
  filename ^ ".json"

let save_json_file filename csts =
  let cout = open_out filename in
  Yojson.Safe.pretty_to_channel cout (Libmorbig.CST.complete_command_list_to_yojson csts);
  close_out cout

(* Helpers *)

let list_of_option_list l =
  let rec aux acc = function
    | [] -> acc
    | None :: t -> aux acc t
    | Some h :: t -> aux (h :: acc) t
  in
  aux [] l |> List.rev

(* Tell the Analyzer engine about all the available analyzers *)

let () =
  Analyzer.register_several [
      (module AssignmentAnalyzer) ;
      (module CommandAnalyzer) ;
      (module FunctionAnalyzer) ;
      (module MiscAnalyzer) ;
      (module RedirectionAnalyzer) ;
      (module StructureAnalyzer) ;
      (module TestAnalyzer) ;
      (module DollarAnalyzer) ;
      (module LocalAnalyzer) ;
    ]

  (* Parse command line *)

let () =
  Options.register_analyzers_options (Analyzer.options ());
  Options.parse_command_line ()
let files = Options.files ()

(* Read file list. If the --cache option is on, see if the cache file
   exists and load it if it does. *)

let cache_files = ref 0

let files =
  if !Options.cache then
    (
      Progress.List.map
        "Loading cache files"
        (fun (_, filename) ->
          if Sys.file_exists (cache filename) then
            (
              incr cache_files;
              (filename, Loaded (load_cache_file (cache filename)))
            )
          else
            (filename, Source))
        files
    )
  else
    (
      List.map
        (fun (_, filename) -> (filename, Source))
        files
    )

let () =
  Format.eprintf "Found %d files" (List.length files);
  if !Options.cache then
    Format.eprintf " (%d cached)" !cache_files;
  Format.eprintf ".@."

(* Parse files that have not been loaded from the cache. *)

let invalid_files = ref 0

let files =
  let pl = Progress.create "Parsing" (List.length files - !cache_files) in
  let files =
    List.map
      (fun (filename, kind) ->
        (filename,
         match kind with
         | Source ->
            (
              Progress.incr pl;
              try
                Parsed (Libmorbig.API.parse_file filename)
              with
                _ ->
                incr invalid_files;
                let msg = "parse error" in
                Error msg
            )
         | Parsed _ | Expanded _ | Error _ -> assert false
         | Loaded _ -> kind))
      files
  in
  Progress.close pl;
  files

let () =
  if !invalid_files > 0 then
    Format.eprintf "%d files could not be parsed.@." !invalid_files

(* Expand files that have not been loaded from the cache *)

let files =
  if !Options.expander && (List.length files - !cache_files - !invalid_files) > 0 then
    (
      let pl = Progress.create "Expanding" (List.length files - !cache_files - !invalid_files) in
      let files =
        List.map
          (fun (filename, kind) ->
            (filename,
             match kind with
             | Parsed csts ->
                Progress.incr pl;
                Expanded (Expander.expand csts)
             | Source | Expanded _ -> assert false
             | Loaded _ | Error _ -> kind))
          files
      in
      Progress.close pl;
      files
    )
  else
    files

(* Cache files that have not been loaded from the cache *)

let () =
  if !Options.cache && (List.length files - !cache_files - !invalid_files) > 0 then
    (
      let pl = Progress.create "Caching" (List.length files - !cache_files - !invalid_files) in
      List.iter
        (fun (filename, kind) ->
          match kind with
          | Parsed csts | Expanded csts ->
             (
               Progress.incr pl;
               save_cache_file (cache filename) csts
             )
          | Source -> assert false
          | Loaded _ | Error _ -> ())
        files;
      Progress.close pl
    )

(* Save JSon if asked *)

let () =
  if !Options.save_json && (List.length files - !invalid_files) > 0 then
    (
      let pl = Progress.create "Saving JSon" (List.length files - !invalid_files) in
      List.iter
        (fun (filename, kind) ->
          match kind with
          | Parsed csts | Expanded csts | Loaded csts ->
             (
               Progress.incr pl;
               save_json_file (json filename) csts
             )
          | Source -> assert false
          | Error _ -> ())
        files;
      Progress.close pl
    )
  
(* Give all the files to the analyzers *)

let () =
  let files =
    list_map_filter
      (fun (filename, kind) ->
        match kind with
        | Parsed csts | Expanded csts | Loaded csts ->
           Some (filename, csts)
        | Error _ -> None
        | Source -> assert false)
      files
  in
  Analyzer.process_scripts files

(* Create the report and end *)

let () =
  Format.eprintf "Creating report... @?";
  let report = Report.create "Statistics Report" in
  Analyzer.output_report report;

  Format.eprintf "Writing it on disk... @?";
  Report.commit report !Options.report_path;

  Format.eprintf "Done!\nYou can now open emacs on %s/index.org@." !Options.report_path
