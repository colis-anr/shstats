(** Compute some statistics about a CST. *)

open Commands

open CommandAnalyzer
open VariableAnalyzer
open StructuresAnalyzer
open MiscAnalyzer
open FunctionsAnalyzer
open IdentifierAnalyzer
open TestAnalyzer

let process = function
  | None -> ()
  | Some (filename, csts) ->
     Analyzer.process_script filename csts

let () =
  Options.register_analyzers_options (Analyzer.options ());
  Options.parse_command_line ();

  Format.eprintf "Reading file list...@.";
  let files = Options.files () in

  Format.eprintf "Analyzing files...@.";
  files
  |> List.map
       (fun (_, filename) ->
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
  |> List.iter process;

  Format.eprintf "Creating report...@.";
  let report = Report.create "Statistics Report" in
  Analyzer.output_report report;

  Format.eprintf "Writing report on disk...@.";
  Report.commit report !Options.report_path;

  Format.eprintf "Done! You can now open emacs on %s/index.org@." !Options.report_path
