(** Compute some statistics about a CST. *)

open Commands

open CommandAnalyzer
open VariableAnalyzer
open StructuresAnalyzer
open MiscAnalyzer
open FunctionsAnalyzer

let read filename =
  if Filename.check_suffix filename ".morbig" then
    (
      let cin = open_in filename in
      let (filename, csts) : string * Libmorbig.CST.complete_command list = input_value cin in
      close_in cin;
      Some (filename, csts)
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

let process total_number_of_files file_number (_, filename) =
  match read filename with
  | None -> ()
  | Some (filename, csts) ->
     Format.eprintf "[%d/%d] %s\r@?"
       (1+file_number) total_number_of_files filename;
     Analyzer.process_script filename csts

let () =
  Options.register_analyzers_options (Analyzer.options ());
  Options.parse_command_line ();
  let files = Options.files () in
  List.iteri (process (List.length files)) files;
  let report = Report.create "Statistics Report" in
  Analyzer.output_report report;
  Report.commit report !Options.report_path
