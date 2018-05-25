(** Compute some statistics about a CST. *)

open Commands

open CommandAnalyzer
open VariableAnalyzer
open StructuresAnalyzer
open MiscAnalyzer
open FunctionsAnalyzer
open IdentityAnalyzer

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
        Format.eprintf "%s: parsing@." filename;
        Some (filename, Libmorbig.API.parse_file filename)
      with
        Libmorbig.API.SyntaxError _ ->
        Format.eprintf "%s: syntax error@." filename;
        None
    )

let process (_, filename) =
  match read filename with
  | None -> ()
  | Some (filename, csts) ->
     Format.eprintf "%s: processing@." filename;
     Analyzer.process_script filename csts

let () =
  Options.parse_command_line (Analyzer.options ());
  List.iter process (Options.files ());
  Analyzer.output_report ()
