(** Compute some statistics about a CST. *)

let process (_, file) =
  let cin = open_in file in
  let (filename, csts) : string * Libmorbig.CST.complete_command list =
    input_value cin in
  Analyzer.process_script filename csts;
  close_in cin

let main =
  Options.parse_command_line (Analyzer.options ());
  Commands.load_commands_specification ();
  List.iter process (Options.files ());
  Analyzer.output_report ()
