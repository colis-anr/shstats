
let list_of_files = ref ""
let report_path = ref ""

let analyzers_options = ref []
let register_analyzers_options l =
  analyzers_options := l
let options () =
  ("--report-path",
   Arg.Set_string report_path,
   "PATH Put the report in PATH")
  ::
    ("--from-file",
     Arg.Set_string list_of_files,
     "FILE Get the list of files from FILE")
  ::
    ("--from-stdin",
     Arg.Unit (fun () -> list_of_files := "-"),
     " Get the list of files from standard input")
  :: !analyzers_options
  |> List.sort (fun (o1, _, _) (o2, _, _) -> compare o1 o2)
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
