let usage_message =
  Format.sprintf
    "%s [options]\n\nRun several statistical analysis on the files provided by standard input.\n\nOptions are:"
    Sys.executable_name

let list_of_files = ref ""
let report_path = ref ""

let get_report_path () =
  match !report_path with
  | "" -> Format.eprintf "--report-path is mandatory@.";
          exit 1
  | path -> path
                
let anonymous_argument options argument =
  Format.eprintf "Unexpected argument: %s" argument;
  Arg.usage options usage_message;
  exit 2

let parse_command_line options =
  let options =
    ("--report-path",
     Arg.Set_string report_path,
     "PATH Put the report in PATH")
    :: ("--from-file",
        Arg.Set_string list_of_files,
        "FILE Get the list of files from FILE")
    :: ("--from-stdin",
        Arg.Unit (fun () -> list_of_files := "-"),
        " Get the list of files from standard input")
    :: options
  in
  let options = List.sort (fun (o1, _, _) (o2, _, _) -> compare o1 o2) options in
  let options = Arg.align options in
  Arg.parse options (anonymous_argument options) usage_message

let files =
  let all_files = ref None in
  fun () ->
  match !all_files with
  | None ->
     let files =
       ExtPervasives.lines_of_channel
         (match !list_of_files with
          | "" -> Format.eprintf "One of --from-stdin or --from-file must be specified.@.";
                  exit 1
          | "-" -> stdin
          | file -> open_in file)
     in
     all_files := Some files;
     files
  | Some files ->
     files
