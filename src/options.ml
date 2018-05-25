let usage_message =
  Printf.sprintf
    "%s [options]\n\nRun several statistical analysis on the files provided by standard input.\n\nOptions are:"
    Sys.executable_name

let list_of_files = ref "-"

let get_arguments options argument =
  Printf.eprintf "Unexpected argument: %s" argument;
  Arg.usage options usage_message;
  exit 2

let parse_command_line options =
  let options =
    ("--from-file",
     Arg.Set_string list_of_files,
     "FILE Get the list of files from FILE instead of standard input")
    :: options
  in
  let options = List.sort (fun (o1, _, _) (o2, _, _) -> compare o1 o2) options in
  let options = Arg.align options in
  Arg.parse options (get_arguments options) usage_message

let files () =
  let files =
    ExtPervasives.lines_of_channel
      (if !list_of_files = "-" then
         ( Format.eprintf "Reading files from standard input@.";
           stdin )
       else
         ( Format.eprintf "Reading files from `%s`@." !list_of_files;
           open_in !list_of_files ))
  in
  Printf.printf "#+TITLE: Statistics report\n";
  Printf.printf "#+STARTUP: indent inlineimages hideblocks\n\n";
  Printf.printf "Processing %d files.\n\n%!" (List.length files);
  files
