let usage_message =
  Printf.sprintf "%s [options] list-of-files command-specification" Sys.executable_name

let list_of_files = ref ""

let commands_specification = ref ""

let get_arguments options =
  let i = ref 0 in
  fun w ->
    match !i with
      | 0 -> incr i; list_of_files := w
      | 1 -> incr i; commands_specification := w
      | _ ->
	Printf.eprintf "Expect only two arguments.\n";
	Arg.usage options usage_message;
	exit 2

let parse_command_line options =
  Arg.parse options (get_arguments options) usage_message

let files () = ExtPervasives.(
  comment (fun () ->
    lines_of_channel (open_in !list_of_files)
  ) (fun files ->
    Printf.printf "#+TITLE: Statistics report\n#+STARTUP: indent inlineimages hideblocks\n\nProcessing %d files.\n\n%!" (List.length files)
  )
)

