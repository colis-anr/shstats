
type t =
  { title : string ;
    buffer : Buffer.t ;
    formatter : Format.formatter ;
    subreports : (string, t) Hashtbl.t }

let create title =
  let buffer = Buffer.create 8 in
  let formatter = Format.formatter_of_buffer buffer in
  let subreports = Hashtbl.create 8 in
  { title ; buffer ; formatter ; subreports }

let add report = Format.fprintf report.formatter

let make_name_valid name =
  String.map
    (fun char ->
      let code = Char.code char in
      if (code >= 48 && code <= 57)
         || (code >= 65 && code <= 90)
         || (code >= 97 && code <= 122)
         || code = 45
         || code = 95
      then
        char
      else
        '_')
    name

let create_subreport report ?(title="") name =
  let name = make_name_valid name in
  if Hashtbl.mem report.subreports name then
    raise (Invalid_argument "add_subreport")
  else
    let subreport = create (if title = "" then name else title) in
    Hashtbl.add report.subreports name subreport;
    subreport

let rec commit report path =
  Format.pp_print_flush report.formatter ();
  let file = open_out (
      if Hashtbl.length report.subreports = 0 then
        (path ^ ".org")
      else
        (Unix.mkdir path 0o755;
         Filename.concat path "index.org")
    )
  in
  output_string file (Buffer.contents report.buffer);
  Hashtbl.iter
    (fun name subreport -> commit subreport (Filename.concat path name))
    report.subreports
