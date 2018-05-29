
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

let create_subreport report ?(title="") name =
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
