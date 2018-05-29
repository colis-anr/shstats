
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

let slug ?(limit=max_int) name =
  let is_alphanum char =
    let code = Char.code char in
    (code >= 48 && code <= 57)
    || (code >= 65 && code <= 90)
    || (code >= 97 && code <= 122)
    || code = 45
  in
  let slug = Bytes.create (String.length name) in
  let i = ref 0 in
  let sep = ref true in
  String.iter
    (fun char ->
      if is_alphanum char then
        (Bytes.set slug !i char;
         incr i;
         sep := false)
      else if not !sep then
        (Bytes.set slug !i '-';
         incr i;
         sep := true))
    name;
  if !i > limit then
    Bytes.set slug (limit -1) '-';
  Bytes.sub_string slug 0 (min !i limit)

let create_subreport report ?(title="") name =
  if Hashtbl.mem report.subreports name then
    raise (Invalid_argument "add_subreport")
  else
    let subreport = create (if title = "" then name else title) in
    Hashtbl.add report.subreports name subreport;
    subreport

let filename name =
  let slug = slug ~limit:16 name in
  let hash = Format.sprintf "%x" (Hashtbl.hash name) in
  if slug = "" then
    hash
  else
    slug ^ "." ^ hash

let link_to_subreport report ?(text="") subreport_name =
  try
    let subreport = Hashtbl.find report.subreports subreport_name in
    Format.sprintf
      "[[file:%s][%s]]"
      (
        if Hashtbl.length subreport.subreports = 0 then
          (filename subreport_name) ^ ".org"
        else
          Filename.concat (filename subreport_name) "index.org"
      )
      (
        if text = "" then
          subreport_name
        else
          text
      )
  with
    Not_found -> if text = "" then subreport_name else text
               
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
  close_out file;
  Hashtbl.iter
    (fun name subreport -> commit subreport (Filename.concat path (filename name)))
    report.subreports
