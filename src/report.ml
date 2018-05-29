
type section = string

let path suffix =
  match !Options.report_path with
  | "" -> Options.failwith "--report-path is mandatory"
  | path -> Filename.concat path suffix

let create_section_directory section =
  Unix.mkdir (path section) 0o755

let is_section_directory section =
  let open Unix in
  match stat (path section) with
  | {st_kind=S_DIR} -> true
  | _ -> false
  | exception (Unix_error (ENOENT, _, _)) -> false

type file = out_channel * Format.formatter

let open_file ?(name="") section =
  let suffix =
    if is_section_directory section then
      (
        let name = if name = "" then "index" else name in
        Filename.concat section (name ^ ".org")
      )
    else
      (section ^ ".org")
  in
  ( try Unix.mkdir (Filename.dirname (path suffix)) 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> () );
  let oc = open_out (path suffix) in
  (oc, Format.formatter_of_out_channel oc)

let close_file (oc, _fmt) =
  flush oc;
  close_out oc

let fprintf (_oc, fmt) =
  Format.fprintf fmt

let get_section_entry section =
  if is_section_directory section then
    Filename.concat section "index.org"
  else
    section ^ ".org"

let print_headers (_oc, fmt) title =
  Format.fprintf fmt "#+TITLE: %s\n" title;
  Format.fprintf fmt "#+STARTUP: indent inlineimages hideblocks\n";
  Format.fprintf fmt "@."

let link_to_file link text =
  Format.sprintf "[[file:%s%s][%s]]"
    (if Filename.is_implicit link then "./" else "")
    link text
