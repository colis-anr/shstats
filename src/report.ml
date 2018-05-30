
(* ================ [ Utilities for unique, correct names ] ================= *)

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

let hash name = Format.sprintf "%x" (Hashtbl.hash name) (*FIXME: CRC32?*)

(* =============================== [ Report ] =============================== *)

type t =
  { title : string ;
    path : string list ;
    buffer : Buffer.t ;
    formatter : Format.formatter ;
    subreports : (string, t) Hashtbl.t }

let create ?(path=[]) title =
  let buffer = Buffer.create 8 in
  let formatter = Format.formatter_of_buffer buffer in
  let subreports = Hashtbl.create 8 in
  { title ; path ; buffer ; formatter ; subreports }

let create_subreport report ?(title="") name =
  if Hashtbl.mem report.subreports name then
    raise (Invalid_argument "add_subreport")
  else
    let subreport = create ~path:(name :: report.path) (if title = "" then name else title) in
    Hashtbl.add report.subreports name subreport;
    subreport

(* shadow create to hide the optionnal path argument *)
let create title = create title

let has_subreports report =
  Hashtbl.length report.subreports <> 0

let add report = Format.fprintf report.formatter








let safe_report_name name =
  let slug = slug ~limit:16 name in
  let hash = hash name in
  if slug = "" then hash else slug ^ "." ^ hash

let safe_source_path source =
  let target =
    String.split_on_char '/' source
    |> List.filter (function "." | ".." -> false | _ -> true)
    |> String.concat "/"
  in
  Filename.remove_extension target
  ^ "." ^ (hash source)
  ^ Filename.extension target





(* =========================== [ Link utilities ] =========================== *)

let link = Format.sprintf "[[%s:%s][%s]]"

let link_to_file target text =
  link "file" target text

let link_to_subreport report ?(text="") subreport_name =
  try
    let subreport = Hashtbl.find report.subreports subreport_name in
    link_to_file
      (
        if Hashtbl.length subreport.subreports = 0 then
          (safe_report_name subreport_name) ^ ".org"
        else
          Filename.concat (safe_report_name subreport_name) "index.org"
      )
      (
        if text = "" then
          subreport_name
        else
          text
      )
  with
    Not_found -> if text = "" then subreport_name else text

let link_to_source report ?(text="") source =
  let path =
    Filename.concat
      ("./" ^ String.concat "/" (
                  let path = List.map (fun _ -> "..") report.path in
                  if has_subreports report
                  then path
                  else List.tl path
      ))
      (Filename.concat "_sources" (safe_source_path source))
  in
  link_to_file path (if text = "" then source else text)







let rec commit_aux report path =
  Format.pp_print_flush report.formatter ();
  let file = open_out (
      if Hashtbl.length report.subreports = 0 then
        (path ^ ".org")
      else
        (Shell.mkdir path;
         Filename.concat path "index.org")
    )
  in
  output_string file (Buffer.contents report.buffer);
  close_out file;
  Hashtbl.iter
    (fun name subreport ->
      commit_aux subreport
        (Filename.concat path (safe_report_name name)))
    report.subreports

let commit report path =
  commit_aux report path;
  let sources_path = Filename.concat path "_sources" in
  Shell.mkdir sources_path;
  List.iter
    (fun (i, source) ->
      let source =
        if Filename.check_suffix source ".morbig"
        then Filename.chop_suffix source ".morbig"
        else source
      in
      let target = Filename.concat sources_path (safe_source_path source) in
      let ic = open_in source in
      let content = ExtPervasives.string_of_channel ic in
      close_in ic;
      Shell.(mkdir ~p (Filename.dirname target));
      let oc = open_out target in
      output_string oc content;
      close_out oc
    )
    (Options.files ())
