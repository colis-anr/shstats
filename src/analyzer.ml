module type S = sig

  (** A (unique) name for the analyzer. This generate a command line option
      to activate this specific analyzer if needed. *)
  val name : string

  (** Command line options for this analyzer. (See {!Args}) *)
  val options : (Arg.key * Arg.spec * Arg.doc) list

  (** [process_script script csts] analyzes the concrete syntax trees
      [csts] of the [script]. The retrieved information is stored in
      the internal state of the analyzer. *)
  val process_script : string -> Libmorbig.CST.complete_command list -> unit

  (** Once all scripts are processed, the following function is called
      once. It is supposed to write a report about the analysis. The
      format of this report is unspecified. *)
  val output_report : string -> bool

end

let analyzers : (module S) list ref = ref []

let register analyzer =
  analyzers := analyzer :: !analyzers

type activation_option =
  | All
  | Subset of (module S) list

let active_analyzers =
  ref All

let activate (module A : S) () =
  active_analyzers := match !active_analyzers with
    | All -> Subset [(module A : S)]
    | Subset analyzers -> Subset ((module A : S) :: analyzers)

let options () =
  let options = ref [] in
  List.iter (fun (module A : S) ->
    let activation_option =
      Arg.(
	"--enable-" ^ A.name,
	Unit (activate (module A)),
	(Format.sprintf " Activate analyzer `%s'." A.name)
      )
    in
    options := activation_option :: A.options @ !options
  ) !analyzers;
  !options

let foreach_active_analyzer f =
  List.iter f (match !active_analyzers with
    | All -> !analyzers
    | Subset analyzers -> analyzers
  )

let process_script filename csts =
  foreach_active_analyzer (fun (module A : S) ->
    A.process_script filename csts
  )

let output_report report_path =
  (
    try Unix.mkdir report_path 0o755
    with Unix.Unix_error _ ->
      Format.eprintf "Could not create dir `%s`@." report_path;
      exit 1
  );
  let oc = open_out (Filename.concat report_path "index.org") in
  let fmt = Format.formatter_of_out_channel oc in

  Format.fprintf fmt "#+TITLE: Statistics report\n";
  Format.fprintf fmt "#+STARTUP: indent inlineimages hideblocks\n\n";
  Format.fprintf fmt "Processed %d files.\n" (List.length (Options.files ()));
  Format.fprintf fmt "Analysers:\n";

  foreach_active_analyzer (fun (module A : S) ->
      let report_path = Filename.concat report_path A.name in
      let report_path =
        A.name ^
          if A.output_report report_path
          then ""
          else ".org"
      in
      Format.fprintf fmt "- [[file:%s][%s]]\n" report_path A.name
    );

  flush oc;
  close_out oc
