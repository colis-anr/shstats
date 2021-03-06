(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

module type S = sig

  (** A (unique) name for the analyzer. This generate a command line option
      to activate this specific analyzer if needed. *)
  val name : string

  (** Command line options for this analyzer. (See {!Args}) *)
  val options : (Arg.key * Arg.spec * Arg.doc) list

  (** [process_script script csts] analyzes the concrete syntax trees
      [csts] of the [script]. The retrieved information is stored in
      the internal state of the analyzer. *)
  val process_script : string -> Morbig.CST.program -> unit

  (** Once all scripts are processed, the following function is called
      once. It is supposed to write a report about the analysis. The
      format of this report is unspecified. *)
  val output_report : Report.t -> unit

end

let analyzers : (module S) list ref = ref []

let register analyzer =
  analyzers := analyzer :: !analyzers

let register_several more_analyzers =
  analyzers := more_analyzers @ !analyzers

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

let process_scripts scripts =
  foreach_active_analyzer (fun (module A : S) ->
      Progress.List.iter
        ("Analyzing: "^A.name)
        (fun (filename, csts) ->
          A.process_script filename csts)
        scripts)
  
let output_report report =
  Report.add report "Processed %d files.\n" (List.length (Options.files ()));
  Report.add report "Analysers:\n";

  foreach_active_analyzer (fun (module A : S) ->
      A.output_report (Report.create_subreport report A.name);
      Report.add report "- %s\n" (Report.link_to_subreport report A.name)
    )
