open Messages

type number_of_arguments =
  | NoArgument
  | Exactly of int
  | MoreThanOne
  | UntilSemicolon
  | UseArgumentAsEOF
  | Unhandled

type command_name = Libmorbig.CST.word
type option_name = Libmorbig.CST.word

type option_specification = {
  name : option_name list;
  number_of_arguments : number_of_arguments
  }

type command_options = {
    command : command_name;
    accumulated_short_options : bool;
    start_with_options : bool;
    double_dash_for_raw : bool;
    options : option_specification list;
    specified : bool;
  }

let remember_synonym, canonical_command_name =
  let command_synonyms = Hashtbl.create 13 in
  (fun c c' -> Hashtbl.add command_synonyms (Libmorbig.CST.Word (c', [])) c), (*FIXME*)
  (fun c -> try Hashtbl.find command_synonyms c with Not_found -> c)

let command_specification =
  ref (fun _ ->
      Format.eprintf "In order to run the `command` analyzer, the --specification option is required.@.";
      exit 2)

let load_commands_specification commands_specification =
  let parse_command_option lines = Scanf.(
    let header (lineno, s) =
      try
	sscanf s "* %s %B %B %B" (fun c b s r ->
	  let command_synonyms = Str.(split (regexp ",") c) in
	  let c = match command_synonyms with
	    | [] -> assert false
	    | c :: cs -> List.iter (remember_synonym (Libmorbig.CST.Word (c, []))) cs; c (*FIXME*)
	  in
	  {
	    command = Libmorbig.CST.Word (c, []); (*FIXME*)
	    accumulated_short_options = b;
	    start_with_options = s;
            double_dash_for_raw = r;
	    options = [];
	    specified = true
	  }
	)
      with _ -> error (
	Printf.sprintf "Line %d: Syntax error.\n" lineno
      )
    in
    let option (lineno, s) =
      try
	sscanf s "** %s %s" (fun o i ->
	  let number_of_arguments =
	    match i with
	      | "*" ->
		MoreThanOne
	      | ";" ->
		UntilSemicolon
	      | "#" ->
		UseArgumentAsEOF
	      | "?" ->
		Unhandled
	      | "0" ->
		NoArgument
	      | d   ->
		Exactly (int_of_string d)
	  in
	  let options = Str.(split (regexp ",") o) in
	  { name = List.map (fun o -> Libmorbig.CST.Word (o, [])) options; number_of_arguments }) (*FIXME*)
      with _ -> error (
	Printf.sprintf "Line %d: Expecting a natural number, '*' or ';'.\n" lineno
      )
    in
    let command = header (List.hd lines) in
    let options = List.fold_left (fun command o ->
      { command with options = o :: command.options }
    ) command (List.map option (List.tl lines))
    in
    options
  )
  in
  let no_comment (_, s) = s = "" || (String.length s > 0 && s.[0] <> '%') in
  let command_options =
    List.map parse_command_option ExtPervasives.(split_list (fun (_, s) -> s = "") (
      List.filter no_comment (lines_of_channel (open_in commands_specification))
    ))
  in
  let table = Hashtbl.create 13 in
  List.iter (fun co -> Hashtbl.add table co.command co) command_options;
  command_specification := fun o ->
    try
      let o = canonical_command_name o in
      Hashtbl.find table o
    with Not_found ->
      {
	command = o;
	accumulated_short_options = false;
	start_with_options = false;
        double_dash_for_raw = false;
	options = [];
	specified = false
      }

let lookup_command command = !command_specification command

let lookup_options command option =
  List.find (fun o -> List.mem option o.name) (!command_specification command).options

let lookup_option_number_of_arguments command option =
  let command = canonical_command_name command in
  try
    (lookup_options command option).number_of_arguments
  with Not_found ->
    NoArgument

let is_option_of_command ?(who_is_asking="") command option =
  let s = Libmorbig.CSTHelpers.unWord option in
  let option_shape = String.length s > 0 && (s.[0] = '-') in
  try
    let command_specification = !command_specification command in
    if command_specification.specified then
      (ignore (lookup_options command option); true)
    else
      option_shape
  with Not_found ->
    if option_shape then warning (
      Printf.sprintf
	"`%s': unknown option `%s' %s."
	(Libmorbig.CSTHelpers.unWord command) s
	(if who_is_asking = "" then
	    ""
	 else
	    Printf.sprintf "\n(See %s)" who_is_asking)
    );
    false

let canonical_option_name command option =
  let command = canonical_command_name command in
  let found = ref true in
  try
    List.hd (lookup_options command option).name
  with Not_found ->
    found := false;
    option
