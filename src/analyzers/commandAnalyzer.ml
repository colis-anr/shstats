open ExtPervasives
open Commands
open Messages
open Libmorbig.CST
open Libmorbig.CSTHelpers

module Self (*: Analyzer.S*) = struct

  let name = "commands"

  let exotic_command_levels = ref []

  let push_exotic_command_level i =
    exotic_command_levels := i :: !exotic_command_levels

  let options = [
      "-i", Arg.Int push_exotic_command_level, "LEVEL If a command is used less than LEVEL time, mark it as exotic" ;
      "--specification", Arg.String Commands.load_commands_specification, "FILE Load commands specification from FILE"
    ]

  type parsed_argument =
    | Option of word * word list
    | SimpleArgument of word

  type arguments_scheme =
    | SimpleArguments
    | UseOption of word * int * arguments_scheme

  let rec options_of_scheme = function
    | SimpleArguments -> []
    | UseOption (w, _, s) -> unWord w :: options_of_scheme s

  let rec string_of_arguments_scheme = function
    | SimpleArguments ->
       Format.sprintf "(arguments)"
    | UseOption (Word (option, _), k, scheme) ->
       option ^ " "
       ^ String.concat " " (ExtPervasives.repeat k (fun i -> "arg" ^ string_of_int i))
       ^ " " ^ string_of_arguments_scheme scheme

  let classify_arguments command filename arguments categories =
    let command_options = lookup_command command in
    let expanded_arguments =
      if command_options.accumulated_short_options then
	List.(flatten (map (fun (Word (s, _)) ->
	                   if String.length s > 1 && s.[0] = '-' && s.[1] <> '-' then (
	                     let options = ref [] in
	                     for i = 1 to String.length s - 1 do
                               let opt = "-" ^ String.make 1 s.[i] in
	                       options := Word (opt, [WordLiteral opt]) :: !options
	                     done;
	                     !options
	                   ) else
	                     [Word (s, [])] (*FIXME*)
	                 ) arguments))
      else
	arguments
    in
    let double_dash = Word ("--", [WordLiteral "--"]) in
    let is_option w =
      (command_options.double_dash_for_raw && w = double_dash)
      || is_option_of_command ~who_is_asking:filename command w
    in
    let is_semicolon (Word (s, _)) = String.length s > 0 && (s.[0] = ';') in
    let parsed_arguments =
      let rec aux raw_mode = function
	| w :: ws ->
	   let raw_mode =
	     raw_mode
             || (command_options.start_with_options && not (is_option w))
             || (command_options.double_dash_for_raw && w = double_dash)
	   in
	   if is_option w && not raw_mode then
	     let w = canonical_option_name command w in
	     if raw_mode then
	       aux raw_mode ws
	     else match lookup_option_number_of_arguments command w with
	          | NoArgument ->
		     Option (w, []) :: aux raw_mode ws
	          | Unhandled ->
		     warning ~filename (
		               Format.sprintf "Detected usage of unhandled option %s for %s."
		                              (unWord w)
		                              (unWord command)
		             );
		     Option (w, []) :: aux raw_mode ws
	          | Exactly d ->
		     let options, ws = ExtPervasives.take d ws in
		     Option (w, options) :: aux raw_mode ws
	          | (MoreThanOne | UntilSemicolon) as condition ->
		     select_option_arguments raw_mode w ws (
		                               if condition = MoreThanOne then is_option else is_semicolon
		                             )
	          | UseArgumentAsEOF ->
		     match ws with
		     | [] ->
		        error (Format.sprintf "There is no EOF marker specified in command %s."
			                      (unWord command))
		     | eofmarker :: ws ->
		        select_option_arguments raw_mode w ws (( = ) eofmarker)
	   else
	     SimpleArgument w :: aux raw_mode ws
	| [] ->
	   []
      and select_option_arguments raw_mode w ws condition =
	let options, ws = ExtPervasives.take_until condition ws in
	Option (w, options) :: aux raw_mode ws
      in
      List.sort compare (aux false expanded_arguments)
    in
    let rec argument_scheme accu = function
      | [] ->
	 SimpleArguments
      | Option (o, ws) :: arguments ->
	 UseOption (o, List.length ws, argument_scheme accu arguments)
      | SimpleArgument _ :: arguments ->
	 argument_scheme (accu + 1) arguments
    in
    let scheme = argument_scheme 0 parsed_arguments in
    let scheme_arguments = Hashtbl.(try find categories scheme with _ -> []) in
    Hashtbl.(replace categories scheme ((arguments, filename) :: scheme_arguments));
    categories

  let commands = Hashtbl.create 13

  let count_use command filename options =
    let argument_categories, filenames =
      try Hashtbl.find commands command with Not_found -> Hashtbl.create 13, []
    in
    let argument_categories = classify_arguments command filename options argument_categories in
    Hashtbl.replace commands command (argument_categories, filename :: filenames)

  let rec words_of_suffix = function
    | CmdSuffix_IoRedirect _ ->
       []
    | CmdSuffix_CmdSuffix_IoRedirect (s, _) ->
       words_of_suffix' s
    | CmdSuffix_Word w ->
       [w]
    | CmdSuffix_CmdSuffix_Word (s, w) ->
       w :: words_of_suffix' s

  and words_of_suffix' s = words_of_suffix s.value

  module StringMap = Map.Make (String)

  let expand_variable env (Word (w, _)) =
    let is_var =
      "\\$\\([a-zA-Z_][a-zA-Z_0-9@*#?$!0-]*\\)"
    in
    let lookup_var s =
      try
	StringMap.find (Str.matched_group 1 s) env
      with Not_found ->
	s
    in
    Word (Str.(global_substitute (regexp is_var) lookup_var w), [])

  let count_command env filename cst =
    let module Counter = struct
        class iterator' = object(self)

	  inherit [_] Libmorbig.CST.iter

	  (* method symbolic_evaluation_of_complete_command = FirstSuccessMonad.(function *)
	  (*   | CompleteCommand_CList ( *)
	  (*     CList_AndOr (AndOr_Pipeline (Pipeline_PipeSequence ( *)
	  (*       PipeSequence_Command (Command_SimpleCommand (SimpleCommand_CmdName ( *)
	  (* 	CmdName_Word (Word name)))) *)
	  (*     )))) -> *)
	  (*      return name *)
          (*   | CompleteCommand_CList_Separator ( *)
          (*     CList_AndOr (AndOr_Pipeline (Pipeline_PipeSequence ( *)
	  (*       PipeSequence_Command (Command_SimpleCommand (SimpleCommand_CmdName ( *)
	  (* 	CmdName_Word (Word (name, _))))) *)
	  (*     ))), separator) -> *)
	  (*     return name *)
	  (*   | _ -> *)
	  (*     fail *)
	  (* ) *)

          (* method symbolic_evaluation_of_complete_command' x = symbolic_evaluation_of_complete_command x.value *)

	  method assign = fun var (Word (w, _)) ->
	    env := StringMap.add var w !env

	  method! visit_cmd_prefix venv = function
	    | CmdPrefix_IoRedirect i ->
               self#visit_io_redirect' venv i
	    | CmdPrefix_CmdPrefix_IoRedirect (cp, i) ->
	       self#visit_cmd_prefix' venv cp;
	       self#visit_io_redirect' venv i
	    | CmdPrefix_AssignmentWord aw ->
               let (Name var, cs) = aw.value in
               self#assign var cs
	    | CmdPrefix_CmdPrefix_AssignmentWord (cp, aw) ->
               let (Name var, cs) = aw.value in
	       self#visit_cmd_prefix' venv cp;
	       self#assign var cs

	  method! visit_simple_command venv s =
	    let args_of_suffix suffix =
              List.(map (fun word' -> expand_variable !env word'.value) (rev (words_of_suffix suffix)))
	    in
	    let cp, w, arguments = match s with
	      | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, cw, suffix) ->
                 let CmdWord_Word w = cw.value in
                 (Some cp, Some w, args_of_suffix suffix.value)
	      | SimpleCommand_CmdName_CmdSuffix (cw, suffix) ->
                 let CmdName_Word w = cw.value in
	         (None, Some w, args_of_suffix suffix.value)
	      | SimpleCommand_CmdPrefix_CmdWord (cp, cw) ->
                 let CmdWord_Word w = cw.value in
	         (Some cp, Some w, [])
	      | SimpleCommand_CmdName cw ->
                 let CmdName_Word w = cw.value in
	         (None, Some w, [])
	      | SimpleCommand_CmdPrefix cp ->
	         (Some cp, None, [])
	    in
            option_iter cp (self#visit_cmd_prefix' ());
            option_iter w (fun w -> count_use (expand_variable !env w.value) filename arguments)
        end
      end
    in
    (new Counter.iterator')#visit_complete_command () cst

  let string_of_command command =
    String.escaped (ExtPervasives.string_cut_at 60 command)

  let process_script filename csts =
    let env = ref StringMap.empty in
    List.iter (count_command env filename) csts

  let output_synthesis report =
    Hashtbl.fold (fun command (_, filenames) s ->
        let count = List.length filenames in
        let count_distinct = List.length (ExtPervasives.uniq filenames) in
        (command, (count, count_distinct)) :: s
      ) commands []
    |> List.sort (fun (_, (c1, _)) (_, (c2, _)) -> - compare c1 c2)
    |> List.iter (fun (Word (command, _), (count, count_distinct)) ->
           let s = string_of_command command in
           let s = if s = "[" then s else "[[" ^ s ^ "]]" in
           Report.add report "%05d %05d %s\n" count count_distinct s)

  let number_of_options_occurrences categories =
    let count = Hashtbl.create 13 in
    let incr k o = Hashtbl.(replace count o (k + try find count o with _ -> 0)) in
    Hashtbl.iter (fun scheme instances ->
        List.iter (incr (List.length instances)) (options_of_scheme scheme)
      ) categories;
    ExtPervasives.hashtbl_to_list count
    |> List.sort (fun (_, o1) (_, o2) -> - compare o1 o2)

  let filename_of_command command =
    let filename =
      match command with
      | "." -> "dot"
      | "[" -> "test"
      | command when String.contains command '/' ->
         (String.map (function '/' -> '_' | c -> c) command)
      | command -> command
    in
    if String.length filename > 20 then
      String.sub filename 0 20
    else
      filename

  let output_details report =
    let show_options command (options, filename) =
      "[[file:" ^ filename ^ "]]:\n" ^
	"       " ^ command ^ " "
        ^ String.concat " " (List.map unWord options)
    in
    let show_category report command (scheme, instances) =
      Report.add report "**** [%05d] %s\n" (List.length instances) (string_of_arguments_scheme scheme);
      List.iter (fun a -> Report.add report "     - %s\n" (show_options command a)) instances
    in
    let l = ref [] in
    Hashtbl.iter (fun k (a, f) -> l := (k, a, List.length f) :: !l) commands;
    l := List.sort (fun (_, _, x) (_, _, y) -> - compare x y) !l;
    List.iter (fun (Word (command, _), categories, nb_occurrences) ->
        let subreport = Report.create_subreport report command in
        Report.add subreport "*** [%05d] %s \n" nb_occurrences (string_of_command command);
        Report.add subreport "**** Number of occurrences per options\n";
        List.iter
	  (fun (o, i) -> Report.add subreport "      - [%05d] %s\n" i o)
	  (number_of_options_occurrences categories);
        List.(
	  ExtPervasives.hashtbl_to_list categories
	  |> sort (fun (_, i1) (_, i2) -> - compare (length i1) (length i2))
	  |> iter (show_category subreport command)
        )
      )  !l

  module StringSet = Set.Make (String)
  let output_covering report =
    exotic_command_levels := List.rev !exotic_command_levels;
    let use_exotic_commands = Array.make (List.length !exotic_command_levels) StringSet.empty in
    Hashtbl.iter (fun command (_, filenames) ->
        let count_distinct = List.length (ExtPervasives.uniq filenames) in
        List.iteri (fun i level ->
	    if count_distinct <= level then
	      List.iter (fun filename ->
	          use_exotic_commands.(i) <- StringSet.add filename use_exotic_commands.(i)
	        ) filenames
          ) !exotic_command_levels
      ) commands;
    let nb_files = List.length (Options.files ()) in
    Report.add report "  |-------|-------|--------|\n";
    Report.add report "  | Level |       |        |\n";
    Report.add report "  |-------|-------|--------|\n";
    List.iteri (fun i level ->
        let nb = StringSet.cardinal use_exotic_commands.(i) in
        let p = float_of_int nb *. 100. /. float_of_int nb_files in
        Report.add report "  | %05d | %05d | %5.2f%% |\n" level nb p
      ) !exotic_command_levels;
    Report.add report "  |-------|-------|--------|\n"

  let output_report report =
    Report.add report "- The first column is the number of times the command appears in the corpus.\n";
    Report.add report "- The second column is the number of distinct files where the command is used.\n";
    output_synthesis report;

    Report.add report
      "* Covering

For each levels provided to the command with the '-i' option, we compute
the number of scripts the use at least one command which is exotic given
the threshold.

";
    output_covering report;

    output_details report
end

let install = Analyzer.register (module Self)
