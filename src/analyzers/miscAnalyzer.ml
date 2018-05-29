open ExtPervasives
open Libmorbig.CST
open Libmorbig.CSTHelpers
open Options
open Commands
open Messages

module Self : Analyzer.S = struct
  let name = "miscellaneous"
  let options = []

  class counter (name: string) = object (self)
    val value = ref 0
    val files = ref []

    method get_value () =
      !value
              
    method handle filename string =
      if !files = [] || fst (List.hd !files) <> filename then
          files := (filename, string) :: !files;
      incr value

    method output_occurrences ?(with_name=true) f =
      if !files <> [] then
        (
          Report.fprintf f "*** %s\n" (if with_name then name else "Occurrences");
          List.iter (fun (filename, string) -> Report.fprintf f "- [[file:%s]]\n  %s\n" filename string) !files
        )
      
    method output_report f =
      Report.fprintf f "** %s\n- %d occurrences in %d files\n" name !value (List.length !files);
      self#output_occurrences ~with_name:false f
  end

  class cmd_string_counter = object (self)
    val variables_counter = new counter "Variables"
    val variables_quoted_counter = new counter "Variables with double-quotes"
    val subprocess_counter = new counter "Subprocesses"
    val subprocess_quoted_counter = new counter "Subprocesses with double-quotes"
    val param_at_counter = new counter "Parameter $@"
    val param_at_quoted_counter = new counter "Parameter $@ with double-quotes"
    val param_star_counter = new counter "Parameter $*"
    val param_star_quoted_counter = new counter "Parameter $* with double-quotes"

    method handle filename command representation =
      (
        match command with
        |   "$@"   -> param_at_counter#handle
        | "\"$@\"" -> param_at_quoted_counter#handle
        |   "$*"   -> param_star_counter#handle
        | "\"$*\"" -> param_star_quoted_counter#handle
        | _ as s ->
           if s.[0] = '$' then
             (
               if s.[1] = '(' then
                 subprocess_counter#handle
               else
                 variables_counter#handle
             )
           else if s.[0] = '"' then
             (
               if s.[1] = '$' then
                 (
                   if s.[2] = '(' then
                     subprocess_quoted_counter#handle
                   else
                     variables_quoted_counter#handle
                 )
               else
                 (fun _ _ -> ())
             )
           else
             (fun _ _ -> ())
      )
        filename representation

    method output_report f =
      Report.fprintf f "** Dynamic commands

|           | With quotes | Without quotes | Total |
|-----------|-------------|----------------|-------|
|     $@    | %11d | %14d | %5d |
|     $*    | %11d | %14d | %5d |
| Variables | %11d | %14d | %5d |
|     $(    | %11d | %14d | %5d |
|-----------|-------------|----------------|-------|
|   Total   | %11d | %14d | %5d |
\n"
                    (param_at_quoted_counter#get_value ())
                    (param_at_counter#get_value ())
                    ((param_at_counter#get_value ()) + (param_at_quoted_counter#get_value ()))
      
                    (param_star_quoted_counter#get_value ())
                    (param_star_counter#get_value ())
                    ((param_star_counter#get_value ()) + (param_star_quoted_counter#get_value ()))
      
                    (variables_quoted_counter#get_value ())
                    (variables_counter#get_value ())
                    ((variables_counter#get_value ()) + (variables_quoted_counter#get_value ()))

                    (subprocess_quoted_counter#get_value ())
                    (subprocess_counter#get_value ())
                    ((subprocess_counter#get_value ()) + (subprocess_quoted_counter#get_value ()))

                    0 0 0;
      
      param_at_counter#output_occurrences f;
      param_at_quoted_counter#output_occurrences f;
      param_star_counter#output_occurrences f;
      param_star_quoted_counter#output_occurrences f;
      variables_counter#output_occurrences f;
      variables_quoted_counter#output_occurrences f;
      subprocess_counter#output_occurrences f;
      subprocess_quoted_counter#output_occurrences f
  end                                  
                               
  let ifs_counter = new counter "IFS" 
  let cmd_string_counter = new cmd_string_counter
                  
  let process_script filename csts =
    let module Counter =
      struct
        class iterator' = object (self)
	  inherit [_] Libmorbig.CST.iter as super

          method! visit_cmd_prefix venv = function
            | CmdPrefix_IoRedirect i ->
               self#visit_io_redirect' venv i
            | CmdPrefix_CmdPrefix_IoRedirect (cp, i) ->
               self#visit_cmd_prefix' venv cp;
               self#visit_io_redirect' venv i
            | CmdPrefix_AssignmentWord aw ->
               let (Name n, _) = aw.value in
               if n = "IFS" then
                 ifs_counter#handle filename "" (*FIXME: (pp_to_string pp_cmd_prefix cp) *)
            | CmdPrefix_CmdPrefix_AssignmentWord (cp, aw) ->
               self#visit_cmd_prefix' venv cp;
               let (Name n, _) = aw.value in
               if n = "IFS" then
                 ifs_counter#handle filename "" (*FIXME: (pp_to_string pp_cmd_prefix cp) *)

          method! visit_simple_command venv sc =
            let ssc = "" in (*FIXME: pp_to_string pp_simple_command sc in *)
            match sc with
            | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, cw, cs) ->
               let CmdWord_Word w' = cw.value in
               self#visit_cmd_prefix' venv cp;
               self#visit_cmd_word' venv cw;
               self#visit_cmd_suffix' venv cs;
               cmd_string_counter#handle filename (unWord w'.value) ssc
               
            | SimpleCommand_CmdPrefix_CmdWord (cp, cw) ->
               let CmdWord_Word w' = cw.value in
               self#visit_cmd_prefix' venv cp;
               self#visit_cmd_word' venv cw;
               cmd_string_counter#handle filename (unWord w'.value) ssc
               
            | SimpleCommand_CmdPrefix cp ->
               self#visit_cmd_prefix' venv cp
              
            | SimpleCommand_CmdName_CmdSuffix (cn, cs) ->
               let CmdName_Word w' = cn.value in
               self#visit_cmd_name' venv cn;
               self#visit_cmd_suffix' venv cs;
               cmd_string_counter#handle filename (unWord w'.value) ssc
               
            | SimpleCommand_CmdName cn ->
               let CmdName_Word w' = cn.value in
               self#visit_cmd_name' venv cn;
               cmd_string_counter#handle filename (unWord w'.value) ssc
        end
      end
    in
    List.iter ((new Counter.iterator')#visit_complete_command ()) csts

  let output_report () =
    let open Report in
    let f = open_file name in
    fprintf f "#+TITLE: Miscellaenous Analyzer\n";
    ifs_counter#output_report f;
    cmd_string_counter#output_report f;
    close_file f
end

let install = Analyzer.register (module Self)
