(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

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

    method output_occurrences ?(with_name=true) report =
      if !files <> [] then
        (
          Report.add report "*** %s\n" (if with_name then name else "Occurrences");
          List.iter (fun (filename, string) -> Report.add report "- %s\n  %s\n" (Report.link_to_source report filename) string) !files
        )

    method output_report report =
      Report.add report "** %s\n- %d occurrences in %d files\n" name !value (List.length !files);
      self#output_occurrences ~with_name:false report
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

    method output_report report =
      Report.add report "** Dynamic commands

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

      param_at_counter#output_occurrences report;
      param_at_quoted_counter#output_occurrences report;
      param_star_counter#output_occurrences report;
      param_star_quoted_counter#output_occurrences report;
      variables_counter#output_occurrences report;
      variables_quoted_counter#output_occurrences report;
      subprocess_counter#output_occurrences report;
      subprocess_quoted_counter#output_occurrences report
  end

  let ifs_counter = new counter "IFS"
  let cmd_string_counter = new cmd_string_counter

  let process_script filename csts =
    let counter = object (self)
      inherit [_] Libmorbig.CST.iter as super

      method! visit_cmd_prefix () = function
        | CmdPrefix_IoRedirect i ->
           self#visit_io_redirect' () i
        | CmdPrefix_CmdPrefix_IoRedirect (cp, i) ->
           self#visit_cmd_prefix' () cp;
           self#visit_io_redirect' () i
        | CmdPrefix_AssignmentWord aw ->
           let (Name n, _) = aw.value in
           if n = "IFS" then
             ifs_counter#handle filename "" (*FIXME: (pp_to_string pp_cmd_prefix cp) *)
        | CmdPrefix_CmdPrefix_AssignmentWord (cp, aw) ->
           self#visit_cmd_prefix' () cp;
           let (Name n, _) = aw.value in
           if n = "IFS" then
             ifs_counter#handle filename "" (*FIXME: (pp_to_string pp_cmd_prefix cp) *)

      method! visit_simple_command () sc =
        let ssc = "" in (*FIXME: pp_to_string pp_simple_command sc in *)
        match sc with
        | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, cw, cs) ->
           let CmdWord_Word w' = cw.value in
           self#visit_cmd_prefix' () cp;
           self#visit_cmd_word' () cw;
           self#visit_cmd_suffix' () cs;
           cmd_string_counter#handle filename (unWord w'.value) ssc

        | SimpleCommand_CmdPrefix_CmdWord (cp, cw) ->
           let CmdWord_Word w' = cw.value in
           self#visit_cmd_prefix' () cp;
           self#visit_cmd_word' () cw;
           cmd_string_counter#handle filename (unWord w'.value) ssc

        | SimpleCommand_CmdPrefix cp ->
           self#visit_cmd_prefix' () cp

        | SimpleCommand_CmdName_CmdSuffix (cn, cs) ->
           let CmdName_Word w' = cn.value in
           self#visit_cmd_name' () cn;
           self#visit_cmd_suffix' () cs;
           cmd_string_counter#handle filename (unWord w'.value) ssc

        | SimpleCommand_CmdName cn ->
           let CmdName_Word w' = cn.value in
           self#visit_cmd_name' () cn;
           cmd_string_counter#handle filename (unWord w'.value) ssc
    end in
    List.iter (counter#visit_complete_command ()) csts

  let output_report report =
    let open Report in
    ifs_counter#output_report report;
    cmd_string_counter#output_report report
end

let install = Analyzer.register (module Self)
