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

let name = "structures"
let options = []

module Handler = struct
  class ['a] baseHandler (name: string) = object (self)
    val counter = ref 0

    val files_counter = ref 0
    val filenames = ref []
    val last_filename = ref ""

    method handle filename (_subtree: 'a) =
      if !last_filename <> filename then
        (
          incr files_counter;
          filenames := filename :: !filenames;
          last_filename := filename
        );
      incr counter

    method output_report report =
      Report.add report "** %s\n- %d occurrences in %d files\n" name !counter !files_counter;
      if !files_counter > 0 then
        (
          Report.add report "*** Files\n";
          List.iter (fun f ->
              Report.link_to_source report f
              |> Report.add report "- %s\n") !filenames
        )
  end

  class forHandler (name: string) = object (self)
    inherit [for_clause] baseHandler name as super

    val variables_counter = new Counters.occCounter "variables"
    val subshells_counter = new Counters.occCounter "subshells"
    val globs_counter = new Counters.occCounter "globs"

    method count_dollar_in_word filename representation w =
      let string_mem s c =
        try let _ = String.index s c in true
        with Not_found -> false
      in
      (
        let s = unWord w in
        try
          let i = String.index s '$' in
          if String.length s > i + 1 && s.[i + 1] = '(' then
            subshells_counter#add
          else
            variables_counter#add
        with
          Not_found ->
          if string_mem s '`' then
            subshells_counter#add
          else if string_mem s '*' then
            globs_counter#add
          else if string_mem s '?' then
            globs_counter#add
          else
            (fun _ _ -> ())
      )
        filename representation

    method count_dollars_in_wordlist filename representation = function
      | WordList_WordList_Word (wordlist, word) ->
         self#count_dollars_in_wordlist filename representation wordlist.value;
         self#count_dollar_in_word filename representation word.value
      | WordList_Word word ->
         self#count_dollar_in_word filename representation word.value

    method handle filename for_clause =
      super#handle filename for_clause;
      match for_clause with
      | ForClause_For_Name_DoGroup _
      | ForClause_For_Name_SequentialSep_DoGroup _
      | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup _ -> ()
      | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (_, _, wordlist', _, _) ->
         self#count_dollars_in_wordlist filename "" (*FIXME: (pp_to_string pp_for_clause for_clause)*) wordlist'.value

    method output_report report =
      Report.add report
        "** %s\n%d occurrences in %d files\n"
        name !counter !files_counter;

      Report.add report
        "*** %d (%f%%) contain a variable\n"
        (variables_counter#n_occurrences())
        (percentage (variables_counter#n_occurrences()) !counter);
      variables_counter#output_occurrences report;

      Report.add report
        "*** %d (%f%%) contain a subshell\n"
        (subshells_counter#n_occurrences())
        (percentage (subshells_counter#n_occurrences()) !counter);
      subshells_counter#output_occurrences report;

      Report.add report
        "*** %d (%f%%) contain a glob\n"
        (globs_counter#n_occurrences())
        (percentage (globs_counter#n_occurrences()) !counter);
      globs_counter#output_occurrences report
  end

  class caseHandler (name: string) = object (self)
    inherit [case_clause] baseHandler name as super

    val dollar1 = ref 0

    method handle filename case_clause =
      super#handle filename case_clause;
      match case_clause with
      | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (w, _, _, _)
      | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (w, _, _, _)
      | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (w, _, _) ->
         if unWord w.value = "$1" || unWord w.value = "\"$1\"" then
           incr dollar1

    method output_report report =
      super#output_report report;
      Report.add report "*** Details\n- %d (%f%%) of them are matching on $1\n" !dollar1 (percentage !dollar1 !counter)
  end

  class whileHandler (name: string) = object (self)
    inherit [while_clause] baseHandler name as super

    val reads = ref 0

    method handle_while_compound_list = function
      | CompoundList_Term t
      | CompoundList_NewLineList_Term (_, t)
      | CompoundList_Term_Separator (t, _)
      | CompoundList_NewLineList_Term_Separator (_, t, _) ->
         (
           match t.value with
           | Term_AndOr aop ->
              (
                match aop.value with
                | AndOr_Pipeline pps ->
                   (
                     match pps.value with
                     | Pipeline_PipeSequence psc ->
                        (
                          match psc.value with
                          | PipeSequence_Command csc ->
                             (
                               match csc.value with
                               | Command_SimpleCommand sc ->
                                  (
                                    match sc.value with
                                    | SimpleCommand_CmdPrefix _ -> ()
                                    | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (_, cww, _)
                                    | SimpleCommand_CmdPrefix_CmdWord (_, cww) ->
                                       let CmdWord_Word w = cww.value in
                                       if unWord w.value = "read" then
                                         incr reads
                                    | SimpleCommand_CmdName_CmdSuffix (cnw, _)
                                    | SimpleCommand_CmdName cnw ->
                                       let CmdName_Word w = cnw.value in
                                       if unWord w.value = "read" then
                                         incr reads
                                  )
                               | _ -> ()
                             )
                          | _ -> ()
                        )
                     | _ -> ()
                   )
                | _ -> ()
              )
           | _ -> ()
         )

    method handle filename while_clause =
      super#handle filename while_clause;
      match while_clause with
      | WhileClause_While_CompoundList_DoGroup (cl, _) ->
         self#handle_while_compound_list cl.value

    method output_report report =
      super#output_report report;
      Report.add report "*** Details\n- %d (%f%%) of them are using 'read'\n" !reads (percentage !reads !counter)
  end
end

type base_handler = If | Subshell | Uppersand | While | Until | Pipe | And | Or | Not

let base_handlers = [
    If        , new Handler.baseHandler "if"        ;
    Subshell  , new Handler.baseHandler "subshell"  ;
    Until     , new Handler.baseHandler "until"     ;
    Pipe      , new Handler.baseHandler "pipe"      ;
    Uppersand , new Handler.baseHandler "uppersand" ;
    And       , new Handler.baseHandler "and"       ;
    Or        , new Handler.baseHandler "or"        ;
    Not       , new Handler.baseHandler "not"       ;
  ]

let get_base_handler name =
  List.assoc name base_handlers

let for_handler = new Handler.forHandler "for"
let case_handler = new Handler.caseHandler "case"
let while_handler = new Handler.whileHandler "while"

let process_script filename csts =
  let counter = object (self)
                  inherit [_] Libmorbig.CST.iter as super

                  method! visit_and_or () = function
                    | AndOr_Pipeline p ->
                       self#visit_pipeline' () p
                    | AndOr_AndOr_AndIf_LineBreak_Pipeline (a, l, p) ->
                       (get_base_handler And)#handle filename ();
                       self#visit_and_or' () a;
                       self#visit_linebreak' () l;
                       self#visit_pipeline' () p
                    | AndOr_AndOr_OrIf_LineBreak_Pipeline (a, l, p) ->
                       (get_base_handler Or)#handle filename ();
                       self#visit_and_or' () a;
                       self#visit_linebreak' () l;
                       self#visit_pipeline' () p

                  method! visit_pipeline () = function
                    | Pipeline_PipeSequence p ->
                       self#visit_pipe_sequence' () p
                    | Pipeline_Bang_PipeSequence p ->
                       (get_base_handler Not)#handle filename ();
                       self#visit_pipe_sequence' () p

                  method! visit_pipe_sequence () = function
                    | PipeSequence_Command c ->
                       self#visit_command' () c
                    | PipeSequence_PipeSequence_Pipe_LineBreak_Command (p, l, c) ->
                       (get_base_handler Pipe)#handle filename ();
                       self#visit_pipe_sequence' () p;
                       self#visit_linebreak' () l;
                       self#visit_command' () c

                  method! visit_compound_command () = function
                    | CompoundCommand_BraceGroup b ->
                       self#visit_brace_group' () b
                    | CompoundCommand_Subshell s ->
                       (get_base_handler Subshell)#handle filename ();
                       self#visit_subshell' () s
                    | CompoundCommand_ForClause f ->
                       for_handler#handle filename f.value;
                       self#visit_for_clause' () f
                    | CompoundCommand_CaseClause c ->
                       case_handler#handle filename c.value;
                       self#visit_case_clause' () c
                    | CompoundCommand_IfClause i ->
                       (get_base_handler If)#handle filename ();
                       self#visit_if_clause' () i
                    | CompoundCommand_WhileClause w ->
                       while_handler#handle filename w.value;
                       self#visit_while_clause' () w
                    | CompoundCommand_UntilClause u ->
                       (get_base_handler Until)#handle filename ();
                       self#visit_until_clause' () u

                  method! visit_separator_op () = function
                    | SeparatorOp_Uppersand ->
                       (get_base_handler Uppersand)#handle filename ()
                    | SeparatorOp_Semicolon ->
                       ()
                end in
  List.iter (counter#visit_complete_command ()) csts

let output_report report =
  Report.add report "* Structures\n";
  List.iter (fun (_, h) -> h#output_report report) base_handlers;
  for_handler#output_report report;
  case_handler#output_report report;
  while_handler#output_report report
