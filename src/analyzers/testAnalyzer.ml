(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Morbig.CST
open Options
open Messages
open Morsmall.Utilities.TestParser

module Name =
  struct
    type t = string
    let compare = Pervasives.compare
  end

module NameSet = Set.Make(Name)

let unCmdName' {value=CmdName_Word word} =
  Morbig.CSTHelpers.unWord word.value
let unCmdWord' {value=CmdWord_Word word} =
  Morbig.CSTHelpers.unWord word.value
let unWord' {value=word} =
  Morbig.CSTHelpers.unWord word
                                                                       
let extract_command = function
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (_,cmdword',_)
  | SimpleCommand_CmdPrefix_CmdWord (_,cmdword')
    -> Some (unCmdWord' cmdword')
  | SimpleCommand_CmdName_CmdSuffix (cmdname',_)
  | SimpleCommand_CmdName cmdname'
    -> Some (unCmdName' cmdname')
  | SimpleCommand_CmdPrefix _prefix
    -> None

let rec extract_arguments_from_suffix = function
  | CmdSuffix_IoRedirect io_redirect
    -> []
  | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix',_io_redirect')
    -> extract_arguments_from_suffix cmd_suffix'.value
  | CmdSuffix_Word word'
    -> [ unWord' word' ]
  | CmdSuffix_CmdSuffix_Word (cmd_suffix',word')
    -> (unWord' word') :: (extract_arguments_from_suffix cmd_suffix'.value) 

let extract_arguments = function
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (_,_,suffix')
  | SimpleCommand_CmdName_CmdSuffix (_,suffix')
    -> List.rev (extract_arguments_from_suffix suffix'.value)
  | SimpleCommand_CmdPrefix_CmdWord _
  | SimpleCommand_CmdPrefix _
  | SimpleCommand_CmdName _
    -> []


class stringcounter = object (self)
  val counters : (string,int) Hashtbl.t = Hashtbl.create 8

  method incr key =
    try
      let oldval = Hashtbl.find counters key in
      Hashtbl.replace counters key (oldval+1)
    with
      Not_found ->
      Hashtbl.add counters key 1

  method iter_ascending f =
    List.iter f 
      (List.sort
         (fun (_,x) (_,y) -> y-x)
         (Hashtbl.fold (fun key value acc -> (key,value)::acc) counters []))
end

class indexed_sets = object (self)
  val sets : (string,NameSet.t) Hashtbl.t = Hashtbl.create 8

  method add key value =
    try
      let oldset = Hashtbl.find sets key in
      Hashtbl.replace sets key (NameSet.add value oldset) 
    with
      Not_found ->
      Hashtbl.add sets key (NameSet.singleton value)

  method iter f =
    Hashtbl.iter f sets

end

(* these are the possible first arguments of maintainer scripts, as given
 by policy v 4.1.4, section 6.5. *)
let canonical_first_arguments = [
    "abort-deconfigure";
    "abort-install";
    "abort-remove";
    "abort-upgrade";
    "configure";
    "deconfigure";
    "disappear";
    "failed-upgrade";
    "install";
    "purge";
    "remove";
    "upgrade"
  ]

let options = []

let name = "test"

let parsing_errors = ref []
let count_uniops = new stringcounter
let count_binops = new stringcounter
let count_contest = ref 0
let count_testinvocations = ref 0
let scripts_with_complex_tests = ref NameSet.empty
let count_complex_tests = ref 0
let count_dollarone = new stringcounter
let scripts_with_strange_dollarone = new indexed_sets

(* indicates whether we are in a function definition *)
let in_fundef = ref false

let process_script filename csts =

  let rec process_expr = function
    | And (e1,e2) -> process_expr e1; process_expr e2
    | Or  (e1,e2) -> process_expr e1; process_expr e2
    | Not e       -> process_expr e
    | Binary (op,left,right) -> begin
        count_binops#incr(op);
        if not (!in_fundef) &&
             (op = "=" || op = "!=") && (left="$1"||right="$1")
        then let otherarg =
               if left = "$1" then right else left
             in begin
                 count_dollarone#incr(otherarg);
                 if not (List.mem otherarg canonical_first_arguments)
                 then
                   scripts_with_strange_dollarone#add otherarg filename
               end
      end
    | Unary (op,_) -> count_uniops#incr(op)
    | Single _ -> incr count_contest
  in
  let has_bool_combinators = function
    | And _ | Or _  -> true
    | _ -> false
  in
  let register_test filename invocation arguments =
    let arguments_unquoted = List.map Morbig.remove_quotes arguments
    and bracket = (invocation = "[" )
    in
    incr count_testinvocations;
    try
      match parse ~bracket arguments_unquoted with
      | None -> ()
      | Some ast -> process_expr ast;
                    if has_bool_combinators ast
                    then begin
                        scripts_with_complex_tests :=
                          NameSet.add filename (!scripts_with_complex_tests);
                        incr count_complex_tests
                      end
    with
      Parse_error ->
      parsing_errors := (filename, invocation, arguments) :: !parsing_errors
  in
  
  let counter = object(self)
                  inherit [_] Morbig.CST.iter as super

                  method! visit_simple_command () csts =
                    let invocation = extract_command csts in
                    match invocation with
                    | Some s when (s = "test" || s = "[" )
                      -> register_test filename s (extract_arguments csts)
                    | _
                      -> super#visit_simple_command () csts

                  method! visit_command () csts = match csts with
                    | Command_FunctionDefinition fdef' ->
                       in_fundef := true;
                       self#visit_function_definition' () fdef';
                       in_fundef := false
                    | _ -> super#visit_command () csts
                end in
  counter#visit_program () csts
  
let output_report report =
  Report.add report "* Test invocations\n";
  Report.add report "  Number of test or []: %d\n" !count_testinvocations; 

  Report.add report "** Tests that could not be parsed\n\n";
  List.iter
    (function (filename,invocation,arguments) ->
       Report.add report "    - %s\n    "
         (Report.link_to_source report filename);
       Report.add report "%s " invocation; 
       List.iter (function s -> Report.add report " %s" s) arguments;
       Report.add report "\n"
    )
    !parsing_errors;

  Report.add report "** Unary test operators\n\n";
  Report.add report "  Operator | Occurrences\n";
  Report.add report "  ---------+------------\n";
  count_uniops#iter_ascending (fun (key,number) ->
      Report.add report "   %5s   | %8d \n" key number);
  Report.add report "\n";

  Report.add report "** Binary test operators\n\n";
  Report.add report "  Operator | Occurrences\n";
  Report.add report "  ---------+------------\n";
  count_binops#iter_ascending (fun (key,number) ->
      Report.add report "   %5s   | %8d \n" key number);
  Report.add report "\n";

  Report.add report "** Tests using boolean operators (-a, -o)\n\n";
  Report.add report "  Number of tests: %d\n" !count_complex_tests;
  Report.add report "  Number of scripts: %d\n"
    (NameSet.cardinal !scripts_with_complex_tests);
  Report.add report "*** Listing of scripts\n\n";
  NameSet.iter
    (function filename ->
       Report.add report "    - %s\n"
         (Report.link_to_source report filename))
    !scripts_with_complex_tests;
  Report.add report "\n";
  
  Report.add report "** Comparisons with $1 (outside function definitions)\n\n";
  Report.add report "  Compared with           | Occurrences\n";
  Report.add report "  ------------------------+------------\n";
  count_dollarone#iter_ascending (fun (key,number) ->
      Report.add report "   %20s   | %8d \n" key number);
  Report.add report "*** Listing of scripts (canonical arguments for $1 omitted)\n";
  scripts_with_strange_dollarone#iter
    (fun key set ->
      Report.add report "   $1 compared with: %s\n" key;
      NameSet.iter
        (function filename ->
           Report.add report "   %s\n"
             (Report.link_to_source report filename))
        set;
      Report.add report "\n"
    );
  Report.add report "\n";
