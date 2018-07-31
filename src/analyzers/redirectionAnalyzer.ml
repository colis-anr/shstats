(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Libmorbig.CST

let name = "redirection"
let options = []

(* where a redirection can occur. *)
(* FIXME: prefix/suffix? *)
(* FIXME: better name *)

type location_simple =
  Prefix | Suffix | Both

type location =
  Assignment | Compound | Function | Simple of location_simple

let pp_location fmt location =
  Format.pp_print_string
    fmt
    (match location with
     | Assignment -> "assignment"
     | Compound -> "compound command"
     | Function -> "function definition"
     | Simple Prefix -> "prefix of a simple command"
     | Simple Suffix -> "suffix of a simple command"
     | Simple Both -> "both prefix and suffix of a simple command")

(* ============== [ Abstract representation of redirections ] =============== *)

let is_expandable word =
  String.index_opt word '$' <> None

let is_quoted word =
  String.index_opt word '\'' <> None
  || String.index_opt word '"' <> None
  || String.index_opt word '\\' <> None

type a_descr =
  | Int of int
  | Other

let pp_a_descr fmt = function
  | Int i -> Format.pp_print_int fmt i
  | Other -> ()

let a_descr_of_io_number = function
  | IONumber n ->
     try Int (int_of_string n)
     with _ -> Other

let a_descr_of_filename = function
  | Filename_Word {value=Word (word, _);_} ->
     try Int (int_of_string word)
     with _ -> Other

type a_file =
  | DevNull
  | Other

let pp_a_file fmt a_file =
  Format.pp_print_string
    fmt
    (match a_file with
     | DevNull -> "/dev/null"
     | Other -> "")

let a_file_of_filename = function
  | Filename_Word {value=Word (word, _);_} ->
     if word = "/dev/null" then
       DevNull
     else
       Other

type a_redirection =
  | Output of a_descr * a_file
  | OutputClobber of a_descr * a_file
  | OutputAppend of a_descr * a_file
  | OutputDuplicate of a_descr * a_descr
  | Input of a_descr * a_file
  | InputDuplicate of a_descr * a_descr
  | InputOutput of a_descr * a_file
  | Here of a_descr * bool

let pp_a_redirection fmt =
  let p = Format.fprintf in
  function
  | Output (a_descr, a_file) ->
     p fmt "%a>%a" pp_a_descr a_descr pp_a_file a_file
  | OutputClobber (a_descr, a_file) ->
     p fmt "%a>|%a" pp_a_descr a_descr pp_a_file a_file
  | OutputAppend (a_descr, a_file) ->
     p fmt "%a>>%a" pp_a_descr a_descr pp_a_file a_file
  | OutputDuplicate (a_descr_1, a_descr_2) ->
     p fmt "%a>&%a" pp_a_descr a_descr_1 pp_a_descr a_descr_2
  | Input (a_descr, a_file) ->
     p fmt "%a<%a" pp_a_descr a_descr pp_a_file a_file
  | InputDuplicate (a_descr_1, a_descr_2) ->
     p fmt "%a<&%a" pp_a_descr a_descr_1 pp_a_descr a_descr_2
  | InputOutput (a_descr, a_file) ->
     p fmt "%a<>%a" pp_a_descr a_descr pp_a_file a_file
  | Here (a_descr, strip) ->
     p fmt "%a<<%s" pp_a_descr a_descr (if strip then "-" else "")

let rec pp_a_redirection_list fmt = function
  | [] -> Format.fprintf fmt "{empty}"
  | [e] -> pp_a_redirection fmt e
  | h :: t -> Format.fprintf fmt "%a %a"
                pp_a_redirection h
                pp_a_redirection_list t

let a_redirection_of_io_file a_descr = function
  | IoFile_Less_FileName filename' ->
     Input (unwrap_or (Int 0) a_descr, a_file_of_filename filename'.value)
  | IoFile_LessAnd_FileName filename' ->
     InputDuplicate (unwrap_or (Int 0) a_descr, a_descr_of_filename filename'.value)
  | IoFile_Great_FileName filename' ->
     Output (unwrap_or (Int 1) a_descr, a_file_of_filename filename'.value)
  | IoFile_GreatAnd_FileName filename' ->
     OutputDuplicate (unwrap_or (Int 1) a_descr, a_descr_of_filename filename'.value)
  | IoFile_DGreat_FileName filename' ->
     OutputAppend (unwrap_or (Int 1) a_descr, a_file_of_filename filename'.value)
  | IoFile_LessGreat_FileName filename' ->
     InputOutput (unwrap_or (Int 0) a_descr, a_file_of_filename filename'.value)
  | IoFile_Clobber_FileName filename' ->
     OutputClobber (unwrap_or (Int 1) a_descr, a_file_of_filename filename'.value)

let a_redirection_of_io_here a_descr = function
  | IoHere_DLess_HereEnd (here_end', _) ->
     Here (unwrap_or (Int 0) a_descr, false)
  | IoHere_DLessDash_HereEnd (here_end', _) ->
     Here (unwrap_or (Int 0) a_descr, true)

let a_redirection_of_io_redirect = function
  | IoRedirect_IoFile io_file' ->
     a_redirection_of_io_file None io_file'.value
  | IoRedirect_IoNumber_IoFile (io_number, io_file') ->
     a_redirection_of_io_file (Some (a_descr_of_io_number io_number)) io_file'.value
  | IoRedirect_IoHere io_here' ->
     a_redirection_of_io_here None io_here'.value
  | IoRedirect_IoNumber_IoHere (io_number, io_here') ->
     a_redirection_of_io_here (Some (a_descr_of_io_number io_number)) io_here'.value

(* =============================== [ Result ] =============================== *)

type result =
  { filename : string ;
    location : location ;
    concrete : io_redirect' list ;
    abstract : a_redirection list }

let make_result filename location concrete =
  assert (concrete <> []);
  { filename ; location ; concrete ;
    abstract =
      List.map
        (fun io_redirect' -> a_redirection_of_io_redirect io_redirect'.value)
        concrete }

let make_one_result filename location concrete =
  [make_result filename location concrete]

let results : result list ref = ref []

(* ===================== [ Helpers about redirections ] ===================== *)

let cmd_prefix_to_io_redirect_list cmd_prefix' =
  let rec aux acc = function
    | CmdPrefix_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | CmdPrefix_CmdPrefix_IoRedirect (cmd_prefix', io_redirect') ->
       aux (io_redirect' :: acc) cmd_prefix'.value
    | CmdPrefix_AssignmentWord _ ->
       acc
    | CmdPrefix_CmdPrefix_AssignmentWord (cmd_prefix', _) ->
       aux acc cmd_prefix'.value
  in
  aux [] cmd_prefix'.value

let cmd_suffix_to_io_redirect_list cmd_suffix' =
  let rec aux acc = function
    | CmdSuffix_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix', io_redirect') ->
       aux (io_redirect' :: acc) cmd_suffix'.value
    | CmdSuffix_Word _ ->
       acc
    | CmdSuffix_CmdSuffix_Word (cmd_suffix', _) ->
       aux acc cmd_suffix'.value
  in
  aux [] cmd_suffix'.value

let redirect_list_to_io_redirect_list redirect_list' =
  let rec aux acc = function
    | RedirectList_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | RedirectList_RedirectList_IoRedirect (redirect_list', io_redirect') ->
       aux (io_redirect' :: acc) redirect_list'.value
  in
  aux [] redirect_list'.value

(* =========================== [ Process Script ] =========================== *)

let process_script filename csts =
  let visitor = object (self)
    inherit [_] reduce as super

    method zero : 'a list = []
    method plus (r : 'a list) (s : 'a list) = r @ s

    method! visit_command () command =
      self#plus
        (match command with
         | Command_CompoundCommand_RedirectList (_, redirect_list') ->
            (
              redirect_list_to_io_redirect_list redirect_list'
              |> make_one_result filename Compound
            )
         | _ -> self#zero)
        (super#visit_command () command)

    method! visit_function_body () function_body =
      self#plus
        (match function_body with
         | FunctionBody_CompoundCommand_RedirectList (_, redirect_list') ->
            (
              redirect_list_to_io_redirect_list redirect_list'
              |> make_one_result filename Function
            )
         | _ -> self#zero)
        (super#visit_function_body () function_body)

    method! visit_simple_command () simple_command =
      self#plus
        (match simple_command with
         | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', _, cmd_suffix') ->
            (
              let in_prefix = cmd_prefix_to_io_redirect_list cmd_prefix' in
              let in_suffix = cmd_suffix_to_io_redirect_list cmd_suffix' in
              match in_prefix, in_suffix with
              | [], [] -> []
              | _, [] -> make_one_result filename (Simple Prefix) in_prefix
              | [], _ -> make_one_result filename (Simple Suffix) in_suffix
              | _, _ -> make_one_result filename (Simple Both) (in_prefix @ in_suffix)
            )
         | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', _) ->
            (
              let content = cmd_prefix_to_io_redirect_list cmd_prefix' in
              if content = []
              then []
              else make_one_result filename (Simple Prefix) content
            )
         | SimpleCommand_CmdPrefix cmd_prefix' ->
            (
              let content = cmd_prefix_to_io_redirect_list cmd_prefix' in
              if content = []
              then []
              else make_one_result filename Assignment content
            )
         | SimpleCommand_CmdName_CmdSuffix (_, cmd_suffix') ->
            (
              let content = cmd_suffix_to_io_redirect_list cmd_suffix' in
              if content = []
              then []
              else make_one_result filename (Simple Suffix) content
            )
         | _ -> self#zero)
        (super#visit_simple_command () simple_command)
    end in
  visitor#visit_complete_command_list () csts
  |> (fun file_results -> file_results @ !results)
  |> ((:=) results)

(* =========================== [ Output Report ] ============================ *)

let output_result report result =
  Report.add report "- %s, line %d\n"
    (Report.link_to_source report result.filename)
    ((List.hd result.concrete)
       .position
       .start_p
       .pos_lnum)

let output_results report results =
  List.iter (output_result report) results

let output_report report =
  Report.add
    report
    "- %d io_redirect lists found\n" (List.length !results);

  (* by location *)

  let batches_location =
    List.sort_batch_sort
      (fun r s -> compare r.location s.location)
      !results
  in

  Report.add report "* Sorted by location\n";

  List.iter
    (fun batch_location ->
      Report.add report "** %05d %a\n"
        (List.length batch_location)
        pp_location (List.hd batch_location).location;

      let batches_abstract =
        List.sort_batch_sort
          (fun r s -> compare r.abstract s.abstract)
          batch_location
      in

      List.iter
        (fun batch_abstract ->
          Report.add report "*** %a\n" pp_a_redirection_list (List.hd batch_abstract).abstract;
          output_results report batch_abstract)
        batches_abstract)
    batches_location;


  (* by abstraction *)

  let batches_abstract =
    List.sort_batch_sort
      (fun r s -> compare r.abstract s.abstract)
      !results
  in

  Report.add report "* Sorted by abstraction\n";

  List.iter
    (fun batch_abstract ->
      Report.add report "** %05d %a\n"
        (List.length batch_abstract)
        pp_a_redirection_list (List.hd batch_abstract).abstract;

      let batches_location =
        List.sort_batch_sort
          (fun r s -> compare r.location s.location)
          batch_abstract
      in
      List.iter
        (fun batch_location ->
          Report.add report "*** %a\n" pp_location (List.hd batch_location).location;
          output_results report batch_location)
        batches_location)
    batches_abstract
