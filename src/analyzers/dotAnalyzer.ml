(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Morbig.CST
open Options
open Messages

   (* 
      finds all included files via the "dot" builtin, and classifies
      them.
    *)
   
let unWord' word' = Morbig.CSTHelpers.unWord word'.value
let unName' name' = Morbig.CSTHelpers.unName name'.value
let unCmdName' {value=CmdName_Word(w')} = unWord' w'

let options = []
            
let name = "dot"

let absolute_paths = ref ([]: string list)
let push_absolute pathname =
  absolute_paths := pathname :: (!absolute_paths)

let expandable_paths = ref ([]: (string*string) list)
let push_expandable filename path =
  expandable_paths := (filename,path) :: (!expandable_paths)

let relative_paths = ref ([]: (string*string) list)
let push_relative filename path =
  relative_paths := (filename,path) :: (!relative_paths)

let starts_on_slash s =
  String.length s > 0 && String.get s 0 ='/'

let expandable s =
  String.contains s '$' || String.contains s '`'

let process_script filename cst =
  let extract_cmd_suffix = function
    | CmdSuffix_Word(w') -> Morbig.API.remove_quotes (unWord' w')
    | _ -> failwith ("This cannot happen: " ^ filename)
  in let detect_includes =
       object (self)
         inherit [_] Morbig.CST.iter as super
         method! visit_simple_command () cst = match cst with
           | SimpleCommand_CmdName_CmdSuffix(cmd_name',cmd_suffix') ->
              if unCmdName' cmd_name' = "."
              then
                let path = extract_cmd_suffix cmd_suffix'.value
                in
                if expandable path
                then push_expandable filename path
                else
                if starts_on_slash path
                then push_absolute path
                else push_relative filename path
           | _ -> super#visit_simple_command () cst
       end
  in
  detect_includes#visit_program () cst

let output_report report =
  Report.add report "** Included files with absolute path\n";
  List.iter
    (function path ->
       Report.add report "  %s\n" path)
    (List.sort_uniq Pervasives.compare (!absolute_paths));
  Report.add report "** Included files with expandable path\n";
  List.iter
    (function (scriptname,path) ->
       Report.add report "  %s: %s\n"  scriptname path)
    (!expandable_paths);
  Report.add report "** Included files with relative path\n";
  List.iter
    (function (scriptname,path) ->
       Report.add report "  %s: %s\n" scriptname path)
    (!relative_paths)
  
              
