(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Libmorbig.CST
open Options
open Commands
open Messages

let unWord' {value=word} =
  Libmorbig.CSTHelpers.unWord word
let unCmdWord' {value=CmdWord_Word word} =
  Libmorbig.CSTHelpers.unWord word.value
                              
module StringSet = Set.Make(String)

let is_special_builtin s =
  List.mem s
           [
             "break"; ":"; "continue"; "."; "eval"; "exec"; "exit";
             "export"; "readonly"; "return"; "set"; "shift"; "times";
             "trap"; "unset"
           ]
           
module Self : Analyzer.S = struct

  let options = []

  let name = "expander"
                                 
  let process_script filename cst =

    let module Effect = struct
        class effect' = object(self)
          inherit [_] Libmorbig.CST.reduce as super
          method zero = StringSet.empty
          method plus s1 s2 = StringSet.union s1 s2
          method! visit_assignment_word functions (name,word) =
            StringSet.singleton (Libmorbig.CSTHelpers.unName name)
          method! visit_simple_command functions cst = match cst with
            | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (pre',cw',suf') ->
               if List.mem (unCmdWord' cw') functions
                  || is_special_builtin (unCmdWord' cw')
               then self#plus
                      (self#visit_simple_prefix' functions pre')
                      (self#visit_cmd_suffix' functions suf')
               else self#visit_cmd_suffix' functions suf'
            | SimpleCommand_CmdPrefix_CmdWord (pre',cw') ->
               if List.mem (unCmdWord' cw') functions
                  || is_special_builtin (unCmdWord' cw')
               then self#visit_cmd_prefix' functions pre'
               else StringSet.empty
            | SimpleCommand_CmdPrefix pre' ->
               self#visit_cmd_prefix' functions pre'
            | SimpleCommand_CmdName_CmdSuffix (nam',suf') ->
               self#visit_cmd_suffix' functions suf'
            | SimpleCommand_CmdName nam' ->
               StringSet.empty
        end                
    end
    in
    let cout = open_out (filename^".vars")
    in begin
        StringSet.iter
          (function s -> Printf.fprintf cout "%s\n" s)
          ((new Effect.effect')#visit_complete_command_list [] cst);
        close_out cout
      end

  let output_report report = ()
    

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
