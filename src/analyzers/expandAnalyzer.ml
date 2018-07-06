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

    let affected_vars cst =
      let effect_collector =
        object(self)
          inherit [_] Libmorbig.CST.reduce as super
          method zero = StringSet.empty
          method plus s1 s2 = StringSet.union s1 s2
          method! visit_assignment_word (fcts:StringSet.t) (name,word) =
            self#plus
              (StringSet.singleton (Libmorbig.CSTHelpers.unName name))
              (self#visit_word fcts word)
          method! visit_simple_command fcts cst = match cst with
            | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (pre',cw',suf') ->
               if StringSet.mem (unCmdWord' cw') fcts
                  || is_special_builtin (unCmdWord' cw')
               then self#plus
                      (self#visit_cmd_suffix' fcts suf')
                      (self#visit_cmd_prefix' fcts pre')
               else self#visit_cmd_suffix' fcts suf'
            | SimpleCommand_CmdPrefix_CmdWord (pre',cw') ->
               if StringSet.mem (unCmdWord' cw') fcts
                  || is_special_builtin (unCmdWord' cw')
               then self#visit_cmd_prefix' fcts pre'
               else StringSet.empty
            | SimpleCommand_CmdPrefix pre' ->
               self#visit_cmd_prefix' fcts pre'
            | SimpleCommand_CmdName_CmdSuffix (nam',suf') ->
               self#visit_cmd_suffix' fcts suf'
            | SimpleCommand_CmdName nam' ->
               StringSet.empty
        end
      in
      effect_collector#visit_complete_command_list
        StringSet.empty
        cst

    in
    let cout = open_out (filename^".vars")
    in begin
        StringSet.iter
          (function s -> Printf.fprintf cout "%s\n" s)
          (affected_vars cst);
        close_out cout
      end

  let output_report report = ()
    

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
