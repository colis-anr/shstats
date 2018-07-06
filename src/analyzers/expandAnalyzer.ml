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
let unWord word = Libmorbig.CSTHelpers.unWord word
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

(* return, for a word [w], a list of variable names to which expansion
of the word [w] possibly makes an assignment (see Section 2.6.2 of
the POSIX standard. *)

let assigned_by_word w =
  let r = Str.regexp "\\${\\([^}:=]+\\):?=[^}]*}"
  and s = unWord w
  in let rec collect_from i =
       try
         let _ = Str.search_forward r s i
         in let first = Str.matched_group 1 s 
         in first::(collect_from (Str.match_end ()))
       with Not_found -> []
     in collect_from 0
;;

module E = struct
  type t = {
      env: (string*string) list;
      touched: StringSet.t;
      fncts: (string*t) list
    }
(* a value of type [t] is an approximation of the effect of executing a
   piece of code in a given environment:
   - [env] is a variable environment that maps names of variables to ground
     (i.e., invariant under expansion) strings. We are sure that after execution
     of the code we obtain at least these bindings. 
   - [touched] is a set of variables. This is an overapproximation of the set
     set of varaibles thate a piece of code might asign to.
   - [fncts] is a mapping. Its domain is the set of functions that might be
     defined by the execution of the code. The functiosn asigns to each of
     these names of functions the effect of involing it.
   Type invariants:
     - [domain(env) \subseteq touched]
 *)
end
           
module Self : Analyzer.S = struct

  let options = []

  let name = "expander"
                                 
  let process_script filename cst =

    let affected_vars cst =
      let effect_collector =
        object(self)
          inherit [_] Libmorbig.CST.reduce as super
          (* the monoid of the reduce part is the monoid of pairs
             of strings of sets. If [cst] reduces to the pair [(v,f)]
             then [v] is the set of variables that might have been
             assigned to by [cst], and [f] is the set of functions
             that might have been defined by the execution of [cst]. *)
          method zero = (StringSet.empty,StringSet.empty)
          method plus (v1,f1) (v2,f2) =
            (StringSet.union v1 v2, StringSet.union f1 f2)
          method! visit_assignment_word (fcts:StringSet.t) (name,word) =
            self#plus
              (StringSet.singleton (Libmorbig.CSTHelpers.unName name),
               StringSet.empty)
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
               else self#zero
            | SimpleCommand_CmdPrefix pre' ->
               self#visit_cmd_prefix' fcts pre'
            | SimpleCommand_CmdName_CmdSuffix (nam',suf') ->
               self#visit_cmd_suffix' fcts suf'
            | SimpleCommand_CmdName nam' ->
               self#zero
          method! visit_word fcts w =
            (StringSet.of_list (assigned_by_word w),
             StringSet.empty)
        end
      in
      effect_collector#visit_complete_command_list
        StringSet.empty
        cst

    in
    let cout = open_out (filename^".vars")
    in begin
        StringSet.iter
          (function s1 -> Printf.fprintf cout "%s\n" s1)
          (fst (affected_vars cst));
        close_out cout
      end

  let output_report report = ()
    

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
