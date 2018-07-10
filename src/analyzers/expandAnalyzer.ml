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

(* check whether [s] is possibly expandable by parameter or sub-shell
expansion *)
let is_expandable s = String.contains s '$' || String.contains s '`'

module Mon = struct
(* a value of type [t] is an over-approximation of the effect of executing a
   piece of code in a given environment:
   - [vars] is a set of variables. This is an overapproximation of the set
     of variables that a piece of code might asign to.
   - [fncts] is a mapping. Its domain is the set of functions that might be
     defined by the execution of the code. The mapping assigns to each of
     these names of functions the effect of involving it.
 *)
  type t = {
      vars: StringSet.t;
      fncts: fenv
    }
   and fenv = (string*t) list
  let zero = {vars=StringSet.empty; fncts= []}
  (* the [plus] of two values of type [t] is defined as the respective
      unions of their [vars] and their [fncts]. If a function name
      is defined in both then we recursively compute the [plus] of the
      associated values. *)  
  let rec plus {vars=t1;fncts=f1} {vars=t2;fncts=f2} =
    {vars = StringSet.union t1 t2;
     fncts = 
       let f12,f1o = List.partition (function f,_ -> List.mem_assoc f f2) f1
       and f21,f2o = List.partition (function f,_ -> List.mem_assoc f f1) f2
       in f1o@f2o@
            (List.map (function (f,i) -> (f, plus i (List.assoc f f21))) f12)
    }
  let from_var x = {vars=StringSet.singleton x; fncts=[]}
  let from_varlist l =  {vars=StringSet.of_list l; fncts=[]}
  let merge_functions effect fncts =
    List.fold_left (fun m (f,i) -> plus m i) effect fncts
      
end
           
module Self : Analyzer.S = struct

  let options = []

  let name = "expander"
                                 
  let process_script filename cst =

    let affected_vars cst =
      let effect_collector =
        object(self)
          inherit [_] Libmorbig.CST.reduce as super
          method zero = Mon.zero
          method plus x y = Mon.plus x y
          method! visit_assignment_word (fcts:Mon.fenv) (name,word) =
            self#plus
              (Mon.from_var (Libmorbig.CSTHelpers.unName name))
              (self#visit_word fcts word)
          method! visit_simple_command fcts = function
            | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (pre',cw',suf') ->
               let cmd = UnQuote.on_string (unCmdWord' cw')
               and prefix_effect = self#visit_cmd_prefix' fcts pre'
               and suffix_effect = self#visit_cmd_suffix' fcts suf' 
               in let presuf_effect = self#plus prefix_effect suffix_effect 
               in
               if is_expandable cmd
               then
                 (* [cmd] might be expanded to anything: to a function or
                     special builtin, so we have to take the effect of the
                     prefix into account, and it might even be one of the
                     currently visible functions. *)
                 Mon.merge_functions presuf_effect fcts
               else if List.mem_assoc cmd fcts
               then
                 (* call to a known function *)
                 self#plus presuf_effect (List.assoc cmd fcts)
               else if is_special_builtin cmd
               then presuf_effect
               else (* creation of a process *)
                 suffix_effect
            | SimpleCommand_CmdPrefix_CmdWord (pre',cw') ->
               let cmd = UnQuote.on_string (unCmdWord' cw')
               and prefix_effect = self#visit_cmd_prefix' fcts pre'
               in
               if is_expandable cmd
               then
                 (* [cmd] might be expanded to anything: to a function or
                     special buildin, so we have the effect of the prefix
                     into account, and it might even be one of the currently
                     visible functions. *)
                 Mon.merge_functions prefix_effect fcts
               else if List.mem_assoc cmd fcts
               then
                 (* call to a known function *)
                 self#plus prefix_effect (List.assoc cmd fcts)
               else if is_special_builtin cmd
               then prefix_effect
               else (* creation of a process *)
                 self#zero
            | SimpleCommand_CmdPrefix pre' ->
               self#visit_cmd_prefix' fcts pre'
            | SimpleCommand_CmdName_CmdSuffix (nam',suf') ->
               self#visit_cmd_suffix' fcts suf'
            | SimpleCommand_CmdName nam' ->
               self#zero
          method! visit_word fcts w = Mon.from_varlist (assigned_by_word w)
           
        end
      in
      (effect_collector#visit_complete_command_list [] cst).Mon.vars

    in
    let cout = open_out (filename^".vars")
    in begin
        StringSet.iter
          (function s1 -> Printf.fprintf cout "%s\n" s1)
          (affected_vars cst);
        close_out cout
      end

  let output_report report = ()
    

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
