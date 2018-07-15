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
let unWord word = Libmorbig.CSTHelpers.unWord word
let unName' {value=name} =
  Libmorbig.CSTHelpers.unName name
let unCmdName' {value=CmdName_Word word} =
  Libmorbig.CSTHelpers.unWord word.value
let unName name = Libmorbig.CSTHelpers.unName name
                              
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

module Ef = struct
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
    {vars = StringSet.union t1 t2; fncts = fncts_plus f1 f2}
  and fncts_plus f1 f2 =
    let f12,f1o = List.partition (function f,_ -> List.mem_assoc f f2) f1
    and f21,f2o = List.partition (function f,_ -> List.mem_assoc f f1) f2
    in f1o@f2o@(List.map (function (f,i) -> (f, plus i (List.assoc f f21))) f12)
  let from_var x = {vars=StringSet.singleton x; fncts=[]}
  let from_varlist l =  {vars=StringSet.of_list l; fncts=[]}
  let mergein_onelevel_functions effect fncts =
    List.fold_left (fun m (f,i) -> plus m i) effect fncts
  let from_function name effect = {vars=StringSet.empty; fncts=[(name,effect)]}

  let rec to_string {vars=vars;fncts=fncts} =
    "{" ^ (StringSet.fold (fun s accu-> s^","^accu) vars "}") ^"\n" ^
      (List.fold_right
         (fun (funname,funeffect) accu ->
           "["^funname^"|->"^(to_string  funeffect)^"]\n"^ accu)
         fncts
         ""
      )
end

let debug s = Printf.printf "DEB: %s\n" s
                                           
module Self : Analyzer.S = struct

  let options = []

  let name = "expander"
                                 
  let process_script filename cst =

    let affected_vars cst =
      let effect_collector =
        object(self)
          inherit [_] Libmorbig.CST.mapreduce as super
          method zero = Ef.zero
          method plus x y = Ef.plus x y

          method! visit_assignment_word (fcts:Ef.fenv) (name,word) =
            (
              (name,word)
            ,
              self#plus
              (Ef.from_var (Libmorbig.CSTHelpers.unName name))
              (snd (self#visit_word fcts word))
            )

          method effect_of_simple_command fcts pre_effect cmd suf_effect =
            let presuf_effect = self#plus pre_effect suf_effect
            in
            if is_expandable cmd
            then
              (* [cmd] might be expanded to anything: to a function or
                 special builtin, or somthing else. So we assume the
                 worst: it is some function among the ones in [fncts].
                 This means that we have to compute the union of the 
                 effect of all top-level functions ins [fncts], and also
                 take the effect of the command prefix into account. *)
              Ef.mergein_onelevel_functions presuf_effect fcts
            else if List.mem_assoc cmd fcts
            then
              (* [cmd] is a call to a known function: we take the effect of
                 that function, plus the command prefix and suffix. *)
              self#plus presuf_effect (List.assoc cmd fcts)
            else if is_special_builtin cmd
            then
              (* [cmd] is a special builtin: we have to take the effects of
                 command prefix and suffix into account. *)
              presuf_effect
            else
              (* [cmd] must be the creation of a process, that is the
                 assignement in the prefix is local to the process. *)
              suf_effect
                   
          method! visit_simple_command fcts cst = match cst with
            | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (pre',cw',suf') ->
               let pre_effect = snd (self#visit_cmd_prefix' fcts pre')
               and suf_effect = snd (self#visit_cmd_suffix' fcts suf' )
               and cmd = UnQuote.on_string (unCmdWord' cw')
               in
               (
                 cst
               ,
                 self#effect_of_simple_command fcts pre_effect cmd suf_effect
               )
            | SimpleCommand_CmdPrefix_CmdWord (pre',cw') ->
               let cmd = UnQuote.on_string (unCmdWord' cw')
               and pre_effect = snd (self#visit_cmd_prefix' fcts pre')
               in
               (
                 cst
               ,
                 self#effect_of_simple_command fcts pre_effect cmd self#zero
               )
            | SimpleCommand_CmdPrefix pre' ->
               (
                 SimpleCommand_CmdPrefix pre'
               ,
                 snd (self#visit_cmd_prefix' fcts pre')
               )
            | SimpleCommand_CmdName_CmdSuffix (nam',suf') ->
               let cmd = UnQuote.on_string (unCmdName' nam')
               and suf_effect = snd (self#visit_cmd_suffix' fcts suf')
               in
               (
                 SimpleCommand_CmdName_CmdSuffix (nam',suf')
               ,
                 self#effect_of_simple_command fcts self#zero cmd suf_effect
               )
            | SimpleCommand_CmdName nam' ->
               let cmd = UnQuote.on_string (unCmdName' nam')
               in
               (
                 cst
               ,
                 self#effect_of_simple_command fcts self#zero cmd self#zero
               )

          method! visit_word fcts w =
            (w, Ef.from_varlist (assigned_by_word w))

          method! visit_function_definition fcts cst = match cst with
            | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody
              ({value=Fname_Name fname}, _linebreak',fbody') ->
               (
                 cst
               ,
                 Ef.from_function
                   (unName fname)
                   (snd (self#visit_function_body' fcts fbody'))
               )

          (* TODO : local variables of functions *)

         (* here is where we are deviating from the reduce scheme : in case
            of a complete command list we pass the function part of the
            computed effect of the first complete command on to
            compute the environment for the rest of the complete
            command list.
          *)
          method! visit_complete_command_list fcts = function
            | [] -> ([], Ef.zero)
            | h::r ->
               let hvisited = self#visit_complete_command fcts h
               in let rvisited = self#visit_complete_command_list
                                   (Ef.fncts_plus fcts
                                      (snd hvisited).Ef.fncts) r
                  in
                  (
                    (fst hvisited)::(fst rvisited)
                  ,
                    Ef.plus (snd hvisited) (snd rvisited)
                  )

        end
      in
      (snd (effect_collector#visit_complete_command_list [] cst)).Ef.vars

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
