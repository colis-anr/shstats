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

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

module Effect = struct
  (* a value of type [t] is an over-approximation of the effect of executing a
   piece of code in a given environment:
   - [var] is a set of variables. This is an overapproximation of the set
     of variables that a piece of code might asign to.
   *)
  type t =
    | Any                      (* any variable may be changed *)
    | Touched of StringSet.t   (* upper bound on set of changed variables *)

  let mem x = function
    | Any -> true
    | Touched s -> StringSet.mem x s
                 
  let zero = Touched StringSet.empty

  let rec plus e1 e2 = match e1 with
    | Any -> Any
    | Touched s1 -> match e2 with
                 | Any -> Any
                 | Touched s2 -> Touched (StringSet.union s1 s2)
     
  let from_var x = Touched (StringSet.singleton x)

  let from_varlist l =  Touched (StringSet.of_list l)

  let rec to_string e = function
    | Any -> "ANY"
    | Touched s ->
       "{" ^ (StringSet.fold (fun s accu-> s^","^accu) s "}")
end

module Env = struct
  type t = string StringMap.t (* certain bindings of variables *)
  let zero = StringMap.empty
  let retract env ef =
    (* [env] without the bindings for elements of [ef] *)
    StringMap.filter (fun x _ -> not (Effect.mem x ef)) env
  let add (name,value) env =
    (* add a binding to an environement, or update *)
    StringMap.add name value (StringMap.remove name env)
end

let extract_simple_assignment _ = None (* FIXME *)

let debug s = Printf.printf "DEB: %s\n" s
                                           
module Self : Analyzer.S = struct

  let options = []

  let name = "expander"

  let process_script filename cst =

    let affected_vars cst =
      let module Fncts = struct
          let set = ref StringSet.empty
          let is s = StringSet.mem s !set
          let add s = set := StringSet.add s !set
        end
      in
      let effect_collector =
        object(self)
          inherit [_] Libmorbig.CST.mapreduce as super
          method zero = Effect.zero
          method plus x y = Effect.plus x y

          method! visit_assignment_word (env: Env.t) (name,word) =
            (
              (name,word)
            ,
              self#plus
              (Effect.from_var (Libmorbig.CSTHelpers.unName name))
              (snd (self#visit_word env word))
            )

          method effect_of_simple_command env pre_effect cmd suf_effect =
            let presuf_effect = self#plus pre_effect suf_effect
            in
            if is_expandable cmd
            then 
              (* [cmd] might be expanded to anything: to a function or
                 special builtin, or somthing else. So we assume the
                 worst: it is some function. *)
              Effect.Any
            else if Fncts.is cmd
            then
              (* [cmd] is a call to a known function whcih could
                 have any effect. *)
              Effect.Any
            else if is_special_builtin cmd
            then
              (* [cmd] is a special builtin: we have to take the effects of
                 command prefix and suffix into account. *)
              presuf_effect
            else
              (* [cmd] must be the creation of a process, that is the
                 assignement in the prefix is local to the process. *)
              suf_effect
                   
          method! visit_simple_command env cst = match cst with
            | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (pre',cw',suf') ->
               let pre_effect = snd (self#visit_cmd_prefix' env pre')
               and suf_effect = snd (self#visit_cmd_suffix' env suf' )
               and cmd = UnQuote.on_string (unCmdWord' cw')
               in
               (
                 cst
               ,
                 self#effect_of_simple_command env pre_effect cmd suf_effect
               )
            | SimpleCommand_CmdPrefix_CmdWord (pre',cw') ->
               let cmd = UnQuote.on_string (unCmdWord' cw')
               and pre_effect = snd (self#visit_cmd_prefix' env pre')
               in
               (
                 cst
               ,
                 self#effect_of_simple_command env pre_effect cmd self#zero
               )
            | SimpleCommand_CmdPrefix pre' ->
               (
                 SimpleCommand_CmdPrefix pre'
               ,
                 snd (self#visit_cmd_prefix' env pre')
               )
            | SimpleCommand_CmdName_CmdSuffix (nam',suf') ->
               let cmd = UnQuote.on_string (unCmdName' nam')
               and suf_effect = snd (self#visit_cmd_suffix' env suf')
               in
               (
                 SimpleCommand_CmdName_CmdSuffix (nam',suf')
               ,
                 self#effect_of_simple_command env self#zero cmd suf_effect
               )
            | SimpleCommand_CmdName nam' ->
               let cmd = UnQuote.on_string (unCmdName' nam')
               in
               (
                 cst
               ,
                 self#effect_of_simple_command env self#zero cmd self#zero
               )

          method! visit_word fcts w =
            (w, Effect.from_varlist (assigned_by_word w))

          method! visit_function_definition fcts cst = match cst with
            | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody
              ({value=Fname_Name fname}, _linebreak',fbody') ->
               Fncts.add (unName fname);
               (cst,Effect.zero)

          method! visit_complete_command_list env = function
            | [] -> ([], Effect.zero)
            | h::r ->
               let (ht,he) = self#visit_complete_command env h
               in
               let env' = Env.retract env he
               in
               let new_env = match extract_simple_assignment h with
                 | None -> env'
                 | Some (name,value) -> Env.add (name,value ) env' 
               in
               let (rt,re) = self#visit_complete_command_list new_env r
               in
               (
                 ht::rt
               ,
                 Effect.plus re he
               )
                 
        end
      in
      snd (effect_collector#visit_complete_command_list Env.zero cst)

    in
    let cout = open_out (filename^".var")
    in begin
        Printf.fprintf cout "%s\n" (Effect.to_string (affected_vars cst));
        close_out cout
      end

  let output_report report = ()
    

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
