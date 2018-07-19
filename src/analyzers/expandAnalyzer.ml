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

module StringTopSet = struct
  (* module implementing finite sets of strings, plus one top element
     representing the set of all strings.
   *)
  type t =
    | All                      (* set of all strings *)
    | Finite of StringSet.t   (* finite set of strings *)

  let empty = Finite StringSet.empty

  let all = All

  let singleton x = Finite (StringSet.singleton x)

  let of_list l = Finite (StringSet.of_list l)
                
  let union vs1 vs2 = match vs1 with
    | All -> All
    | Finite s1 -> match vs2 with
                    | All -> All
                    | Finite s2 -> Finite (StringSet.union s1 s2)

  let mem x = function
    | All -> true
    | Finite s -> StringSet.mem x s

  let rec to_string = function
    | All -> "All"
    | Finite s ->
       "{" ^ (StringSet.fold (fun s accu-> s^","^accu) s "}")
end
                    
module Env = struct
  type t = string StringMap.t (* certain bindings of variables *)
  let zero = StringMap.empty
  let add (name,value) env =
    (* add a binding to an environement, or update *)
    StringMap.add name value (StringMap.remove name env)

  let singleton (name,value) = StringMap.singleton name value

  (* FIXME: word fragments *)
  (* FIXME: more precise matching of paramters: ${.} *)
  let expand_variable env (Word (w, _)) =
    let is_var =
      "\\$\\([a-zA-Z_][a-zA-Z_0-9@*#?$!0-]*\\)"
    in
    let lookup_var s =
      try
	StringMap.find (Str.matched_group 1 s) env
      with Not_found ->
	s
    in
    Word (Str.(global_substitute (regexp is_var) lookup_var w), [])

  let ground_instance env w =
    let w_instance = expand_variable env w
    in if is_expandable (unWord w_instance)
       then None
       else Some w_instance

  let to_string env =
    "{"^(StringMap.fold
           (fun key value accu -> key^"->"^value^","^accu) env "}"
        )
end

module Effect = struct
  (* a value of type [t] is an over-approximation of the effect of executing a
   piece of code in a given environment:
   - [var] is a set of variables. This is an overapproximation of the set
     of variables that a piece of code might asign to.
   *)
  

  type t = {
      vars: StringTopSet.t;
      bind: Env.t;
      isnull: bool
    }
  (* type invariants:
     - domain(bind) \subseteq vars
     - isnull=true implies vars=emptset and bind=emptymap
   *)            
         
  let zero = {
      (* null effect. *)
      isnull=true;
      vars=StringTopSet.empty;
      bind=Env.zero;
    }

  let one = {
      (* maximal effect: any variable may be overwritten *)
      isnull=false;
      vars=StringTopSet.all;
      bind=Env.zero
    }
           
  let rec plus e1 e2 =
    (* the effect of combining [e1] and [e2] any arbitrary times and in 
       any order.
     *)
    if e1.isnull then e2
    else if e2.isnull then e1
    else
      {
        isnull=false;
        bind=Env.zero;
        vars=StringTopSet.union e1.vars e2.vars
      }

  let compose e1 e2 =
    (* the effect of sequentially composing first effect [e1], then
       effect [e2].
     *)
    if e1.isnull then e2
    else if e2.isnull then e1
    else {
        isnull=false;
        vars=StringTopSet.union e1.vars e2.vars;
        bind=
          let e1bind_without_e2touched =
            StringMap.filter
              (fun x _ -> not (StringTopSet.mem x e2.vars))
              e1.bind
          in
          StringMap.union (fun key x y -> assert false)
            e1bind_without_e2touched
            e2.bind
      }

  let from_var_touched x = {
      isnull=false;
      vars=StringTopSet.singleton x;
      bind=Env.zero
    }

  let from_var_bound x v = {
      isnull=false;
      vars=StringTopSet.singleton x;
      bind=Env.singleton (x,v)
    }
      
  let from_varlist l = {
      isnull=false;
      vars=StringTopSet.of_list l;
      bind=Env.zero
    }

  let to_string {isnull=isnull;vars=vars;bind=bind} =
    "["^
      (if isnull then "null" else "nonull")^";"^
        (StringTopSet.to_string vars)^";"^
          (Env.to_string bind)^"]"
end

let debug s1 s2 = Printf.printf "DEB %s: %s\n" s1 s2
                                           
module Self : Analyzer.S = struct

  let options = []

  let name = "expander"

  let process_script filename cst =

    let expand cst =
      let module Fncts = struct
          let set = ref StringSet.empty
          let is s = StringSet.mem s !set
          let add s = set := StringSet.add s !set
        end
      in
      let expand_and_effect =
        object(self)
          inherit [_] Libmorbig.CST.mapreduce as super
          method zero = Effect.zero
          method plus x y = Effect.plus x y

          method! visit_word env w =
            (
              (match Env.ground_instance env w with
               | None -> w
               | Some wi -> wi)
            ,
              Effect.from_varlist (assigned_by_word w)
            )

          method! visit_assignment_word (env: Env.t) (name,word) =
            let word_visited,word_effect = self#visit_word env word in
            let value = unWord word_visited in
            (
              (name,word_visited)
            ,
              Effect.compose
                word_effect
                (if is_expandable value
                 then
                   Effect.from_var_touched (unName name)
                 else
                   Effect.from_var_bound (unName name) value
                )
            )

          method effect_of_simple_command env pre_effect cmd suf_effect =
            let presuf_effect = Effect.compose pre_effect suf_effect
            in
            if is_expandable cmd || Fncts.is cmd
            then 
              (* [cmd] might be (expanded to) the name of a visible
                 function. So we assume the worst. *)
              Effect.one
            else if is_special_builtin cmd
            then
              (* [cmd] is a special builtin: we have to take the effects of
                 command prefix and suffix into account. *)
              presuf_effect
            else
              (* [cmd] must be the creation of a process, that is the
                 assignement in the prefix is local to the process. *)
              suf_effect (* FIXME *)

          method! visit_simple_command env cst = match cst with
            (* FIXME: effects of expansion the command word or name *)
            | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (pre',cw',suf') ->
               let (prev,pree) = self#visit_cmd_prefix' env pre'
               and (sufv,sufe) = self#visit_cmd_suffix' env suf'
               and cmd = UnQuote.on_string (unCmdWord' cw')
               in
               (
                 SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (prev,cw',sufv)
               ,
                 self#effect_of_simple_command env pree cmd sufe
               )
            | SimpleCommand_CmdPrefix_CmdWord (pre',cw') ->
               let cmd = UnQuote.on_string (unCmdWord' cw')
               and (prev,pree) = self#visit_cmd_prefix' env pre'
               in
               (
                 SimpleCommand_CmdPrefix_CmdWord (prev,cw')
               ,
                 self#effect_of_simple_command env pree cmd self#zero
               )
            | SimpleCommand_CmdPrefix pre' ->
               let (prev,pree) = self#visit_cmd_prefix' env pre'
               in
               (
                 SimpleCommand_CmdPrefix prev
               ,
                 pree
               )
            | SimpleCommand_CmdName_CmdSuffix (nam',suf') ->
               let cmd = UnQuote.on_string (unCmdName' nam')
               and (sufv,sufe) = self#visit_cmd_suffix' env suf'
               in
               (
                 SimpleCommand_CmdName_CmdSuffix (nam',sufv)
               ,
                 self#effect_of_simple_command env self#zero cmd sufe
               )
            | SimpleCommand_CmdName nam' ->
               let cmd = UnQuote.on_string (unCmdName' nam')
               in
               (
                 cst
               ,
                 self#effect_of_simple_command env self#zero cmd self#zero
               )

          method! visit_function_definition fcts cst = match cst with
            | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody
              ({value=Fname_Name fname}, _linebreak',fbody') ->
               Fncts.add (unName fname);
               (cst,Effect.zero)

          method! visit_complete_command_list env = function
            | [] -> ([], Effect.zero)
            | h::r ->
               let (ht,he) =
                 self#visit_complete_command env h
               in
               let (rt,re) =
                 self#visit_complete_command_list he.Effect.bind r
               in
               (
                 ht::rt
               ,
                 Effect.compose he re
               )

           (* FIXME: more constructions implementing sequential composition *)
        end
      in
      fst (expand_and_effect#visit_complete_command_list Env.zero cst)

    in
    let cout = open_out (filename^".expanded")
    in begin
        Libmorbig.JsonHelpers.save_as_json true cout (expand cst);
        close_out cout
      end

  let output_report report = ()
    

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
