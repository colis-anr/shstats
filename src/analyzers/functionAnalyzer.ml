(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open Morbig.CST
open Morbig.CSTHelpers

(* module of graphs with cycle check *)
module Graph (Key : Map.OrderedType) =
  struct
    module KMap = Map.Make(Key)

    type node = {
        key : Key.t ;
        neighbours : t
      }

     and t = node KMap.t

    let empty = KMap.empty

    let empty_node key = { key ; neighbours = empty }

    exception VerticeExists
    exception VerticeDoesntExist

    let add_vertice graph key =
      if KMap.mem key graph then
        raise VerticeExists
      else
        KMap.add key (empty_node key) graph

    let add_edge graph key_source key_target =
      try
        let source = KMap.find key_source graph in
        let target = KMap.find key_target graph in
        KMap.add
          key_source
          { source with neighbours =
                          KMap.add key_target target source.neighbours }
          graph
      with
        Not_found -> raise VerticeDoesntExist

    module KSet = Set.Make(Key)

    exception Cycle

    let check_acyclic graph =
      let rec visit visited trace node =
        if KSet.mem node.key trace then
          raise Cycle
        else if KSet.mem node.key visited then
          visited
        else
          let trace = KSet.add node.key trace in
          KSet.add
            node.key
            (KMap.fold
               (fun _ node visited -> visit visited trace node)
               node.neighbours
               visited)
      in
      let _ = KMap.fold
                (fun _ node visited -> visit visited KSet.empty node)
                graph
                KSet.empty
      in
      ()

    let print graph ppf key_to_string =
      Format.fprintf ppf "digraph {@\n@[<h 2>  ";
      KMap.iter
        (fun _ source ->
          Format.fprintf ppf "\"%s\";@\n" (key_to_string source.key);
          KMap.iter
            (fun _ target ->
              Format.fprintf ppf "\"%s\" -> \"%s\";@\n"
                             (key_to_string source.key)
                             (key_to_string target.key))
            source.neighbours)
        graph;
      Format.fprintf ppf "@]}@\n"
  end

module SGraph = Graph (String)

let name = "functions"
let options = []

let functions_counter = new Counters.occCounter "functions"

let buf_cycles = Buffer.create 8
let ppf_cycles = Format.formatter_of_buffer buf_cycles

let buf_duplicates = Buffer.create 8
let ppf_duplicates = Format.formatter_of_buffer buf_duplicates

let process_script filename csts =
  let counter = object (self)
                  inherit [_] Morbig.CST.iter as super
                        
                  val functions_stack =
                    let s = Stack.create () in
                    Stack.push "*toplevel*" s;
                    s

                  val functions_tbl =
                    Hashtbl.create 8
                    
                  method begin_function name = Stack.push name functions_stack
                  method end_function _ = let _ = Stack.pop functions_stack in ()
                                                                             
                  method in_function () = not (Stack.is_empty functions_stack)
                  method get_function () = Stack.top functions_stack

                  val dep_graph = ref (SGraph.add_vertice SGraph.empty "*toplevel*")

                  method get_dep_graph () =
                    !dep_graph

                  method add_edge_to s =
                    try
                      dep_graph := SGraph.add_edge !dep_graph (self#get_function ()) s
                    with
                      SGraph.VerticeDoesntExist -> ()

                  method! visit_function_definition venv = function
                    | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody
                      (fn, _, fb) (* as fd *)->
                       let Fname_Name (Name fn) = fn.value in
                       functions_counter#add filename "";
                       (*FIXME: (pp_shell_repr pp_function_definition fd); *)
                       self#begin_function fn;
                       (
                         try
                           dep_graph := SGraph.add_vertice !dep_graph fn;
                           Hashtbl.add functions_tbl fn fb
                         with
                           SGraph.VerticeExists ->
                            Format.fprintf
                              ppf_duplicates
                              "- %s\n  - '%s' is defined more than once@." filename fn;
                            if fb = Hashtbl.find functions_tbl fn then
                              Format.fprintf
                                ppf_duplicates
                                "    - and with the same body@."
                       );
                       self#visit_function_body' venv fb;
                       self#end_function fn

                  method! visit_cmd_word venv = function
                    | CmdWord_Word w ->
                       self#add_edge_to (unWord w.value)

                  method! visit_cmd_name venv = function
                    | CmdName_Word w ->
                       self#add_edge_to (unWord w.value)
                end in
  counter#visit_program () csts;
  try
    SGraph.check_acyclic (counter#get_dep_graph ())
  with
    SGraph.Cycle ->
    Format.fprintf
      ppf_cycles
      "- %s@.@[<h 2>  " filename;
    Format.fprintf
      ppf_cycles
      "#+BEGIN_SRC dot :file %s.png :exports results@.@[<h 2>  " filename;
    SGraph.print (counter#get_dep_graph()) ppf_cycles (fun x -> x);
    Format.fprintf ppf_cycles "@]#+END_SRC@.@]"

let output_report report =
  Report.add
    report
    "* Functions\n- %d functions declarations in %d files\n** Occurrences@."
    (functions_counter#n_occurrences()) (functions_counter#n_files());
  functions_counter#output_occurrences report;

  let duplicates = Buffer.contents buf_duplicates in
  if duplicates = "" then
    Report.add report "** No duplicates\n"
  else
    Report.add report "** Duplicates\n%s" duplicates;

  let cycles = Buffer.contents buf_cycles in
  if cycles = "" then
    Report.add report "** No cycles\n"
  else
    Report.add report "** Cycles\n%s" cycles
