(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

let term_cols ?(default=80) () =
  let (stdout, stdin, stderr) =
    Unix.(open_process_full "tput cols" (environment ())) in
  let cols =
    try
      int_of_string (input_line stdout)
    with
    | Failure _ | End_of_file -> default
  in
  close_in stdout;
  close_in stderr;
  close_out stdin;
  cols

let int_length n =
  1 + int_of_float (log10 (0.5 +. float_of_int n))

let spaces n =
  String.make n ' '

let bar n =
  if n = 0 then "" else (String.make (n - 1) '=') ^ ">"

(* Core *)

type t =
  { name : string ;
    mutable curr : int ; total : int ;
    len_txt : int ; len_int : int ; len_bar : int ; refresh : int }

let create name total =
  let term_cols = term_cols () in
  let len_int = int_length total in
  let len_txt = term_cols / 3 in
  let min_len_right = len_int * 2 + 10 in
  (* 4 for the percentage, 6 for spaces, / and [] *)
  let len_bar = term_cols - len_txt - min_len_right in
  (* len_bar might very well be negative *)
  { name ; curr = 0 ; total ; len_txt ; len_int ; len_bar ; refresh = total/len_bar }

let percentage ?(scale=100) l =
  ExtPervasives.percentage_i ~scale l.curr l.total

let eprint l =
  let open Format in
  eprintf "\r%s%s %s%d/%d [%s%s] %3d%%@?"
    l.name (spaces (l.len_txt - String.length l.name))
    (spaces (l.len_int - int_length l.curr)) l.curr l.total
    (bar (percentage ~scale:l.len_bar l)) (spaces (l.len_bar - percentage ~scale:l.len_bar l))
    (percentage l)

let close l =
  eprint l;
  Format.eprintf "@."

let incr l =
  l.curr <- l.curr + 1;
  if l.curr mod l.refresh = 0 || l.curr >= l.total then
    eprint l

(* Higher-level *)

module List =
  struct
    let iter name f l =
      let pl = create name (List.length l) in
      List.iter (fun e -> incr pl; f e) l;
      close pl

    let map name f l =
      let pl = create name (List.length l) in
      let r = List.map (fun e -> incr pl; f e) l in
      close pl;
      r
  end
