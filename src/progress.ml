(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
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
  String.make (max 0 n) ' '

let fixed_size size str =
  let len = String.length str in
  if len <= size then
    str ^ (spaces (size - len))
  else
    (String.sub str 0 (size - 3)) ^ "..."

let bar n =
  if n = 0 then "" else (String.make (n - 1) '=') ^ ">"

let time_to_string t =
  (* takes a time in milliseconds and return a string with less than 3 characters *)
  if t < 60. then
    string_of_int (int_of_float (0.5 +. t)) ^ "s"
  else if t < 3600. then
    string_of_int (int_of_float (0.5 +. t /. 60.)) ^ "m"
  else
    string_of_int (int_of_float (0.5 +. t /. 3600.)) ^ "h"

(* Core *)

type t =
  { name : string ;
    mutable curr : int ; total : int ;
    start : float ; mutable elapsed : float ;
    len_txt : int ; len_int : int ; len_bar : int ; refresh : int }

let create name total =
  let term_cols = term_cols () in
  let len_int = int_length total in
  let len_txt = term_cols / 3 in
  let min_len_right = len_int * 2 + 14 in
  (* 3 for the time, 4 for the percentage, 7 for spaces, / and [] *)
  let len_bar = term_cols - len_txt - min_len_right in
  (* len_bar might very well be negative *)
  let start = Unix.gettimeofday () in
  { name ;
    curr = 0 ; total ;
    start ; elapsed = 0. ;
    len_txt ; len_int ; len_bar ; refresh = max 1 (total/len_bar) }

let percentage ?(scale=100) l =
  ExtPervasives.percentage_i ~scale l.curr l.total

let eprint l =
  let open Format in
  eprintf "\r%s %3s %s%d/%d [%s%s] %3d%%@?"
    (fixed_size l.len_txt l.name)
    (time_to_string l.elapsed)
    (spaces (l.len_int - int_length l.curr)) l.curr l.total
    (bar (percentage ~scale:l.len_bar l)) (spaces (l.len_bar - percentage ~scale:l.len_bar l))
    (percentage l)

let close l =
  eprint l;
  Format.eprintf "@."

let incr l =
  l.curr <- l.curr + 1;
  if l.curr mod l.refresh = 0 || l.curr >= l.total then
    (
      l.elapsed <- Unix.gettimeofday () -. l.start;
      eprint l
    )

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
