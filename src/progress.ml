(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

type t =
  { name : string ;
    total : int ;
    mutable curr : int }

let create name total =
  { name ; total ; curr = 0 }

let percentage ?(scale=100) l =
  ExtPervasives.percentage_i ~scale l.curr l.total

let eprint l =
  let open Format in
  eprintf "\r%s%s%s[%s%s] %3d%%@?"
    l.name
    (String.make (28 - String.length l.name) ' ')
    (String.make 5 ' ')
    (String.make (percentage ~scale:40 l) '=')
    (String.make (40 - percentage ~scale:40 l) ' ')
    (percentage l)

let close l =
  eprint l;
  Format.eprintf "@."

let update l i =
  l.curr <- i;
  eprint l

let incr l =
  l.curr <- l.curr + 1;
  eprint l
  
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
