(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

include List

let batch equal list =
  (* Invariants for aux (among others):
     - curr \in curr_batch
     - curr_batch <> [] *)
  let rec aux curr curr_batch other_batches = function
    | [] ->
       List.rev
         ((List.rev curr_batch)
          :: other_batches)
    | h :: q when equal h curr ->
       aux curr (h :: curr_batch) other_batches q
    | h :: q ->
       aux h [h] ((List.rev curr_batch) :: other_batches) q
  in
  match list with
  | [] -> []
  | h :: q -> aux h [h] [] q

let sort_batch compare list =
  list
  |> sort compare
  |> batch (fun a b -> compare a b = 0)

let split_delim is_delim l =
  let rec aux acc1 acc2 = function
    | [] ->
       List.(rev (rev acc2 :: acc1))
    | x :: xs ->
       if is_delim x then
	 aux (List.rev acc2 :: acc1) [] xs
       else
	 aux acc1 (x :: acc2) xs
  in
  List.filter (fun x -> x <> []) (aux [] [] l)

let uniq l =
  let rec remove_dup = function
    | [] -> []
    | [x] -> [x]
    | x :: y :: ys when x = y -> remove_dup (y :: ys)
    | x :: ys -> x :: remove_dup ys
  in
  remove_dup (List.sort compare l)
