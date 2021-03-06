(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

module List = struct include List include ExtList end
module String = struct include String include ExtString end

let (||>) f g x = f x |> g

let rec nat_exp k = function
  | -1 -> assert false
  | 0 -> 1
  | 1 -> k
  | n -> let l = nat_exp k (n / 2) in
        l * l * (if n mod 2 = 0 then 1 else k)

let comment f message =
  let y = f () in
  message y;
  y

let string_of_channel cin =
  let b = Buffer.create 16384 in
  let rec aux () =
    Buffer.add_channel b cin 1;
    aux ()
  in
  try
    aux ()
  with _ -> Buffer.contents b

let lines_of_channel cin =
  let lines = ref [] in
  let lineno = ref 0 in
  (try while true do
      incr lineno;
      lines := (!lineno, input_line cin) :: !lines
   done with _ -> ());
  List.rev !lines

let histogram projector l =
  let similar a b = match a, b with
    | Some (_, a), Some (_, b) -> a = b
    | _, _ -> false
  in
  let rec count c prec = function
    | [] -> []
    | x :: xs ->
      let x' = projector x in
      if similar prec (Some (x, x')) then
	count (c + 1) prec xs
      else match prec with
	| None -> count 1 (Some (x, x')) xs
	| Some (y, _) -> (y, c) :: count 1 (Some (x, x')) xs
  in
  let compare_options a b =
    compare (projector a) (projector b)
  in
  count 0 None (List.sort compare_options l)

let option_iter o f = match o with
  | None -> ()
  | Some x -> f x

let string_cut_at k s = String.(
  if length s > k then
    sub s 0 k ^ "..."
  else
    s
)

let repeat n f =
  let rec aux i =
  if i = n then
    []
  else
    f i :: aux (i + 1)
  in
  aux 0

let rec take n l =
  if n = 0 then [], l else
    match l with
      | [] ->
	[], []
      | x :: xs ->
	let ys, xs = take (n - 1) xs in
	x :: ys, xs

let rec take_until pred = function
  | [] -> [], []
  | x :: xs ->
    if pred x then
      let ys, xs = take_until pred xs in
      x :: ys, xs
    else
      [], x :: xs

let hashtbl_to_list h =
  let l = ref [] in
  Hashtbl.iter (fun k v -> l := (k, v) :: !l) h;
  !l

module FirstSuccessMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val fail : 'a t
  val reduce : 'b -> ('b -> 'a -> 'b) -> 'a t list -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( +> ) : 'a t -> 'a t -> 'a t
  val run : 'a t -> 'a option
  val should_succeed :'a t -> 'a
end = struct
  type 'a t = 'a option Lazy.t

  let return x = lazy (Some x)

  let fail = lazy None

  let ( >>= ) x f =
    match Lazy.force x with
      | None -> fail
      | Some x -> f x

  let rec reduce default f = function
    | [] -> return default
    | c :: cs -> c >>= fun a -> reduce (f default a) f cs

  let ( +> ) x y =
    match Lazy.force x with
      | None -> y
      | Some _ -> x

  let run x = Lazy.force x

  exception ShouldHaveSucceeded
  let should_succeed x = match run x with
    | None -> raise ShouldHaveSucceeded
    | Some x -> x
end

(* Pretty-printers helpers *)

(** [pp_to_print pp] is the pretty-printer [pp] that, instead of
    taking any formatter, uses the std_formatter. *)
let pp_to_print pp =
  pp Format.std_formatter

let pp_to_string pp arg =
  let b = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer b in
  pp ppf arg;
  Format.pp_print_flush ppf ();
  Buffer.contents b

let remove_extension f =
  try
    let i = String.rindex f '.' in
    String.sub f 0 i
  with
    Not_found -> f

let extension f =
  try
    let i = String.rindex f '.' in
    String.sub f i (String.length f - i)
  with
    Not_found -> ""

let percentage ?(scale=100.) n d =
  if d = 0 then scale else (scale *. float_of_int n) /. (float_of_int d)

let percentage_i ?(scale=100) n d =
  int_of_float (percentage ~scale:(float_of_int scale) n d)

let extract p1 p2 = (* FIXME: poorly chosen name *)
  let open Lexing in
  let buf = Bytes.make (p2.pos_cnum - p1.pos_bol) ' ' in
  let ic = open_in p1.pos_fname in
  seek_in ic p1.pos_cnum;
  really_input ic buf (p1.pos_cnum - p1.pos_bol) (p2.pos_cnum - p1.pos_cnum);
  close_in ic;
  Bytes.to_string buf

let unwrap = function
  | None -> failwith "unwrap"
  | Some x -> x

let unwrap_or y = function
  | None -> y
  | Some x -> x
