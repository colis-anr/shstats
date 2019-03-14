(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

type t

val create : ?title:string -> string -> t
val create_subreport : t -> ?title:string -> string -> t
val commit : t -> string -> unit

val add : t -> ('a, Format.formatter, unit) format -> 'a
val add_org : t -> Org.t -> unit

(** {2 Org shortcuts} *)

val sourcesList : t -> (string * int) list -> Org.t

(** {3 Link utilities} *)

val link_to_subreport : t -> ?text:string -> string -> string

val link_to_source : t -> ?text:string -> string -> string
(** [link_to_source report source] returns a org-mode link pointing to
   [source] and relative to the given [report]. *)

val link : string -> string -> string -> string
(** think twice; you probably don't need this *)

val link_to_file : string -> string -> string
(** think twice; you probably don't need this *)
