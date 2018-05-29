
type t

val create : string -> t
val create_subreport : t -> ?title:string -> string -> t

val add : t -> ('a, Format.formatter, unit) format -> 'a
val link_to_subreport : t -> ?text:string -> string -> string
  
val commit : t -> string -> unit
