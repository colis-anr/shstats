
type t

val create : string -> t
val create_subreport : t -> ?title:string -> string -> t

val add : t -> ('a, Format.formatter, unit) format -> 'a

val commit : t -> string -> unit
