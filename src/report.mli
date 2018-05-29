
type section = string
type file

val create_section_directory : section -> unit

val open_file : ?name:string -> section -> file
val close_file : file -> unit

val fprintf : file -> ('a, Format.formatter, unit) format -> 'a

val print_headers : file -> string -> unit

val link_to_file : string -> string -> string

val get_section_entry : section -> string
