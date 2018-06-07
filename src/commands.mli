(**************************************************************************)
(*  Copyright (C) 2017,2018 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

(*

       A priori knowledge about commands and their options.

*)

(* FIXME: shouldn't these be simply string ? *)
type command_name = Libmorbig.CST.word
type option_name = Libmorbig.CST.word

(** type for representing the possible arguments to an option of a given
    command. *)
type number_of_arguments =
  | NoArgument        (* option has no argument *)
  | Exactly of int    (* option has exactly that many arguments *)
  | MoreThanOne       (* option may have  any positive number of arguments *)
  | UntilSemicolon    (* all command arguments until the next semicolon
                         are arguments of that option *)
  | UseArgumentAsEOF  (* the following command argument is a delimiter for
                         the arguments to that option *)
  | Unhandled         (* not implemented, will raise an error when such an
                         option is encountered *)

(** type representing a specific option *)
type option_specification = {
    name : option_name list;       (* icnlcudes all synonymes *)
    number_of_arguments : number_of_arguments
  }

(** type representing a specific command *)
type command_options = {
    command : command_name;
    accumulated_short_options : bool;
    start_with_options : bool;
    double_dash_for_raw : bool;
    options : option_specification list;
    specified : bool;
  }

(** load command specification from a file *)
val load_commands_specification : string -> unit

(** [lookup_command cmd] returns the specification of command [cmd]. *)
val lookup_command : command_name -> command_options

(** [lookup_options cmd opt] returns the specification of option [opt]
    to command [cmd]. *)
val lookup_options : command_name -> option_name -> option_specification

(** [lookup_option_number_of_arguments cmd opt] returns the argument
    specification of option [opt] to command [cmd]. *)
val lookup_option_number_of_arguments :
  command_name -> option_name -> number_of_arguments

(** [is_option_of_command ?who cmd opt] checks whether [opt] is a known
    option of command [cmd]. [who], when set, is used in error message. *)
val is_option_of_command :
  ?who_is_asking:string -> command_name -> option_name -> bool

(** [canonical_option_name cmd opt] returns the canonical option name of
    option [opt] to command [cmd]. *)
val canonical_option_name : command_name -> option_name -> option_name
