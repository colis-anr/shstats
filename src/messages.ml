(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

let message what consequence ?filename msg =
  Format.eprintf "%s[%s] %s\n"
    (match filename with None -> "" | Some f -> f ^":0:0:\n")
    what
    msg;
  consequence ()

let warning = message "WARNING" ignore
let error = message "ERROR" (fun () -> exit 1)
