
open Unix

let test_e path =
  match stat path with
  | _ -> true
  | exception (Unix_error _) -> false

let test_d path =
  match stat path with
  | {st_kind=S_DIR} -> true
  | _ -> false
  | exception (Unix_error _) -> false

let p = true

let rec mkdir ?(umask=0o022) ?(p=false) path =
  if p then
    (
      match String.rindex_from path (String.length path - 2) '/' with
      | i ->
         mkdir ~p (String.sub path 0 i);
         if not (test_d path) then
           mkdir path
      | exception Not_found ->
         if not (test_d path) then
           mkdir path
    )
  else
    try
      Unix.mkdir path (0o777 lxor umask)
    with
      Unix_error _ -> failwith "mkdir"
