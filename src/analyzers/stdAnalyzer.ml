(* Not an analyzer in itself, but just kind of a "standard library"
for the analyzers. *)

class ['a, 'b] counter (name: string) = object (self)
  val values : ('a, 'b Queue.t) Hashtbl.t = Hashtbl.create 8

  method n_keys () =
    Hashtbl.length values

  method n_elements () =
    Hashtbl.fold (fun _ elmts n -> n + Queue.length elmts) values 0

  method add key elmt =
    try
      let elmts = Hashtbl.find values key in
      Queue.push elmt elmts
    with
      Not_found ->
      let elmts = Queue.create () in
      Queue.push elmt elmts;
      Hashtbl.add values key elmts

  method iter f =
    Hashtbl.iter
      (fun key -> Queue.iter (f key))
      values
end

let split_on_char sep s =
  (* In the stdlib since 4.04.0 *)
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep then
      (
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      )
  done;
  String.sub s 0 !j :: !r

let indent_string s n =
  let sep = "\n" ^ (String.make n ' ') in
  String.concat sep (split_on_char '\n' s)

class occCounter (name: string) = object (self)
  inherit [string, string] counter name as super

  method output_occurrences report =
    super#iter (fun f r -> Report.add report "- [[file:%s]]\n  %s\n" f (indent_string r 2))

  method n_files = super#n_keys
  method n_occurrences = super#n_elements
end

let pp_shell_repr pp s =
  "#+BEGIN_SRC sh\n" ^ (indent_string (ExtPervasives.pp_to_string pp s) 2) ^ "\n#+END_SRC\n"
