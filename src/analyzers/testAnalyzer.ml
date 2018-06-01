open ExtPervasives
open Libmorbig.CST
open Options
open Commands
open Messages

let unCmdName' {value=CmdName_Word word} =
  Libmorbig.CSTHelpers.unWord word.value
let unCmdWord' {value=CmdWord_Word word} =
  Libmorbig.CSTHelpers.unWord word.value
let unWord' {value=word} =
  Libmorbig.CSTHelpers.unWord word
                                                                       
let extract_command = function
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (_,cmdword',_)
  | SimpleCommand_CmdPrefix_CmdWord (_,cmdword')
    -> Some (unCmdWord' cmdword')
  | SimpleCommand_CmdName_CmdSuffix (cmdname',_)
  | SimpleCommand_CmdName cmdname'
    -> Some (unCmdName' cmdname')
  | SimpleCommand_CmdPrefix _prefix
    -> None

let rec extract_arguments_from_suffix = function
  | CmdSuffix_IoRedirect io_redirect
    -> []
  | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix',_io_redirect')
    -> extract_arguments_from_suffix cmd_suffix'.value
  | CmdSuffix_Word word'
    -> [ unWord' word' ]
  | CmdSuffix_CmdSuffix_Word (cmd_suffix',word')
    -> (unWord' word') :: (extract_arguments_from_suffix cmd_suffix'.value) 

let extract_arguments = function
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (_,_,suffix')
  | SimpleCommand_CmdName_CmdSuffix (_,suffix')
    -> List.rev (extract_arguments_from_suffix suffix'.value)
  | SimpleCommand_CmdPrefix_CmdWord _
  | SimpleCommand_CmdPrefix _
  | SimpleCommand_CmdName _
    -> []

(* tokens for arguments of the test command *)
type token =
  | UnOp of string    (* unary operators -e, -f, etc. *) 
  | BinOp of string   (* binary operators -eq, =, etc. *)
  | AndOp             (* -a *)
  | OrOp              (* -o *)
  | Not               (* ! *)
  | ParL              (* ( *)
  | ParR              (* ) *)
  | BracketR          (* ] *)
  | String of string  (* all the rest *)
  | EOF

let to_token s = match s with
  (* file existence and type *)
  | "-e" | "-d" | "-f" | "-b" | "-c" | "-h" | "-L" | "-p" | "-S" -> UnOp s
  (* file attributes *)
  | "-g" | "-u" | "-s" | "-r" | "-w" | "-x" -> UnOp s
  (* GNU extension on files *)
  | "-G" | "-O" | "-k" -> UnOp s
  (* GNU extension on files *)
  | "-nt" | "-ot" | "-ef" -> BinOp s
  (* unary operators on strings *)
  | "-n" | "-z" -> UnOp s
  (* binary operators on strings *)
  | "=" | "!=" -> BinOp s
  (* binary operators on integers *)
  | "-eq" | "-ne" | "-gt" | "-ge" | "-lt" | "-le" -> BinOp s
  (* unary operator on file descriptor *)
  | "-t" -> UnOp s
  | "-a" -> AndOp
  | "-o" -> OrOp
  | "("  -> ParL
  | ")"  -> ParR
  | "]"  -> BracketR
  | "!"  -> Not
  | _    -> String s

(* abstract syntax of test expressions *)
type test_expression =
  | Andtest of test_expression * test_expression
  | Ortest of test_expression * test_expression
  | Onetest of test_literal
and test_literal =
  | Postest of test_atom
  | Negtest of test_atom
and test_atom =
  | Bintest of string * string * string  (* op arg_left arg_right *)
  | Unitest of string * string           (* op arg *)
  | Contest of string                    (* arg *)
  | Partest of test_expression           (* ( expression ) *)

exception Parse_error
let parse is_bracket tokens =
  let tokenbuf = ref tokens in
  let lookup () = match !tokenbuf with
    | h::r -> h
    | [] -> EOF
  and pop () = match !tokenbuf with
    | h::r -> tokenbuf := r
    | [] -> assert false
  in
  let rec parse_S () =
    let exp = parse_disj () in
    if is_bracket then
      if lookup () = BracketR
      then pop ()
      else raise Parse_error;
    if lookup () = EOF
    then exp
    else raise Parse_error
  and parse_disj () =
    let head = parse_conj () in
    match parse_disj' () with
    | None -> head
    | Some rest -> Ortest (head,rest)
  and parse_disj' () =
    match lookup () with
    | EOF | BracketR | ParR -> None
    | OrOp -> pop (); Some (parse_disj ())
    | _ -> raise Parse_error
  and parse_conj () =
    let head = Onetest (parse_literal ()) in
    match parse_conj' () with
    | None -> head
    | Some rest ->  Andtest (head, rest)
  and parse_conj' () =
    match lookup () with
    | OrOp | EOF | BracketR | ParR -> None
    | AndOp -> pop (); Some (parse_conj ())
    | _ -> raise Parse_error
  and parse_literal () =
    match lookup () with
    | Not -> pop (); Negtest (parse_atom ())
    | UnOp _ | ParL | String _ -> Postest (parse_atom ())
    | _ -> raise Parse_error
  and parse_atom () =
    match lookup () with
    | UnOp op -> pop ();
                 begin
                 match lookup () with
                 | String s -> pop (); Unitest (op,s)
                 | _ -> raise Parse_error
                 end
    | ParL -> pop ();
              begin
              let exp = parse_disj () in
              match lookup () with
              | ParR -> pop (); Partest exp
              | _ -> raise Parse_error
              end
    | String s -> pop ();
                  begin
                  match parse_atom' () with
                  | None -> Contest s
                  | Some (binop,rightarg) -> Bintest (binop,s,rightarg)
                  end
    | _ -> raise Parse_error
  and parse_atom' () =
    match lookup () with
    | AndOp | OrOp | EOF | BracketR -> None
    | BinOp binop -> pop ();
                     begin
                     match lookup () with
                     | String rightarg | UnOp rightarg | BinOp rightarg
                       -> pop (); Some (binop,rightarg)
                     | _ -> raise Parse_error
                     end
    | _ -> raise Parse_error
  in parse_S ()       

module Self : Analyzer.S = struct

  let options = []

  let name = "test"

  let parsing_errors = ref []

  let process_script filename csts =

    let module Counter = struct

    let register_test filename invocation arguments =
      let arguments_unquoted = List.map UnQuote.on_string arguments
      and is_bracket = (invocation = "[" )
      in
      try
        let _ = parse is_bracket (List.map to_token arguments_unquoted)
        in ()
      with
        Parse_error ->
        parsing_errors := (filename, invocation, arguments) :: !parsing_errors

    class iterator' = object(self)

	inherit [_] Libmorbig.CST.iter as super

	method! visit_simple_command venv csts =
      let invocation = extract_command csts in
      match invocation with
      | Some s when (s = "test" || s = "[" )
        -> register_test filename s (extract_arguments csts)
      | _
        -> super#visit_simple_command venv csts
                
    end (* class iterator' = object ... *)
    end (* module Counter = struct ... *)
    in
    List.iter ((new Counter.iterator')#visit_complete_command ()) csts

  let output_report report =
    Report.add report
"* Test invocations

  Analyzer under construction.

** Tests that could not be parsed:\n";

    List.iter
      (function (filename,invocation,arguments) ->
                Report.add report "    - %s\n    "
                           (Report.link_to_source report filename);
                Report.add report "%s " invocation; 
                List.iter (function s -> Report.add report " %s" s) arguments;
                Report.add report "\n"
      )
      !parsing_errors;

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
