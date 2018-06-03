open ExtPervasives
open Libmorbig.CST
open Options
open Commands
open Messages

module Name =
  struct
    type t = string
    let compare = Pervasives.compare
  end

module NameSet = Set.Make(Name)

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

let rec has_bool_combinators = function
  | Andtest (_,_) | Ortest (_,_) -> true
  | Onetest l -> has_bool_lit l
and has_bool_lit = function
  | Postest a | Negtest a -> has_bool_atom a
and has_bool_atom = function
  | Partest e -> has_bool_combinators e
  | _ -> false

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


class stringcounter = object (self)
  val counters : (string,int) Hashtbl.t = Hashtbl.create 8

  method incr key =
    try
      let oldval = Hashtbl.find counters key in
      Hashtbl.replace counters key (oldval+1)
    with
      Not_found ->
      Hashtbl.add counters key 1

  method iter_ascending f =
    List.iter f 
      (List.sort
         (fun (_,x) (_,y) -> y-x)
         (Hashtbl.fold (fun key value acc -> (key,value)::acc) counters []))
end

module Self : Analyzer.S = struct

  let options = []

  let name = "test"

  let parsing_errors = ref []
  let count_uniops = new stringcounter
  let count_binops = new stringcounter
  let count_contest = ref 0
  let count_testinvocations = ref 0
  let scripts_with_complex_tests = ref NameSet.empty
  let count_complex_tests = ref 0
  let count_dollarone = new stringcounter

  let process_script filename csts =

    let module Counter = struct

    let rec process_expr = function
      | Andtest (e1,e2) -> process_expr e1; process_expr e2
      | Ortest (e1,e2) -> process_expr e1; process_expr e2
      | Onetest l -> process_lit l
    and process_lit = function
      | Postest a -> process_atom a
      | Negtest a -> process_atom a
    and process_atom = function
      | Bintest (op,left,right) -> begin
          count_binops#incr(op);
          if op = "=" || op = "!="
          then if left = "$1"
               then count_dollarone#incr(right)
               else if right = "$1"
               then count_dollarone#incr(left)
        end
      | Unitest (op,_) -> count_uniops#incr(op)
      | Contest _ -> incr count_contest
      | Partest e -> process_expr e
      
    let register_test filename invocation arguments =
      let arguments_unquoted = List.map UnQuote.on_string arguments
      and is_bracket = (invocation = "[" )
      in
      incr count_testinvocations;
      try
        let ast = parse is_bracket (List.map to_token arguments_unquoted)
        in
        process_expr ast;
        if has_bool_combinators ast
        then begin
            scripts_with_complex_tests :=
              NameSet.add filename (!scripts_with_complex_tests);
            incr count_complex_tests
          end
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
    Report.add report "* Test invocations\n";
    Report.add report "  Number of test or []: %d\n" !count_testinvocations; 

    Report.add report "** Tests that could not be parsed\n\n";
    List.iter
      (function (filename,invocation,arguments) ->
                Report.add report "    - %s\n    "
                           (Report.link_to_source report filename);
                Report.add report "%s " invocation; 
                List.iter (function s -> Report.add report " %s" s) arguments;
                Report.add report "\n"
      )
      !parsing_errors;

    Report.add report "** Unary test operators\n\n";
    Report.add report "  Operator | Occurrences\n";
    Report.add report "  ---------+------------\n";
    count_uniops#iter_ascending (fun (key,number) ->
        Report.add report "   %5s   | %8d \n" key number);
    Report.add report "\n";

    Report.add report "** Binary test operators\n\n";
    Report.add report "  Operator | Occurrences\n";
    Report.add report "  ---------+------------\n";
    count_binops#iter_ascending (fun (key,number) ->
        Report.add report "   %5s   | %8d \n" key number);
    Report.add report "\n";

    Report.add report "** Tests using boolean operators (-a, -o)\n\n";
    Report.add report "  Number of tests: %d\n" !count_complex_tests;
    Report.add report "  Number of scripts: %d\n"
      (NameSet.cardinal !scripts_with_complex_tests);
    Report.add report "*** Listing of scripts\n\n";
    NameSet.iter
      (function filename ->
         Report.add report "    - %s\n"
           (Report.link_to_source report filename))
      !scripts_with_complex_tests;
    Report.add report "\n";
    
    Report.add report "** Comparisons with $1\n\n";
    Report.add report "  Compared with           | Occurrences\n";
    Report.add report "  ------------------------+------------\n";
    count_dollarone#iter_ascending (fun (key,number) ->
        Report.add report "   %20s   | %8d \n" key number);
    Report.add report "\n";

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
