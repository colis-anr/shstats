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
  | ParL              (* ( *)
  | ParR              (* ) *)
  | BracketR          (* ] *)
  | String of string  (* all the rest *)

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
  | _    -> String s

module Self : Analyzer.S = struct

  let options = []

  let name = "test"

  let process_script filename csts =

    let module Counter = struct

    let register_test is_bracket arguments =
      (* is_bracket is true when the command was "[", false when "test" *)
      ()
                                                   
    class iterator' = object(self)

	inherit [_] Libmorbig.CST.iter as super

	method! visit_simple_command venv csts =
      match extract_command csts with
      | Some "test" ->
         register_test false (List.map to_token (extract_arguments csts))
      | Some "[" ->
         register_test true (List.map to_token (extract_arguments csts))
      | _ ->
         super#visit_simple_command venv csts
                
    end (* class iterator' = object ... *)
    end (* module Counter = struct ... *)
    in
    List.iter ((new Counter.iterator')#visit_complete_command ()) csts

  let output_report report =
    Report.add report
"* Test invocations

  Analyzer under construction."

end (* module Self = struct ... *)

let install = Analyzer.register (module Self)
