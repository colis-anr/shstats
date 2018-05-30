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
    -> unCmdWord' cmdword'
  | SimpleCommand_CmdName_CmdSuffix (cmdname',_)
  | SimpleCommand_CmdName cmdname'
    -> unCmdName' cmdname'
  | SimpleCommand_CmdPrefix _prefix
    -> raise Not_found

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

module Self : Analyzer.S = struct

  let options = []

  let name = "test"

  let process_script filename csts =

    let module Counter = struct
    class iterator' = object(self)

	inherit [_] Libmorbig.CST.iter as super

	method! visit_simple_command venv csts =
      let command = extract_command csts
      and arguments = extract_arguments csts
      in ()
      
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
