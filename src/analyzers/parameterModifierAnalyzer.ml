(**************************************************************************)
(*  Copyright (C) 2017-2019 Nicolas Jeannerod, Yann Régis-Gianas,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(**************************************************************************)

open ExtPervasives
open Morbig.CST
open Options
open Messages

let options = []

let name = "parameterModifier"

let cntNoAttribute                  = ref 0
let cntParameterLength              = ref 0
let cntUseDefaultValues             = ref 0
let cntAssignDefaultValues          = ref 0
let cntIndicateErrorifNullorUnset   = ref 0
let cntUseAlternativeValue          = ref 0
let cntRemoveSmallestSuffixPattern  = ref 0
let cntRemoveLargestSuffixPattern   = ref 0
let cntRemoveSmallestPrefixPattern  = ref 0
let cntRemoveLargestPrefixPattern   = ref 0

let process_script filename csts =
  let counter =
    object(self)
      inherit [_] Morbig.CST.iter as super
      method! visit_variable_attribute _env = function
        | NoAttribute ->
           incr cntNoAttribute
        | ParameterLength _ ->
           incr cntParameterLength
        | UseDefaultValues _ ->
           incr cntUseDefaultValues
        | AssignDefaultValues _ ->
           incr cntAssignDefaultValues
        | IndicateErrorifNullorUnset _ ->
           incr cntIndicateErrorifNullorUnset
        | UseAlternativeValue _ ->
           incr cntUseAlternativeValue
        | RemoveSmallestSuffixPattern _ ->
           incr cntRemoveSmallestSuffixPattern
        | RemoveLargestSuffixPattern _ ->
           incr cntRemoveLargestSuffixPattern
        | RemoveSmallestPrefixPattern _ ->
           incr cntRemoveSmallestPrefixPattern
        | RemoveLargestPrefixPattern _ ->
           incr cntRemoveLargestPrefixPattern
    end
  in counter#visit_program () csts

let output_report report =
  let p = [
      "None", !cntNoAttribute;
      "ParameterLength", !cntParameterLength;
      "UseDefaultValues", !cntUseDefaultValues;
      "AssignDefaultValues", !cntAssignDefaultValues;
      "IndicateErrorIfNullOrUnset", !cntIndicateErrorifNullorUnset;
      "UseAlternativeValues", !cntUseAlternativeValue;
      "RemoveSmallestSuffixPattern", !cntRemoveSmallestSuffixPattern;
      "RemoveLargestSmallestSuffixPattern", !cntRemoveLargestSuffixPattern;
      "RemoveSmallestPrefixPattern", !cntRemoveSmallestPrefixPattern;
      "RemoveLargestPrefixPattern", !cntRemoveLargestPrefixPattern;
    ]
  in
  Report.add report "* Statistics of parameter modfiers\n";
  Report.add report "  %-38s| %s\n" "Operator" "Occurrences";
  Report.add report "  --------------------------------------+------------\n";
  List.iter
    (function (name,value) -> Report.add report "  %-38s| %7i\n" name value)
    p; 
  Report.add report "\n"
