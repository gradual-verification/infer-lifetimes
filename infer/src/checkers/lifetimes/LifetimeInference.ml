(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = MayPointsToDomain

  type analysis_data = MayPointsToDomain.t InterproceduralAnalysis.t

  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : MayPointsToDomain.t)
      {InterproceduralAnalysis.proc_desc= _; tenv; analyze_dependency= _; _} _ _
      (instr : HilInstr.t) =
    match instr with
    | Call (_return_opt, Direct _callee_procname, _actuals, _, _loc) ->
        (* function call [return_opt] := invoke [callee_procname]([actuals]) *)
        astate
    | Assign (lhs, rhs, _loc) ->
        (* an assignment [lhs_access_path] := [rhs_exp] *)
        MayPointsToDomain.set_pointing_to astate tenv ~lhs ~rhs
    | Assume (_assume_exp, _, _, _loc) ->
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
        astate
    | Call (_, Indirect _, _, _, _) ->
        astate
    | Metadata _ ->
        astate

  let pp_session_name _node fmt = F.pp_print_string fmt "lifetimes"
end

(** 5(a) Type of CFG to analyze--Exceptional to follow exceptional control-flow edges, Normal to
    ignore them *)
module CFG = ProcCfg.Normal

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))


let report_annotation_or_error _ _post = 
()

(** Main function into the checker--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) =
  let result =
    Analyzer.compute_post analysis_data ~initial:(MayPointsToDomain.initial proc_desc tenv) proc_desc
  in
  Option.iter result ~f:(fun post -> report_annotation_or_error analysis_data post) ;
  result