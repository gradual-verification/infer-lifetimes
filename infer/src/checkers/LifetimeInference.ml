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
      {InterproceduralAnalysis.proc_desc= _; tenv= tenv; analyze_dependency= _; _} _ _
      (instr : HilInstr.t) =
    match instr with
    | Call (_return_opt, Direct _callee_procname, _actuals, _, _loc) ->
        (* function call [return_opt] := invoke [callee_procname]([actuals]) *)
        astate
    | Assign (lhs_access_path, rhs_exp, _loc) -> (
        (* an assignment [lhs_access_path] := [rhs_exp] *)
        let rhs_acc_opt = MayPointsToDomain.find_inner_access_expr rhs_exp in
        match rhs_acc_opt with
        | Some acc -> (
            let lhs_pts_to_set = MayPointsToDomain.get_lhs_locations astate tenv lhs_access_path in
            let rhs_pts_to_set = MayPointsToDomain.get_rhs_locations astate tenv acc in
            match (lhs_pts_to_set, rhs_pts_to_set) with
            | Some l, Some r ->
                MayPointsToDomain.set_pointing_to astate l r
            | _ ->
                astate )
        | None ->
            astate )
    | Assume (_assume_exp, _, _, _loc) ->
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
        astate
    | Call (_, Indirect _, _, _, _) ->
        astate
    | Metadata _ ->
        astate

  let pp_session_name _node fmt = F.pp_print_string fmt "may-points-to"
end

(** 5(a) Type of CFG to analyze--Exceptional to follow exceptional control-flow edges, Normal to
    ignore them *)
module CFG = ProcCfg.Normal

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

(** Report an error when we have acquired more resources than we have released *)
let report_if_leak {InterproceduralAnalysis.proc_desc; err_log; _} post =
  let change_me = false in
  if change_me then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let message = F.asprintf "Leaked %a resource(s)" MayPointsToDomain.pp post in
    Reporting.log_issue proc_desc err_log ~loc:last_loc LifetimeInference
      IssueType.lab_resource_leak message


(** Main function into the checker--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) =
  let result =
    Analyzer.compute_post analysis_data ~initial:(MayPointsToDomain.initial proc_desc tenv) proc_desc
  in
  Option.iter result ~f:(fun post -> report_if_leak analysis_data post) ;
  result
