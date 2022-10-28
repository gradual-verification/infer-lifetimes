

 open! IStd
 module L = Logging
 module CFG = ProcCfg.NormalOneInstrPerNode

 
 module Report = struct
   (** A utility module for use while developing. This would likely be integrated within a proper
       Summary one when interprocedural analysis is supported. *)
 
   let debug result proc_desc =
     (* For now we just want to print environment somewhere to debug the analysis. *)
     let procname = Procdesc.get_proc_name proc_desc in
     L.debug Analysis Verbose "@[<v>Result for procedure : %a@,@]" Procname.pp procname ;
     L.debug Analysis Verbose "@[<v>%a@,@]" LifetimeConstraintSet.Env.pp result
 end

 module TransferFunctions (Env : sig
   val env : LifetimeConstraintSet.Env.t
 end)(CFG : ProcCfg.S) =
 struct
   module Domain = AbstractDomain.Unit
   module CFG = CFG
 
   type analysis_data = AbstractDomain.Unit.t InterproceduralAnalysis.t
 
   let constr = Env.env
 
   (** Returns the shape of an expression. Fresh shapes will be created as needed. *)

   (** Execute an instruction by mutating the environment *)
   let exec_instr (astate : Domain.t)
   {InterproceduralAnalysis.proc_desc= _; analyze_dependency= _; _} _ _
   (instr : HilInstr.t) =
 match instr with
 | Call (_return_opt, Direct _callee_procname, _actuals, _, _loc) ->
     (* function call [return_opt] := invoke [callee_procname]([actuals]) *)
     astate
 | Assign (lhs, rhs, _loc) ->
    LifetimeConstraintSet.Env.constrain ~lhs ~rhs ~constr 
    
 | Assume (_assume_exp, _, _, _loc) ->
     (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
     astate
 | Call (_, Indirect _, _, _, _) ->
     astate
 | Metadata _ ->
     astate
   (** Mutates the environment and then return an abstract state (which is actually the same as the
       parameter). *)
   let pp_session_name _node fmt = Format.pp_print_string fmt "Lifetime Inference"
 end
 
 (** A generative module that creates a fresh environment and passes it to the {!TransferFunctions}
     functor to build an analysis engine. *)
 module Analyzer () = struct
   module Env = struct
     let env = LifetimeConstraintSet.Env.initial
   end
 
  include LowerHil.MakeAbstractInterpreter(TransferFunctions(Env)(CFG))
end
 
 let unskipped_checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
   let module Analyzer = Analyzer () in
   let _invmap = Analyzer.compute_post analysis_data ~initial:() proc_desc in
   (* Just print the final environment in the debug logs for now *)
   Report.debug Analyzer.Env.env proc_desc ;
   ()
 
 
 let checker = unskipped_checker
 