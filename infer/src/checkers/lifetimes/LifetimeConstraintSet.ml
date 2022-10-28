open! IStd
module G = Graph
include AbstractDomain.Empty

module Env : sig
  type t

  val initial : t

  val constrain : lhs:HilExp.access_expression -> rhs:HilExp.t -> constr:t -> unit

  val pp : Format.formatter -> t -> unit
end = struct
  module DG = G.Imperative.Digraph.Abstract (LifetimeVariable)
  module LTVarMap = Caml.Map.Make (LifetimeVariable)
  module LTVarSet = Caml.Set.Make (LifetimeVariable)
  module UF = UnionFind.Make (LifetimeVariable) (LTVarSet) (LTVarMap)

  type t = {mutable equivalencies: UF.t; constraints: DG.t}

  let initial = {constraints= DG.create (); equivalencies= UF.empty}

  let pp fmt constr = UF.pp LifetimeVariable.pp fmt constr.equivalencies

  let add_outlives ~(constr : t) ~(pair : LifetimeVariable.t * LifetimeVariable.t) =
    let lhs, rhs = pair in
    let lv = DG.V.create lhs in
    let rv = DG.V.create rhs in
    let graph = constr.constraints in
    DG.add_edge graph lv rv


  let add_equality ~(constr : t) ~(pair : LifetimeVariable.t * LifetimeVariable.t) =
    let lhs, rhs = pair in
    let curr_eqv = constr.equivalencies in
    let updated = match UF.union curr_eqv lhs rhs with u, _ -> u in
    constr.equivalencies <- updated


  let align (ll : LifetimeVariable.t list) (rl : LifetimeVariable.t list) :
      (LifetimeVariable.t * LifetimeVariable.t) list option =
    match ll with
    | _ :: _ -> (
      match rl with
      | _ :: _ -> (
          let zipped_opt = List.zip ll rl in
          match zipped_opt with Ok ls -> Some ls | _ -> None )
      | [] ->
          None )
    | [] ->
        None


  let constrain ~(lhs : HilExp.access_expression) ~(rhs : HilExp.t) ~(constr : t) =
    let rhs_acc_opt = LifetimeUtils.find_inner_access_exp rhs in
    match rhs_acc_opt with
    | Some rhs_acc -> (
        let lhs_lifetimes = LifetimeVariable.of_aexp lhs in
        let rhs_lifetimes = LifetimeVariable.of_aexp rhs_acc in
        match align lhs_lifetimes rhs_lifetimes with
        | Some pairs -> (
          match pairs with
          | hd :: tl ->
              let () = add_outlives ~constr ~pair:(List.hd_exn pairs) in
              if LifetimeVariable.points_to_const (fst hd) then
                List.fold tl ~init:() ~f:(fun _ pair -> add_outlives ~constr ~pair)
              else List.fold tl ~init:() ~f:(fun _ pair -> add_equality ~constr ~pair)
          | [] ->
              () )
        | None ->
            () )
    | None ->
        ()
end

(*
   module LifetimeVariableSet = Caml.Set.Make (LifetimeVariable)
   module LifetimeVariableMap = Caml.Map.Make (LifetimeVariable)
   module EqualitySet = UnionFind.Make (LifetimeVariable) (LifetimeVariableSet) (LifetimeVariableMap)

   type t = {eqv: EqualitySet.t; outlives: LifetimeVariableSet.t LifetimeVariableMap.t}
   let initial = {eqv= EqualitySet.empty; outlives= LifetimeVariableMap.empty}
   let _add_subtype_constraint cst ~(lhs:LifetimeVariable.t) ~(rhs:LifetimeVariable.t) =
     let outlives = cst.outlives in
     let curr_set_opt = LifetimeVariableMap.find_opt lhs outlives in
     let curr_set = (match curr_set_opt with
     | Some(set) -> set
     | None -> LifetimeVariableSet.empty) in
     {
       eqv = cst.eqv;
       outlives = LifetimeVariableMap.add lhs (LifetimeVariableSet.add rhs curr_set) outlives;
     }

   let _add_equality_constraint cst ~(lhs:LifetimeVariable.t) ~(rhs:LifetimeVariable.t) =
     {
       outlives = cst.outlives;
       eqv = (match EqualitySet.union cst.eqv lhs rhs with
       | (set, _) -> set)
     }

   let _constrain_formal mptsto cset (formal:(Mangled.t * Typ.t * Annot.Item.t)) =
     let (_, tp, _) = formal in
     match tp.desc with
     | Tptr(_, _) -> (
       let _lifetime_vars = LifetimeVariable.of_formal in
       let base_location = AbstractLocation.create_loc_for_formal formal in
       let _pointing_to = MayPointsToMap.get_pointing_to mptsto base_location in cset

       (* traverse_constrain mptsto cset lthd lttl loc_set *)
     )
     | _ -> cset

   let generate_constraints proc_desc (astate:MayPointsToDomain.t) =
     let _ptsto = MayPointsToDomain.insensitive astate in
     let _constraint_set = initial in
     let _formals = Procdesc.get_formals proc_desc in
     " "
    (* List.fold formals ~init:constraint_set ~f:(fun cset fr -> constrain_formal ptsto cset fr) *)

   open MayPointsToDomain *)
