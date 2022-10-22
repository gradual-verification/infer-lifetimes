open! IStd

open MayPointsToDomain
(* open MayPointsToDomain *)
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


