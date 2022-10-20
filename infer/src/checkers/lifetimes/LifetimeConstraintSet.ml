open! IStd
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