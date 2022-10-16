(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type indirection = int [@@deriving compare]

type varinfo = string * Typ.t [@@deriving compare]

type abstract_location = Variable of (string * Typ.t) | Alloc of Typ.t [@@deriving compare]

module AbstractLocation : sig
  include PrettyPrintable.PrintableOrderedType

  val generate_all_levels_of_indirection : string * Typ.t -> t list

  val _of_base : AccessPath.base -> t option

  val of_var_data : ProcAttributes.var_data -> t
end = struct
  type t = abstract_location [@@deriving compare]

  let pp _fmt _loc = ()

  let rec generate_all_levels_of_indirection (pair : string * Typ.t) : abstract_location list =
    let var = fst pair in
    let typ = snd pair in
    [Variable (var, typ)]
    @ match typ.desc with Tptr (pt, _) -> generate_all_levels_of_indirection (var, pt) | _ -> []


  let of_var_data (vd : ProcAttributes.var_data) = Variable (Mangled.to_string vd.name, vd.typ)

  let _of_base (b : AccessPath.base) : t option =
    let var = fst b in
    match Var.get_pvar var with
    | Some pv ->
        Some (Variable (Pvar.get_simplified_name pv, snd b))
    | None ->
        None
end

module AbstractLocationSet = AbstractDomain.FiniteSet (AbstractLocation)
module MayPointsToMap = AbstractDomain.Map (AbstractLocation) (AbstractLocationSet)

type t = MayPointsToMap.t

let leq ~lhs ~rhs = MayPointsToMap.leq ~lhs ~rhs

let join a b = MayPointsToMap.join a b

let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

let _pp fmt () = F.fprintf fmt "(nothing)"

let rec map_pairs (map : MayPointsToMap.t) (tlist : AbstractLocation.t list) : MayPointsToMap.t =
  match tlist with
  | h1 :: h2 :: tl ->
      let singleton_set = AbstractLocationSet.add h2 AbstractLocationSet.empty in
      map_pairs (MayPointsToMap.add h1 singleton_set map) ([h2] @ tl)
  | [h] ->
      MayPointsToMap.add h AbstractLocationSet.empty map
  | [] ->
      map

let set_pointing_to (map : t) (lhs : AbstractLocationSet.t) (rhs : AbstractLocationSet.t) =
  AbstractLocationSet.fold (fun loc map -> MayPointsToMap.add loc rhs map) lhs map

let get_lhs_locations (_map: t) (_lhs: HilExp.AccessExpression.t) : AbstractLocationSet.t = AbstractLocationSet.empty
let get_rhs_locations (_map: t) (_lhs: HilExp.t) :  AbstractLocationSet.t = AbstractLocationSet.empty

let initial (p : Procdesc.t) : t =
  let formals =
    List.map (Procdesc.get_formals p) ~f:(fun pf -> (Mangled.to_string (fst3 pf), snd3 pf))
  in
  let locals = Procdesc.get_locals p in
  let abs_locals = List.map locals ~f:(fun loc -> AbstractLocation.of_var_data loc) in
  let with_locals =
    List.fold abs_locals ~init:MayPointsToMap.empty ~f:(fun map loc ->
        MayPointsToMap.add loc AbstractLocationSet.empty map )
  in
  let with_formals =
    List.fold formals ~init:with_locals ~f:(fun map f ->
        let gen = AbstractLocation.generate_all_levels_of_indirection f in
        map_pairs map gen )
  in
  with_formals

type summary = t

let pp _fmt _loc = ()