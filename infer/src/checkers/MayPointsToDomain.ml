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

type abstract_location =
  | Field of (abstract_location option * varinfo)
  | Variable of (string * Typ.t)
  | HeapMemory of Typ.t option
[@@deriving compare]

module AbstractLocation : sig
  include PrettyPrintable.PrintableOrderedType

  val generate_all_levels_of_indirection : string * Typ.t -> t list

  val of_base : AccessPath.base -> t option

  val of_var_data : ProcAttributes.var_data -> t
end = struct
  type t = abstract_location [@@deriving compare]

  let pp fmt loc =
    let name =
      match loc with Field (_, (nm, _)) -> nm | Variable (nm, _) -> nm | HeapMemory _ -> "a"
    in
    F.fprintf fmt "l_%s" name


  let rec generate_all_levels_of_indirection (pair : string * Typ.t) : abstract_location list =
    let var = fst pair in
    let typ = snd pair in
    [Variable (var, typ)]
    @ match typ.desc with Tptr (pt, _) -> generate_all_levels_of_indirection (var, pt) | _ -> []


  let of_var_data (vd : ProcAttributes.var_data) = Variable (Mangled.to_string vd.name, vd.typ)

  let of_base (b : AccessPath.base) : t option =
    let var = fst b in
    match Var.get_pvar var with
    | Some pv ->
        Some (Variable (Pvar.get_simplified_name pv, snd b))
    | None -> (
      match Var.get_ident var with
      | Some id ->
          Some (Variable (Ident.name_to_string (Ident.get_name id), snd b))
      | None ->
          None )
end

module AbstractLocationSet = struct
  include AbstractDomain.FiniteSet (AbstractLocation)

  let singleton loc = add loc empty
end

module MayPointsToMap = AbstractDomain.Map (AbstractLocation) (AbstractLocationSet)

type t = MayPointsToMap.t

let leq ~lhs ~rhs = MayPointsToMap.leq ~lhs ~rhs

let join a b = MayPointsToMap.join a b

let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

let pp fmt mp = MayPointsToMap.pp fmt mp

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


let dereference curr map =
  AbstractLocationSet.fold
    (fun loc s ->
      AbstractLocationSet.union s
        ( match MayPointsToMap.find_opt loc map with
        | Some set ->
            set
        | None ->
            AbstractLocationSet.empty ) )
    curr AbstractLocationSet.empty


let resolve_accesses_to_location_set (al : HilExp.t option HilExp.Access.t list) curr map =
  let represents_dereference (acc : HilExp.t option HilExp.Access.t) =
    match acc with
    | HilExp.Access.Dereference | HilExp.Access.ArrayAccess (_, _) ->
        false
    | _ ->
        true
  in
  let rec dereference_all curr map list =
    match list with
    | hd :: tl ->
        let next_set = if represents_dereference hd then dereference curr map else curr in
        dereference_all next_set map tl
    | [] ->
        curr
  in
  dereference_all curr map al


let filter_accesses (al : HilExp.t option HilExp.Access.t list) :
    HilExp.t option HilExp.Access.t list =
  let drop_last_if_present (to_drop : 'a list) =
    match List.drop_last to_drop with Some ls -> ls | None -> to_drop
  in
  let rec filter_accesses_rec (al : HilExp.t option HilExp.Access.t list)
      (derefs : HilExp.t option HilExp.Access.t list) =
    match al with
    | hd :: tl -> (
      match hd with
      | HilExp.Access.Dereference | HilExp.Access.ArrayAccess (_, _) ->
          filter_accesses_rec tl (derefs @ [hd])
      | HilExp.Access.TakeAddress ->
          filter_accesses_rec tl (drop_last_if_present derefs)
      | HilExp.Access.FieldAccess _ ->
          derefs @ filter_accesses_rec tl [] )
    | [] ->
        derefs
  in
  filter_accesses_rec al []


let get_lhs_locations (map : t) (rhs : HilExp.AccessExpression.t) : AbstractLocationSet.t option =
  let base = HilExp.AccessExpression.get_base rhs in
  let _, accesses = HilExp.AccessExpression.to_accesses rhs in
  let base_location = AbstractLocation.of_base base in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      let filtered = filter_accesses accesses in
      Some (resolve_accesses_to_location_set filtered singleton_set map)
  | None ->
      None


let rec find_inner_access_expr (ex : HilExp.t) =
  match ex with
  | HilExp.AccessExpression acc ->
      Some acc
  | HilExp.UnaryOperator (_, inner, _) ->
      find_inner_access_expr inner
  | HilExp.BinaryOperator (op, l, _) -> (
    match op with Binop.MinusPI | Binop.PlusPI -> find_inner_access_expr l | _ -> None )
  | HilExp.Cast (_, inner) ->
      find_inner_access_expr inner
  | _ ->
      None


let get_rhs_locations (map : t) (rhs : HilExp.AccessExpression.t) : AbstractLocationSet.t option =
  let base = HilExp.AccessExpression.get_base rhs in
  let _, accesses = HilExp.AccessExpression.to_accesses rhs in
  let base_location = AbstractLocation.of_base base in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      let filtered = filter_accesses ([HilExp.Access.Dereference] @ accesses) in
      Some (resolve_accesses_to_location_set filtered singleton_set map)
  | None ->
      None


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