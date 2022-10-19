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
  | Field of (abstract_location option * string)
  | Variable of string
  | HeapMemory of Typ.t option
[@@deriving compare]

module AbstractLocation : sig
  include PrettyPrintable.PrintableOrderedType

  val generate_all_levels_of_indirection : string * Typ.t -> t list

  val of_base : AccessPath.base -> t option

  val of_var_data : ProcAttributes.var_data -> t

  val create_field_offset : t -> Fieldname.t -> t
end = struct
  type t = abstract_location [@@deriving compare]

  let pp fmt loc =
    let name = match loc with Field (_, nm) -> nm | Variable nm -> nm | HeapMemory _ -> "a" in
    F.fprintf fmt "l_%s" name


  let rec generate_all_levels_of_indirection (pair : string * Typ.t) : abstract_location list =
    let var = fst pair in
    let typ = snd pair in
    [Variable var]
    @ match typ.desc with Tptr (pt, _) -> generate_all_levels_of_indirection (var, pt) | _ -> []


  let of_var_data (vd : ProcAttributes.var_data) = Variable (Mangled.to_string vd.name)

  let of_base (b : AccessPath.base) : t option =
    let var = fst b in
    match Var.get_pvar var with
    | Some pv ->
        Some (Variable (Pvar.get_simplified_name pv))
    | None -> (
      match Var.get_ident var with
      | Some id ->
          Some (Variable (Ident.name_to_string (Ident.get_name id)))
      | None ->
          None )


  let create_field_offset loc nm = Field (Some loc, Fieldname.get_field_name nm)
end

module AbstractLocationSet = struct
  include AbstractDomain.FiniteSet (AbstractLocation)

  let singleton loc = add loc empty
end

module MayPointsToMap = struct
  module PtsToMap = AbstractDomain.Map (AbstractLocation) (AbstractLocationSet)
  include PtsToMap

  let widen ~(prev : t) ~(next : t) ~num_iters:_ = join prev next

  let rec map_pairs (map : PtsToMap.t) (tlist : AbstractLocation.t list) : PtsToMap.t =
    match tlist with
    | h1 :: h2 :: tl ->
        let singleton_set = AbstractLocationSet.add h2 AbstractLocationSet.empty in
        map_pairs (PtsToMap.add h1 singleton_set map) ([h2] @ tl)
    | [h] ->
        PtsToMap.add h AbstractLocationSet.empty map
    | [] ->
        map


  let initial (p : Procdesc.t) : PtsToMap.t =
    let formals =
      List.map (Procdesc.get_formals p) ~f:(fun pf -> (Mangled.to_string (fst3 pf), snd3 pf))
    in
    let locals = Procdesc.get_locals p in
    let abs_locals = List.map locals ~f:(fun loc -> AbstractLocation.of_var_data loc) in
    let with_locals =
      List.fold abs_locals ~init:PtsToMap.empty ~f:(fun map loc ->
          PtsToMap.add loc AbstractLocationSet.empty map )
    in
    let with_formals =
      List.fold formals ~init:with_locals ~f:(fun map f ->
          let gen = AbstractLocation.generate_all_levels_of_indirection f in
          map_pairs map gen )
    in
    with_formals
end

include AbstractDomain.Empty

type t = {sensitive: MayPointsToMap.t; insensitive: MayPointsToMap.t}

let leq ~lhs ~rhs =
  MayPointsToMap.leq ~lhs:lhs.sensitive ~rhs:rhs.sensitive
  && MayPointsToMap.leq ~lhs:lhs.insensitive ~rhs:rhs.insensitive


let join a b =
  { sensitive= MayPointsToMap.join a.sensitive b.sensitive
  ; insensitive= MayPointsToMap.join a.insensitive b.insensitive }


let widen ~(prev : t) ~(next : t) ~(num_iters : int) =
  { sensitive= MayPointsToMap.widen ~prev:prev.sensitive ~next:next.sensitive ~num_iters
  ; insensitive= MayPointsToMap.widen ~prev:prev.insensitive ~next:next.insensitive ~num_iters }


let pp fmt st =
  MayPointsToMap.pp fmt st.sensitive ;
  MayPointsToMap.pp fmt st.insensitive


let initial pdesc _tenv =
  {sensitive= MayPointsToMap.initial pdesc; insensitive= MayPointsToMap.initial pdesc}


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


let resolve_accesses_to_location_set (al : HilExp.access_expression list) curr map =
  let offset_by_field curr nm _tp =
    AbstractLocationSet.map (fun loc -> AbstractLocation.create_field_offset loc nm) curr
  in
  let rec dereference_all curr map (ls : HilExp.access_expression list) =
    match ls with
    | hd :: tl ->
        let next_set =
          match hd with
          | Dereference _ | ArrayOffset (_, _, _) ->
              dereference curr map
          | FieldOffset (_, nm) ->
              offset_by_field curr nm nm
          | Base _ | AddressOf _ ->
              curr
        in
        dereference_all next_set map tl
    | [] ->
        curr
  in
  dereference_all curr map al


let unfold_access_exp (aex : HilExp.access_expression) =
  let rec unfold_access_exp_rec (aex : HilExp.access_expression)
      (derefs : HilExp.access_expression list) (addrofs : HilExp.access_expression list) :
      HilExp.access_expression list =
    match aex with
    | Base _ ->
        [aex]
    | FieldOffset (inner, _) ->
        (derefs @ addrofs) @ [aex] @ unfold_access_exp_rec inner [] []
    | AddressOf inner ->
        if List.length derefs > 0 then
          unfold_access_exp_rec inner (List.drop_last_exn derefs) addrofs
        else unfold_access_exp_rec inner derefs (addrofs @ [aex])
    | Dereference inner ->
        if List.length addrofs > 0 then
          unfold_access_exp_rec inner derefs (List.drop_last_exn addrofs)
        else unfold_access_exp_rec inner (derefs @ [aex]) addrofs
    | ArrayOffset (inner, _, _) ->
        if List.length addrofs > 0 then
          unfold_access_exp_rec inner derefs (List.drop_last_exn addrofs)
        else unfold_access_exp_rec inner (derefs @ [aex]) addrofs
  in
  unfold_access_exp_rec aex [] []


let get_lhs_locations (map : MayPointsToMap.t) (rhs : HilExp.AccessExpression.t) :
    AbstractLocationSet.t option =
  let base = HilExp.AccessExpression.get_base rhs in
  let base_location = AbstractLocation.of_base base in
  let unfolded = unfold_access_exp rhs in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      Some (resolve_accesses_to_location_set unfolded singleton_set map)
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


let get_rhs_locations (map : MayPointsToMap.t) (rhs : HilExp.AccessExpression.t) :
    AbstractLocationSet.t option =
  let base = HilExp.AccessExpression.get_base rhs in
  let base_location = AbstractLocation.of_base base in
  let unfolded = unfold_access_exp rhs in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      Some (dereference (resolve_accesses_to_location_set unfolded singleton_set map) map)
  | None ->
      None


let set_pointing_to_sensitive (domain : t) (lhs : HilExp.access_expression)
    (rhs : HilExp.access_expression) =
  let map = domain.sensitive in
  let lhs_pts_to_set = get_lhs_locations map lhs in
  let rhs_pts_to_set = get_rhs_locations map rhs in
  match (lhs_pts_to_set, rhs_pts_to_set) with
  | Some l, Some r ->
      AbstractLocationSet.fold (fun loc map -> MayPointsToMap.add loc r map) l map
  | _ ->
      map


let set_pointing_to_insensitive (domain : t) (lhs : HilExp.access_expression)
    (rhs : HilExp.access_expression) =
  let map = domain.insensitive in
  let lhs_pts_to_set = get_lhs_locations map lhs in
  let rhs_pts_to_set = get_rhs_locations map rhs in
  match (lhs_pts_to_set, rhs_pts_to_set) with
  | Some l, Some r ->
      AbstractLocationSet.fold
        (fun loc map ->
          let current_opt = MayPointsToMap.find_opt loc map in
          match current_opt with
          | Some curr ->
              MayPointsToMap.add loc (AbstractLocationSet.union r curr) map
          | None ->
              MayPointsToMap.add loc l map )
        l map
  | _ ->
      map


let set_pointing_to (domain : t) ~(lhs : HilExp.access_expression) ~(rhs : HilExp.t) =
  let rhs_acc_opt = find_inner_access_expr rhs in
  match rhs_acc_opt with
  | Some acc ->
      { sensitive= set_pointing_to_sensitive domain lhs acc
      ; insensitive= set_pointing_to_insensitive domain lhs acc }
  | None ->
      domain


type summary = t