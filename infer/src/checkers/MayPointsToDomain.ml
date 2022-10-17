(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type indirection = int [@@deriving compare]

type varinfo =
  | Named of (string * Typ.t)
  | MaybeUntyped of (string * Typ.t option)
  | Unnamed of (Typ.t * indirection) option
[@@deriving compare]

type abstract_location = Variable of varinfo * indirection | Field of (abstract_location * varinfo)
[@@deriving compare]

let string_of_varinfo vi =
  match vi with
  | Named (s, _) ->
      s
  | MaybeUntyped (s, _) ->
      s
  | Unnamed (Some (_, i)) ->
      "a" ^ string_of_int i
  | Unnamed None ->
      "a?"


module AbstractLocation : sig
  include PrettyPrintable.PrintableOrderedType

  val generate_all_levels_of_indirection : ?ind:int -> string * Typ.t -> t list

  val of_base : AccessPath.base -> t option

  val of_var_data : ProcAttributes.var_data -> t

  val offset_by_field : t -> Typ.t option -> string -> t
end = struct
  type t = abstract_location [@@deriving compare]

  let rec format_rec loc : string =
    match loc with
    | Field (p, vi) ->
        format_rec p ^ "." ^ string_of_varinfo vi
    | Variable (vi, id) ->
        "l_" ^ string_of_varinfo vi ^ string_of_int id


  let pp fmt loc = F.fprintf fmt "%s" (format_rec loc)

  let rec generate_all_levels_of_indirection ?(ind = 0) (pair : string * Typ.t) :
      abstract_location list =
    let var = fst pair in
    let typ = snd pair in
    [Variable (Named (var, typ), ind)]
    @
    match typ.desc with
    | Tptr (pt, _) ->
        [Variable (Named (var, typ), ind)] @ generate_all_levels_of_indirection (var, pt)
    | _ ->
        []


  let of_var_data (vd : ProcAttributes.var_data) =
    Variable (Named (Mangled.to_string vd.name, vd.typ), 0)


  let offset_by_field ploc typ fname = Field (ploc, MaybeUntyped (fname, typ))

  let of_base (b : AccessPath.base) : t option =
    let var = fst b in
    match Var.get_pvar var with
    | Some pv ->
        Some (Variable (Named (Pvar.get_simplified_name pv, snd b), 0))
    | None -> (
      match Var.get_ident var with
      | Some id ->
          Some (Variable (Named (Ident.name_to_string (Ident.get_name id), snd b), 0))
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


let dereference (curr : AbstractLocationSet.t) (map : MayPointsToMap.t) =
  AbstractLocationSet.fold
    (fun loc s ->
      AbstractLocationSet.union s
        ( match MayPointsToMap.find_opt loc map with
        | Some set ->
            set
        | None ->
            AbstractLocationSet.empty ) )
    curr AbstractLocationSet.empty


let field_offset (aex : HilExp.access_expression) (fname : Fieldname.t)
    (curr : AbstractLocationSet.t) (tenv : Tenv.t) =
  let accessed_type = HilExp.AccessExpression.get_typ aex tenv in
  let fieldname = Fieldname.get_field_name fname in
  AbstractLocationSet.map
    (fun loc -> AbstractLocation.offset_by_field loc accessed_type fieldname)
    curr


let rec resolve_accesses_to_location_set (al : HilExp.access_expression list) curr map tenv =
  match al with
  | hd :: tl -> (
    match hd with
    | HilExp.Dereference _ | HilExp.ArrayOffset (_, _, _) ->
        resolve_accesses_to_location_set tl (dereference curr map) map tenv
    | HilExp.FieldOffset (_, nm) ->
        resolve_accesses_to_location_set tl (field_offset hd nm curr tenv) map tenv
    | HilExp.AddressOf _ ->
        resolve_accesses_to_location_set tl curr map tenv
    | _ ->
        curr )
  | [] ->
      curr


let rec unfold_access_expr ?(derefs : HilExp.access_expression list = [])
    (aex : HilExp.access_expression) : HilExp.access_expression list =
  let drop_last_if_present (to_drop : 'a list) =
    match List.drop_last to_drop with Some ls -> ls | None -> to_drop
  in
  match aex with
  | Base _ ->
      derefs @ [aex]
  | FieldOffset (inner, _) ->
      derefs @ unfold_access_expr inner ~derefs:[]
  | ArrayOffset (inner, _, _) ->
      unfold_access_expr ~derefs:(derefs @ [inner]) inner
  | AddressOf inner ->
      unfold_access_expr inner ~derefs:(drop_last_if_present derefs)
  | Dereference inner ->
      unfold_access_expr inner ~derefs:(derefs @ [aex])


(*
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
  filter_accesses_rec al [] *)

let get_lhs_locations (map : t) (tenv : Tenv.t) (rhs : HilExp.AccessExpression.t) :
    AbstractLocationSet.t option =
  let base = HilExp.AccessExpression.get_base rhs in
  let base_location = AbstractLocation.of_base base in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      let filtered = unfold_access_expr rhs in
      Some (resolve_accesses_to_location_set filtered singleton_set map tenv)
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


let get_rhs_locations (map : t) (tenv : Tenv.t) (rhs : HilExp.AccessExpression.t) :
    AbstractLocationSet.t option =
  let base = HilExp.AccessExpression.get_base rhs in
  let base_location = AbstractLocation.of_base base in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      let filtered = unfold_access_expr rhs in
      Some (resolve_accesses_to_location_set filtered singleton_set map tenv)
  | None ->
      None


let initial (p : Procdesc.t) (_tenv : Tenv.t) : t =
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