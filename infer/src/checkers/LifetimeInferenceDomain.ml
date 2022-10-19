(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type indirection = int [@@deriving compare]

type varinfo = string * Typ.t * indirection [@@deriving compare]

let string_of_varinfo vi =
  let n, t, i = vi in
  n ^ string_of_int i ^ ":" ^ Typ.to_string t


type abstract_location =
  | Field of (abstract_location * varinfo)
  | Variable of varinfo
  | HeapMemory of Typ.t option
[@@deriving compare]

module AbstractLocation : sig
  include PrettyPrintable.PrintableOrderedType

  val of_base : AccessPath.base -> t option

  val of_var_data : ProcAttributes.var_data -> t

  val create_field_offset : t -> Fieldname.t -> Typ.t -> t

  val create_variable: string -> Typ.t -> int -> t
end = struct
  type t = abstract_location [@@deriving compare]

  let pp fmt loc =
    let name =
      match loc with
      | Field (_, vi) ->
          string_of_varinfo vi
      | Variable vi ->
          string_of_varinfo vi
      | HeapMemory _ ->
          "a"
    in
    F.fprintf fmt "l_%s" name

  let of_var_data (vd : ProcAttributes.var_data) = Variable (Mangled.to_string vd.name, vd.typ, 0)

  let of_base (b : AccessPath.base) : t option =
    let var, typ = b in
    match Var.get_pvar var with
    | Some pv ->
        Some (Variable (Pvar.get_simplified_name pv, typ, 0))
    | None -> (
      match Var.get_ident var with
      | Some id ->
          Some (Variable (Ident.name_to_string (Ident.get_name id), typ, 0))
      | None ->
          None )


  let create_field_offset loc nm typ = Field (loc, (Fieldname.get_field_name nm, typ, 0))
  let create_variable nm typ ind = Variable(nm, typ, ind)
end

module AbstractLocationSet = struct
  include AbstractDomain.FiniteSet (AbstractLocation)

  let singleton loc = add loc empty
end

module MayPointsToMap = struct
  module PtsToMap = AbstractDomain.Map (AbstractLocation) (AbstractLocationSet)
  include PtsToMap

  let widen ~(prev : t) ~(next : t) ~num_iters:_ = join prev next

  let rec map_pairs (map : PtsToMap.t) (tlist : (AbstractLocation.t * AbstractLocation.t) list) : PtsToMap.t =
    match tlist with
    | (ptr, ptee) :: tl ->
        let singleton_set = AbstractLocationSet.add ptee AbstractLocationSet.empty in
        map_pairs (PtsToMap.add ptr singleton_set map) tl
    | [] ->
        map


  let already_visited (t : Typ.t) (visited_structs : String.Set.t) : bool =
    let type_name = Typ.to_string t in
    String.Set.exists visited_structs ~f:(fun nm -> phys_equal type_name nm)

  let strip_ptr (typ: Typ.t) : Typ.t = match typ.desc with | Tptr(inner, _) -> inner | _ -> typ

  let rec generate_locations_rec ~(ind : int) (tenv : Tenv.t) (parent : AbstractLocation.t)
      (pair : string * Typ.t) (visited_structs : String.Set.t) :
      ( AbstractLocation.t *  AbstractLocation.t) list =
    let vname, vtype = pair in
    let next_loc = AbstractLocation.create_variable vname vtype ind in
    let next_link = [(parent, next_loc)] in
    next_link
    @
    match vtype.desc with
    | Tstruct nm -> (
        if already_visited vtype visited_structs then []
        else
          let struct_recorded = String.Set.add visited_structs (Typ.to_string vtype) in
          let found_defn_opt = Tenv.lookup tenv nm in
          match found_defn_opt with
          | Some strct ->
              List.fold strct.fields ~init:[] ~f:(fun prev fld ->
                  prev
                  @
                  let nm, typ, _ = fld in
                  let inner_typ = strip_ptr typ in
                  if Typ.equal inner_typ typ then []
                  else
                    let next_pair = (Fieldname.to_string nm, inner_typ) in
                    let next_parent = AbstractLocation.create_field_offset next_loc nm inner_typ in
                    generate_locations_rec ~ind:1 tenv next_parent next_pair struct_recorded )
          | None ->
              [] )
    | Tarray ct ->
        let inner_array_loc = AbstractLocation.create_variable vname ct.elt (ind + 1) in
        [(next_loc, inner_array_loc)]
    | Tptr (pt, _) ->
        generate_locations_rec ~ind:(ind + 1) tenv next_loc (vname, pt) visited_structs
    | _ ->
        []


  let generate_locations (tenv : Tenv.t) (pair : string * Typ.t) =
    let vname, vtype = pair in
    let ind = 0 in
    let initial_location = AbstractLocation.create_variable vname vtype ind in
    let initial_struct_set = String.Set.empty in
    generate_locations_rec ~ind tenv initial_location pair initial_struct_set


  let initial (p : Procdesc.t)(tenv: Tenv.t) : PtsToMap.t =
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
          let gen = generate_locations tenv f in
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


let initial pdesc tenv =
  {sensitive= MayPointsToMap.initial pdesc tenv; insensitive= MayPointsToMap.initial pdesc tenv}


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


let resolve_accesses_to_location_set (al : HilExp.access_expression list) (tenv : Tenv.t) curr map =
  let offset_by_field curr nm tp =
    AbstractLocationSet.map (fun loc -> AbstractLocation.create_field_offset loc nm tp) curr
  in
  let rec dereference_all curr map (ls : HilExp.access_expression list) =
    match ls with
    | hd :: tl ->
        let next_set =
          match hd with
          | Dereference _ | ArrayOffset (_, _, _) ->
              dereference curr map
          | FieldOffset (_, nm) -> (
              let typ_opt = HilExp.AccessExpression.get_typ hd tenv in
              match typ_opt with Some typ -> offset_by_field curr nm typ | None -> curr )
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


let get_lhs_locations (map : MayPointsToMap.t) (tenv : Tenv.t) (rhs : HilExp.AccessExpression.t) :
    AbstractLocationSet.t option =
  let base = HilExp.AccessExpression.get_base rhs in
  let base_location = AbstractLocation.of_base base in
  let unfolded = unfold_access_exp rhs in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      Some (resolve_accesses_to_location_set unfolded tenv singleton_set map)
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


let get_rhs_locations (map : MayPointsToMap.t) (tenv : Tenv.t) (rhs : HilExp.t) :
    AbstractLocationSet.t option =
  let rhs_aexp_opt = find_inner_access_expr rhs in
  match rhs_aexp_opt with
  | Some rexp -> (
      let base = HilExp.AccessExpression.get_base rexp in
      let base_location = AbstractLocation.of_base base in
      let unfolded = unfold_access_exp rexp in
      match base_location with
      | Some loc ->
          let singleton_set = AbstractLocationSet.singleton loc in
          Some (dereference (resolve_accesses_to_location_set unfolded tenv singleton_set map) map)
      | None ->
          None )
  | None ->
      None


let set_pointing_to_sensitive (domain : t) (tenv : Tenv.t) (lhs : HilExp.access_expression)
    (rhs : HilExp.t) =
  let map = domain.sensitive in
  let lhs_pts_to_set = get_lhs_locations map tenv lhs in
  let rhs_pts_to_set = get_rhs_locations map tenv rhs in
  match (lhs_pts_to_set, rhs_pts_to_set) with
  | Some l, Some r ->
      AbstractLocationSet.fold (fun loc map -> MayPointsToMap.add loc r map) l map
  | _ ->
      map


let set_pointing_to_insensitive (domain : t) (tenv : Tenv.t) (lhs : HilExp.access_expression)
    (rhs : HilExp.t) =
  let map = domain.insensitive in
  let lhs_pts_to_set = get_lhs_locations map tenv lhs in
  let rhs_pts_to_set = get_rhs_locations map tenv rhs in
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


let set_pointing_to (domain : t) (tenv : Tenv.t) ~(lhs : HilExp.access_expression) ~(rhs : HilExp.t)
    =
  { sensitive= set_pointing_to_sensitive domain tenv lhs rhs
  ; insensitive= set_pointing_to_insensitive domain tenv lhs rhs }


type summary = t