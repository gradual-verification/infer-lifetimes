open! IStd
module F = Format

module AbstractLocationSet = struct
  include AbstractDomain.FiniteSet (AbstractLocation)

  let singleton loc = add loc empty

  let to_list set : AbstractLocation.t list =
    fold (fun (elt : AbstractLocation.t) ls -> ls @ [elt]) set []
end

module MayPointsToMap = struct
  module PtsToMap = AbstractDomain.Map (AbstractLocation) (AbstractLocationSet)
  include PtsToMap
  module TypeSet = Set.Make (Typ)

  let widen ~(prev : t) ~(next : t) ~num_iters:_ = join prev next

  let rec map_pairs (map : PtsToMap.t) (tlist : (AbstractLocation.t * AbstractLocation.t) list) :
      PtsToMap.t =
    match tlist with
    | (ptr, ptee) :: tl ->
        let singleton_set = AbstractLocationSet.add ptee AbstractLocationSet.empty in
        map_pairs (PtsToMap.add ptr singleton_set map) tl
    | [] ->
        map


  let already_visited (t : Typ.t) (visited_structs : TypeSet.t) : bool =
    TypeSet.exists visited_structs ~f:(fun str -> Typ.equal t str)


  let rec populate_locations_rec ~(ind:int) (tenv : Tenv.t) (parent : AbstractLocation.t)
      (pair : string * Typ.t) (visited_structs : TypeSet.t) :
      (AbstractLocation.t * AbstractLocation.t) list =
    let vname, vtype = pair in
    let next_loc = AbstractLocation.create_loc_for_variable ~ind vname vtype in
    let next_link = [(parent, next_loc)] in
    next_link
    @
    match vtype.desc with
    | Tstruct nm -> (
        if already_visited vtype visited_structs then []
        else
          let struct_recorded = TypeSet.add visited_structs vtype in
          let found_defn_opt = Tenv.lookup tenv nm in
          match found_defn_opt with
          | Some strct ->
              List.fold strct.fields ~init:[] ~f:(fun prev fld ->
                  prev
                  @
                  let nm, typ, _ = fld in
                  let inner_typ = Typ.strip_ptr typ in
                  if Typ.equal inner_typ typ then []
                  else
                    let next_pair = (Fieldname.to_string nm, inner_typ) in
                    let next_parent =
                      AbstractLocation.create_loc_for_field_offset next_loc nm inner_typ
                    in
                    populate_locations_rec ~ind:1 tenv next_parent next_pair struct_recorded )
          | None ->
              [] )
    | Tarray ct ->
        let inner_array_loc =
          AbstractLocation.create_loc_for_variable vname ct.elt ~ind:(ind + 1)
        in
        [(next_loc, inner_array_loc)]
    | Tptr (pt, _) ->
        populate_locations_rec ~ind:(ind + 1) tenv next_loc (vname, pt) visited_structs
    | _ ->
        []


  let populate_locations (tenv : Tenv.t) (pair : string * Typ.t) =
    let vname, vtype = pair in
    let initial_location = AbstractLocation.create_loc_for_variable vname vtype in
    let initial_struct_set = TypeSet.empty in
    populate_locations_rec ~ind:0 tenv initial_location pair initial_struct_set


  let initial (p : Procdesc.t) (tenv : Tenv.t) : PtsToMap.t =
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
          let gen = populate_locations tenv f in
          map_pairs map gen )
    in
    with_formals


  let get_pointing_to map loc =
    match find_opt loc map with Some set -> set | None -> AbstractLocationSet.empty


  let dereference map curr =
    AbstractLocationSet.fold
      (fun loc s -> AbstractLocationSet.union s (get_pointing_to map loc))
      curr AbstractLocationSet.empty
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
  F.fprintf fmt "SENSITIVE:\n" ;
  MayPointsToMap.pp fmt st.sensitive ;
  F.fprintf fmt "\n\n INSENSITIVE:\n" ;
  MayPointsToMap.pp fmt st.insensitive


let initial pdesc tenv =
  {sensitive= MayPointsToMap.initial pdesc tenv; insensitive= MayPointsToMap.initial pdesc tenv}


let resolve_accesses_to_location_set (al : HilExp.access_expression list) (tenv : Tenv.t) curr map =
  let offset_by_field curr nm tp =
    AbstractLocationSet.map (fun loc -> AbstractLocation.create_loc_for_field_offset loc nm tp) curr
  in
  let rec dereference_all map curr (ls : HilExp.access_expression list) =
    match ls with
    | hd :: tl ->
        let next_set =
          match hd with
          | Dereference _ | ArrayOffset (_, _, _) ->
              MayPointsToMap.dereference map curr
          | FieldOffset (_, nm) -> (
              let typ_opt = HilExp.AccessExpression.get_typ hd tenv in
              match typ_opt with Some typ -> offset_by_field curr nm typ | None -> curr )
          | Base _ | AddressOf _ ->
              curr
        in
        dereference_all map next_set tl
    | [] ->
        curr
  in
  dereference_all map curr al





let get_locations (acc : HilExp.access_expression) (tenv : Tenv.t) (map : MayPointsToMap.t) =
  let base = HilExp.AccessExpression.get_base acc in
  let base_location = AbstractLocation.of_base base in
  let unfolded = LifetimeUtils.unfold_access_exp acc in
  match base_location with
  | Some loc ->
      let singleton_set = AbstractLocationSet.singleton loc in
      Some (resolve_accesses_to_location_set unfolded tenv singleton_set map)
  | None ->
      None


let get_lhs_locations (map : MayPointsToMap.t) (tenv : Tenv.t) (lhs : HilExp.AccessExpression.t) :
    AbstractLocationSet.t option =
  get_locations lhs tenv map





let get_rhs_locations (map : MayPointsToMap.t) (tenv : Tenv.t) (rhs : HilExp.t) :
    AbstractLocationSet.t option =
  let rhs_aexp_opt = LifetimeUtils.find_inner_access_exp rhs in
  match rhs_aexp_opt with Some rexp -> get_locations rexp tenv map | None -> None


let set_pointing_to_sensitive (domain : t) (tenv : Tenv.t) (lhs : HilExp.access_expression)
    (rhs : HilExp.t) =
  let map = domain.sensitive in
  let lhs_pts_to_set = get_lhs_locations map tenv lhs in
  let rhs_pts_to_set = get_rhs_locations map tenv rhs in
  match (lhs_pts_to_set, rhs_pts_to_set) with
  | Some l, Some r ->
      let dereferenced_rhs = MayPointsToMap.dereference map r in
      AbstractLocationSet.fold (fun loc map -> MayPointsToMap.add loc dereferenced_rhs map) l map
  | _ ->
      map


let rec unify_rec ptrs new_ptees map =
  let ptr_chosen_opt = AbstractLocationSet.choose_opt ptrs in
  match ptr_chosen_opt with
  | Some ptr ->
      let curr_ptees = MayPointsToMap.get_pointing_to map ptr in
      let updated_ptees = AbstractLocationSet.union new_ptees curr_ptees in
      let next_ptees = MayPointsToMap.dereference map updated_ptees in
      let updated_map =
        AbstractLocationSet.fold (fun loc map -> MayPointsToMap.add loc updated_ptees map) ptrs map
      in
      unify_rec updated_ptees next_ptees updated_map
  | None ->
      map


let set_pointing_to_insensitive (domain : t) (tenv : Tenv.t) (lhs : HilExp.access_expression)
    (rhs : HilExp.t) =
  let map = domain.insensitive in
  let lhs_pts_to_set = get_lhs_locations map tenv lhs in
  let rhs_pts_to_set = get_rhs_locations map tenv rhs in
  match (lhs_pts_to_set, rhs_pts_to_set) with
  | Some l, Some r ->
      let dereferenced_rhs = MayPointsToMap.dereference map r in
      unify_rec l dereferenced_rhs map
  | _ ->
      map


let set_pointing_to (domain : t) (tenv : Tenv.t) ~(lhs : HilExp.access_expression) ~(rhs : HilExp.t)
    =
  { sensitive= set_pointing_to_sensitive domain tenv lhs rhs
  ; insensitive= set_pointing_to_insensitive domain tenv lhs rhs }


let sensitive (domain : t) = domain.sensitive

let insensitive (domain : t) = domain.insensitive

type summary = t