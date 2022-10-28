open! IStd
module F = Format
type t =
  | Local of (string * Typ.t)
  | StructMember of (Typ.t * string * Typ.t)
  | NonLocal of (string * Typ.t * AbstractLocation.indirection)
  | Heap
[@@deriving compare, equal]

let rec of_name_and_type ?(ind = 0) (nm : string) (curr_typ : Typ.t) =
  match curr_typ.desc with
  | Tptr (inner, _) ->
      [NonLocal (nm, curr_typ, ind)] @ of_name_and_type ~ind:(ind + 1) nm inner
  | Tarray desc ->
      [NonLocal (nm, curr_typ, ind)] @ of_name_and_type ~ind:(ind + 1) nm desc.elt
  | _ ->
      if ind > 0 then [NonLocal (nm, curr_typ, ind)] else [Local (nm, curr_typ)]

let of_formal (p : Mangled.t * Typ.t * Annot.Item.t) =
  let mname, typ, _ = p in
  let actual_name = Mangled.to_string mname in
  of_name_and_type actual_name typ

let of_base (b : AccessPath.base) : t list =
  let var, typ = b in
  match Var.get_pvar var with
  | Some pv ->
      let nm = Pvar.get_simplified_name pv in
      of_name_and_type nm typ
  | None -> (
    match Var.get_ident var with
    | Some id ->
        let nm = Ident.name_to_string (Ident.get_name id) in
        of_name_and_type nm typ
    | None ->
        [] )

let is_simpler_than _lv1 _lv2 = true


let string_of v =
  match v with
  | Local (nm, _) ->
      "local:" ^ nm
  | NonLocal (nm, _, ind) ->
      nm ^ string_of_int ind
  | Heap ->
      "a"
  | StructMember (ptp, nm, _) ->
      Typ.to_string ptp ^ "." ^ nm

let pp fmt lv = F.fprintf fmt "%s" (string_of lv)

let is_const lv =
  match lv with
  | Local _ ->
      true
  | NonLocal (_, tp, _) ->
      Typ.is_const tp.quals
  | Heap ->
      false
  | StructMember _ ->
      false

let points_to_const lv =
  match lv with
  | Local _ ->
      true
  | NonLocal (_, tp, _) ->
      Typ.is_const_reference tp
  | Heap ->
      false
  | StructMember _ ->
      false

let of_abstract_locations (abs_set : MayPointsToDomain.AbstractLocationSet.t) =
  let list_of_locs = MayPointsToDomain.AbstractLocationSet.to_list abs_set in
  let convert abs =
    let typ = AbstractLocation.type_of abs in
    match abs with
    | AbstractLocation.Field (parent, (nm, tp, ind)) ->
        if phys_equal ind 0 then
          let parent_typ = AbstractLocation.type_of parent in
          StructMember (parent_typ, nm, tp)
        else NonLocal (nm, typ, ind)
    | AbstractLocation.Variable (nm, tp, ind) ->
        if phys_equal ind 0 then Local (nm, tp) else NonLocal (nm, typ, ind)
    | AbstractLocation.HeapMemory _ ->
        Heap
  in
  List.map list_of_locs ~f:convert

let rec get_lifetime_sublist (lifetimes : t list) (access : HilExp.access_expression list) : t list
    =
  match access with
  | hd :: tl -> (
    match hd with
    | Dereference _ | ArrayOffset (_, _, _) ->
        get_lifetime_sublist (List.tl_exn lifetimes) tl
    | FieldOffset (_, _) ->
        lifetimes
    | Base _ | AddressOf _ ->
        lifetimes )
  | [] ->
      lifetimes

let of_aexp (aexp : HilExp.access_expression) : t list =
  let base = HilExp.AccessExpression.get_base aexp in
  let base_lifetimes = of_base base in
  let unfolded = LifetimeUtils.unfold_access_exp aexp in
  match base_lifetimes with [] -> [] | _ -> get_lifetime_sublist base_lifetimes unfolded