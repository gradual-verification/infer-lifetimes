open! IStd

type t =
  | Local
  | StructLocal of string
  | NonLocal of (string * AbstractLocation.indirection * bool)
  | Heap
[@@deriving compare, equal]

let variables_of_formal (p : Mangled.t * Typ.t * Annot.Item.t) =
  let mname, typ, _ = p in
  let actual_name = Mangled.to_string mname in
  let rec generate_lt_variables ?(ind = 0) (root_name:string) (curr_typ:Typ.t) =
    let is_const = Typ.is_const curr_typ.quals in
    let stripped = Typ.strip_ptr curr_typ in
    if Typ.equal curr_typ stripped then
      if phys_equal ind 0 then [Local] else [NonLocal (root_name, ind, is_const)]
    else
      [NonLocal (root_name, ind, is_const)]
      @ generate_lt_variables ~ind:(ind + 1) root_name stripped
  in
  generate_lt_variables actual_name typ

let is_simpler_than _lv1 _lv2 = true

let string_of v =
  match v with
  | Local ->
      "local"
  | NonLocal (nm, ind, _) ->
      nm ^ string_of_int ind
  | Heap ->
      "a"
  | StructLocal nm ->
      nm

let is_const lv = match lv with
| Local -> true
| NonLocal(_, _, cs) -> cs
| Heap -> false
| StructLocal(_) -> false

let of_abstract_locations (abs_set : MayPointsToDomain.AbstractLocationSet.t) =
  let list_of_locs = MayPointsToDomain.AbstractLocationSet.to_list abs_set in
  let convert abs =
    let typ = AbstractLocation.type_of abs in
    match abs with
    | AbstractLocation.Field (parent, (nm, _, ind)) ->
        if phys_equal ind 0 then
          let parent_typ = AbstractLocation.type_of parent in
          StructLocal (Typ.to_string parent_typ)
        else
          NonLocal (nm, ind, Typ.is_const typ.quals)
    | AbstractLocation.Variable (nm, _, ind) ->
        if phys_equal ind 0 then Local else NonLocal (nm, ind, Typ.is_const typ.quals)
    | AbstractLocation.HeapMemory _ ->
        Heap
  in
  List.map list_of_locs ~f:convert
