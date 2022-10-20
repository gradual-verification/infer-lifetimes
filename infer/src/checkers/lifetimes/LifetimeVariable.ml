open! IStd

type t = Local | StructLocal of string | NonLocal of (string * AbstractLocation.indirection) | Heap [@@deriving compare]

let variables_of_formal (p : Mangled.t * Typ.t * Annot.Item.t) =
  let mname, typ, _ = p in
  let actual_name = Mangled.to_string mname in
  let rec generate_lt_variables ?(ind = 0) root_name curr_typ =
    let stripped = Typ.strip_ptr curr_typ in
    if Typ.equal curr_typ stripped then
      if phys_equal ind 0 then [Local] else [NonLocal (root_name, ind)]
    else [NonLocal (root_name, ind)] @ generate_lt_variables ~ind:(ind + 1) root_name stripped
  in
  generate_lt_variables actual_name typ


let string_of v =
  match v with
  | Local ->
      "local"
  | NonLocal (nm, ind) ->
      nm ^ string_of_int ind
  | Heap ->
      "a"
  | StructLocal nm ->
      nm


let of_abstract_location (abs : AbstractLocation.t) =
  match abs with
  | Field (parent, (nm, _, ind)) ->
      if phys_equal ind 0 then
        let typ = AbstractLocation.type_of parent in
        StructLocal (Typ.to_string typ)
      else NonLocal (nm, ind)
  | Variable (nm, _, ind) ->
      if phys_equal ind 0 then Local else NonLocal (nm, ind)
  | HeapMemory _ ->
      Heap