open! IStd
module F = Format

type indirection = int [@@deriving compare, equal]

type varinfo = string * Typ.t * indirection [@@deriving compare, equal]

let string_of_varinfo vi =
  let n, t, i = vi in
  n ^ string_of_int i ^ ":" ^ Typ.to_string t


type t = Field of (t * varinfo) | Variable of varinfo | HeapMemory of Typ.t [@@deriving compare, equal]

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


let type_of loc =
  match loc with Variable (_, tp, _) -> tp | Field (_, (_, tp, _)) -> tp | HeapMemory tp -> tp


let create_loc_for_field_offset ?(ind = 0) loc nm typ = Field (loc, (Fieldname.get_field_name nm, typ, ind))

let create_loc_for_variable ?(ind = 0) nm typ = Variable (nm, typ, ind)

let create_loc_for_formal formal =
  let nm, tp, _ = formal in
  let fname = Mangled.to_string nm in
  create_loc_for_variable fname tp


let of_var_data (vd : ProcAttributes.var_data) =
  create_loc_for_variable (Mangled.to_string vd.name) vd.typ 


let of_base (b : AccessPath.base) : t option =
  let var, typ = b in
  match Var.get_pvar var with
  | Some pv ->
      Some (create_loc_for_variable (Pvar.get_simplified_name pv) typ)
  | None -> (
    match Var.get_ident var with
    | Some id ->
        Some (create_loc_for_variable (Ident.name_to_string (Ident.get_name id)) typ)
    | None ->
        None )
