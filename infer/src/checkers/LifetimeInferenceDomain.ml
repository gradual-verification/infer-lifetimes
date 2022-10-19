open! IStd
module F = Format

type indirection = int [@@deriving compare, equal, sexp]

type varinfo =
  | Named of (string * Typ.t)
  | MaybeUntyped of (string * Typ.t option)
  | Unnamed of (Typ.t * indirection) option
[@@deriving compare, equal, sexp]

type lifetime_variable = Variable of varinfo * indirection | Field of (lifetime_variable * varinfo)
[@@deriving compare, equal, sexp]

let string_of_varinfo vi =
  match vi with
  | Named (s, _) ->
      s
  | MaybeUntyped (s, _) ->
      s
  | Unnamed (Some (_, i)) ->
      "a" ^ string_of_int i
  | Unnamed None ->
      "a"

module LTVar = struct
  type t = lifetime_variable [@@deriving compare, equal, sexp]

  let string_of loc:string = 
  let rec format_rec loc : string =
    match loc with
    | Field (p, vi) ->
        format_rec p ^ "." ^ string_of_varinfo vi
    | Variable (vi, id) ->
        "'" ^ string_of_varinfo vi ^ string_of_int id in format_rec loc

  let pp fmt loc = F.fprintf fmt "%s" (string_of loc)

  let _of_var_data (vd : ProcAttributes.var_data) =
    Variable (Named (Mangled.to_string vd.name, vd.typ), 0)

  let _offset_by_field ploc typ fname = Field (ploc, MaybeUntyped (fname, typ))

  let _of_base (b : AccessPath.base) : t option =
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


type constraint_type = Outlives of (LTVar.t * LTVar.t) | Satisfies of (LTVar.t * LTVar.t) [@@deriving compare, equal, sexp]

module Constraint = struct
  type t = constraint_type [@@deriving compare, equal, sexp]
  let pp fmt cs = match cs with 
    | Outlives(c1, c2) -> F.fprintf fmt "%s <: %s" (LTVar.string_of c1) (LTVar.string_of c2)
    | Satisfies(c1, c2) ->  F.fprintf fmt "%s = %s" (LTVar.string_of c1) (LTVar.string_of c2)
end

include AbstractDomain.FiniteSet(Constraint)


let initial _pdesc _tenv = empty

type summary = t