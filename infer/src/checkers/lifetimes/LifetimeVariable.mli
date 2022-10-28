open! IStd

type t =
  | Local of (string * Typ.t)
  | StructMember of (Typ.t * string * Typ.t)
  | NonLocal of (string * Typ.t * AbstractLocation.indirection)
  | Heap
[@@deriving compare, equal]

val of_formal : Mangled.t * Typ.t * Annot.Item.t -> t list

val of_base : AccessPath.base -> t list

val string_of : t -> string

val pp : Format.formatter -> t -> unit

val of_abstract_locations : MayPointsToDomain.AbstractLocationSet.t -> t list

val of_aexp : HilExp.access_expression -> t list

val is_simpler_than : t -> t -> bool

val is_const : t -> bool

val points_to_const : t -> bool