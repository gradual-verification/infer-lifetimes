open! IStd

type t = Local | StructLocal of string | NonLocal of (string * AbstractLocation.indirection * bool) | Heap [@@deriving compare, equal]
    
val variables_of_formal : Mangled.t * Typ.t * Annot.Item.t -> t list

val string_of : t -> string

val of_abstract_locations : MayPointsToDomain.AbstractLocationSet.t -> t list

val is_simpler_than : t -> t -> bool

val is_const : t -> bool