open! IStd

type t = Local | StructLocal of string | NonLocal of (string * AbstractLocation.indirection) | Heap [@@deriving compare]
    
val variables_of_formal : Mangled.t * Typ.t * Annot.Item.t -> t list

val string_of : t -> string

val of_abstract_location : AbstractLocation.t -> t