open! IStd

type indirection = int [@@deriving compare, equal]

type varinfo = string * Typ.t * indirection [@@deriving compare, equal]

type t = Field of (t * varinfo) | Variable of varinfo | HeapMemory of Typ.t [@@deriving compare, equal]

val of_base : AccessPath.base -> t option

val of_var_data : ProcAttributes.var_data -> t

val create_loc_for_field_offset : t -> Fieldname.t -> Typ.t -> t

val create_loc_for_variable : string -> Typ.t -> int -> t

val create_loc_for_formal : Mangled.t * Typ.t * Annot.Item.t -> t

val type_of : t -> Typ.t

val pp : Format.formatter -> t -> unit

