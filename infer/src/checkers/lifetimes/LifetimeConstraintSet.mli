open! IStd

type t

val initial : t

val generate_constraints : Procdesc.t -> MayPointsToDomain.t -> string