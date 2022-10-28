open! IStd


module Env : sig
    type t
    val initial : t
    
    val constrain: lhs:HilExp.access_expression -> rhs:HilExp.t -> constr:t -> unit

    val pp : Format.formatter -> t -> unit

end
