open! IStd

type indirection = int [@@deriving compare, equal, sexp]

type varinfo =
  | Named of (string * Typ.t)
  | MaybeUntyped of (string * Typ.t option)
  | Unnamed of (Typ.t * indirection) option
[@@deriving compare, equal, sexp]

type lifetime_variable = Variable of varinfo * indirection | Field of (lifetime_variable * varinfo)
[@@deriving compare, equal, sexp]

module LTVar : sig
    include PrettyPrintable.PrintableOrderedType
end

type constraint_type = Outlives of (LTVar.t * LTVar.t) | Satisfies of (LTVar.t * LTVar.t) [@@deriving compare, equal, sexp]
module Constraint : sig
    include PrettyPrintable.PrintableOrderedType
end

include AbstractDomain.FiniteSetS

val initial: Procdesc.t -> Tenv.t -> t

type summary = t

