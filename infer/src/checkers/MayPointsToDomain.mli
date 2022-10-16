(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type indirection = int [@@deriving compare]
type abstract_location = Variable of (string * Typ.t) | Alloc of Typ.t [@@deriving compare]

module AbstractLocation : sig
    include PrettyPrintable.PrintableOrderedType 
end

module AbstractLocationSet : sig
    include AbstractDomain.FiniteSetS
end

include AbstractDomain.S

val initial : Procdesc.t -> t

type summary = t

val set_pointing_to : t -> AbstractLocationSet.t -> AbstractLocationSet.t -> t
val get_lhs_locations : t -> HilExp.AccessExpression.t -> AbstractLocationSet.t
val get_rhs_locations : t -> HilExp.t ->  AbstractLocationSet.t 