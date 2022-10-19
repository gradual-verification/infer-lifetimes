(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type indirection = int [@@deriving compare]

type varinfo =
  | Named of (string * Typ.t)
  | MaybeUntyped of (string * Typ.t option)
  | Unnamed of (Typ.t * indirection) option
[@@deriving compare]

type abstract_location =
  | Field of (abstract_location option * string)
  | Variable of string
  | HeapMemory of Typ.t option
[@@deriving compare]

module AbstractLocation : sig
  include PrettyPrintable.PrintableOrderedType
end

module AbstractLocationSet : sig
  include AbstractDomain.FiniteSetS
end

module MayPointsToMap : sig
  include AbstractDomain.MapS
end

include AbstractDomain.S

val initial : Procdesc.t -> Tenv.t -> t

val set_pointing_to : t -> lhs:HilExp.access_expression -> rhs:HilExp.t -> t

type summary = t