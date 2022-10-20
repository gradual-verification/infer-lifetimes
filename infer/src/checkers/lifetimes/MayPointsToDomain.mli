(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module AbstractLocationSet : sig
  include AbstractDomain.FiniteSetS

  val singleton : AbstractLocation.t -> t
  
  val to_list: t -> AbstractLocation.t list
end

module MayPointsToMap : sig
  include AbstractDomain.MapS

  val get_pointing_to: t -> AbstractLocation.t -> AbstractLocationSet.t

  val dereference: t -> AbstractLocationSet.t -> AbstractLocationSet.t
end

include AbstractDomain.S

val sensitive: t -> MayPointsToMap.t

val insensitive: t ->  MayPointsToMap.t

val initial : Procdesc.t -> Tenv.t -> t

val set_pointing_to : t -> Tenv.t -> lhs:HilExp.access_expression -> rhs:HilExp.t -> t

type summary = t