(***************************************************)
(*                                                 *)
(*               Domain Partition                  *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Constraints


(** Signature for a single partition of the domain of a ranking function. *)
module type PARTITION = sig
  module C : CONSTRAINT

  type t
  val constraints : t -> C.t list
  val env : t -> Environment.t
  val vars : t -> var list
  val varsLive : t -> var list

  val bot : Environment.t -> var list -> t
  val inner : Environment.t -> var list -> C.t list -> t
  val top : Environment.t -> var list -> t
  
  val supports_underapproximation: bool 
  val isBot : t -> bool
  val isLeq : t -> t -> bool

  val join : t -> t -> t
  val widen : t -> t -> t
  val meet : t -> t -> t
  val lowerwiden : t -> t -> t

  val fwdAssign : t -> aExp * aExp -> t
  val bwdAssign : t -> aExp * aExp -> t
  val bwdAssign_underapprox : t -> aExp * aExp -> t
  val filter : t -> bExp -> t
  val bwdfilter : t -> bExp -> t
  val bwdfilter_underapprox : t -> bExp -> t
  val bwdfilter_underapproxwhile : t -> t -> bExp -> t -> t

  val print : Format.formatter -> t -> unit
  val to_stringYices : t -> string list
  val to_stringLatte : t -> var list -> (string list * int list * int)

end
